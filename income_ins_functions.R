library(tidyverse)
library(data.table)

ff_clean_ces <- function(data_file, header_file) {
  
  ##########################################################################################
  #
  #  This function cleans the following CES data:
  #     health insurance, transportation, appareal and services, housekeeping supplies
  #     personal care products and services, reading, misc
  #
  #    Input:
  #     data_file: txt file containing data of the individual expense
  #     header_file: txt data of header information (different file for each expenses)
  #
  # The function adjust household expenditures to the South region by taking the expense
  # ratio of the south to national averages, and then multiplying the expense by household
  # size by the ratio
  ###########################################################################################
  
  
  # import data
  df <- read_delim(data_file,
                   delim = '\t') %>%
    # convert from wide (each column is a different year) 
    # to long (each row is a different year and expense)
    pivot_longer(cols = -`Series ID`,
                 names_to = 'Year', values_to = 'Expense') %>%
    # remove word 'Annual' from year column
    mutate(Year = str_replace_all(Year, 'Annual ', '')) %>%
    # trim whitespace in all columns
    # need to trim so values in this column can match values in dataframe containing estimates
    mutate_all(funs(str_trim(., side = 'both'))) %>%
    # convert years and expense estimtae to numeric
    mutate_at(c('Expense', 'Year'), as.numeric)
  
  # import headers
  df_headers <- read_delim(header_file,
                           delim = '\t',
                           col_names = FALSE) %>%
    # only keep headers
    filter(!str_detect(X1, '^CXU'),
           # remove rows that jsut say 'Series ID'
           X1 != 'Series Id') %>%
    # separate columsn at ':', this puts header title and header value in different columns
    separate(X1, into = c('titles', 'values'), sep = ': ', extra = 'merge', remove = TRUE) %>%
    drop_na() %>%
    # add column that adds an ID number to each series group
    # needed to spread columns
    mutate(id = rep(1:(nrow(.)/6), each = 6),
           values = str_trim(values)) %>%
    # convert from long to wide where each header title is in a different row
    pivot_wider(id_cols = 'id', names_from = 'titles', values_from = 'values') %>%
    # remove unneeded values
    select(-id, -Category) %>%
    # rename to match column name in dataframe containing data
    # we will merge on this column, so names need to be the same
    rename(`Series ID` = `Series Id`) %>%
    # trim whitespace in all columns
    # need to trim so values in this column can match values in dataframe containing estimates
    mutate_all(funs(str_trim(., side = 'both')))
  
  # combine header descriptions to dataframe containing expenses dat
  df <- left_join(df, df_headers, by = 'Series ID')
  
  ### adjust expenses of number of people in house by region (south)
  
  ###################################################################################################
  # The data shows average national expenses by type, averge region expenses, 
  # and average expenses by number of poepl in household.
  # But, there is no data for expenses by average number of people in household and region.
  # Therefore, we will adjust household expenses by the ration of the south to the national average
  # in each expense category
  ##################################################################################################
  
  # create ratio of south to the rest of the United States by splitting entire US and South into different datasets,
  # then merging; this puts each region's estimate on the same row
  
  us <- df %>%
    filter(Characteristics == 'All Consumer Units') %>%
    select(Item, Year, Expense)
  
  south <- df %>%
    filter(Characteristics == 'Region of residence: south') %>%
    select(Item, Year, Expense)
  
  # merge (must join by item because series ID is different depending on region and num of people in houshold)
  ratio <- left_join(us, south, by = c('Item', 'Year')) %>%
    # calculate ratio by dividing South estimate by US estimate
    mutate(region_ratio = Expense.y / Expense.x) %>%
    # drop column not needed to join ratio to dataset containing estimate
    select(Item, Year, region_ratio)
  
  # remove region rows from dataset contining expenses, because we only needed
  # regions to calculate ratio
  df <- df %>%
    filter(!(Characteristics == 'All Consumer Units' |
               Characteristics == 'Region of residence: south')) %>%
    # add region adjusted ratios
    left_join(ratio, by = c('Item', 'Year')) %>%
    # multiply expense by region ratio, round to nearest whole number
    mutate(Expense = round( Expense*region_ratio, 0 ))
  
  return(df)
}

create_economic_units <- function(file_path, state, year) {
  
  # this function imports population level PUMA data and creates economic units
  
  ##### calculate economic unit labels ###############
  
  # An economic unit is the reference person and everyone with a relationship to the reference person.
  # This includes spouses, partners, and any children in the house (whether related or not)
  # All people in the household outside this group are assigned their own ecenomic unit
  
  # economic units are derived from the relationship variable
  # these variables changed coding in 2008;
  # therefore we need two objects representing these variables, depending on year
  econonmic_unit_a <- c(0,1,2,3,4,5,6,7,10,11)
  econonmic_unit_b <- c(0,1,2,3,4,5,6,7,8,9,10,13,14,15)
  
  economic_unit_vec <- if (year < 2008) econonmic_unit_a else econonmic_unit_b
  
  ###############################################
  
  # since estimated expenses are at the county level, we need to add counties
  counties <- read_csv('puma_counties.csv')
  
  # select PUMA area code based on year
  if (!!year < 2012) {
    
    counties <- counties %>%
      select(puma2k, cntyname) %>%
      rename(PUMA = puma2k)
    
  } else {
    
    counties <- counties %>%
      select(puma12, cntyname) %>%
      rename(PUMA = puma12)
     
  }
  
  # remove duplicate PUMAs
  # this will remove some counties since counties can span more than one PUMA
  counties <- counties %>%
    distinct(PUMA, .keep_all = TRUE) %>%
    # remove ' NC' from county name
    mutate(cntyname = str_replace_all(cntyname, ' NC', ''))
  
  pop_vars <- c('SERIALNO', # serial number grouped by household 
                'SPORDER', # order of person in household
                'ST', # state (needed to calculate state income taxes)
                'PUMA', # Public use micro area
                'year',
                # relationship to reference person; variable name changed in 2010
                ifelse(year < 2010, 'REL', 'RELP'),
                'PINCP', # total personal income
                'HISP', # hispanic origin
                'RAC1P', # race
                'AGEP', # age
                'SEX', # sex
                'ESR' # employment status
  )

  # import population data
  pop <- vroom(file_path,
                  col_types = cols(RT= col_character(),
                                   NAICSP = col_character(),
                                   SOCP = col_character(),
                                   SERIALNO = col_character(),
                                   .default = col_double())) %>%
    mutate(year = !!year,
           PUMA = as.numeric(PUMA)) %>%
    select(!!pop_vars) %>%
    # filter for state and PUMA
    filter(ST == !!state) %>%
    # add county names
    left_join(counties, by = 'PUMA') #%>%
    # filter for needed counties
    #filter(cntyname %in% c('Forsyth', 'Guilford', 'Durham'))

  # change REL column name to RELP if REL is a column (less than 2010)
  # this allows us to use the same column names for all years
  pop <- if ('REL' %in% colnames(pop)) rename(pop, RELP = REL) else pop

  pop <- pop %>%
    # replace NA values for income with zero
    mutate(PINCP = replace_na(PINCP, 0),
           # ESR represents employment, and is used to determine whether person is present to watch kid; 
           # make NA's 0
           ESR = replace_na(ESR, 0),
           # convert employment status to boolean: TRUE = employed and FALSE == not working
           # 3 and 6 represent not working
           # use the following critera:
           # if reference person or spouse is not working then status is FALSE
           # if other person in economic unit is not working than status is FALSE only if person is 21 or over
           ESR = ifelse(ESR %in% c(3, 6) & RELP %in% c(0,1), FALSE, 
                        ifelse(ESR %in% c(3, 6) & AGEP >= 21, FALSE, TRUE)),
           # create boolean signifying if person is in economic unit
           economic_unit = ifelse(RELP %in% !!economic_unit_vec, TRUE, FALSE)) %>%
    mutate(SERIALNO = str_remove(SERIALNO, "[A-Z]{2}"),
           SERIALNO = str_remove(SERIALNO, "^[0-9]{4}"),
           SERIALNO = as.integer(SERIALNO))

  return(pop)
  
}

tax_liability <- function(pop, current_year) {

  # This function calculates income for each economic unit
  # it incorporates, and subtracts, taxes
  
  print('taxes')
  
  tax_liabilities <- read_csv(glue('update_taxes/nc_tax_liab_ind-{current_year}.csv'))
  
  # merge taxes with population dataset
  pop <- pop %>%
    left_join(tax_liabilities, by = c('year', 'SERIALNO', 'SPORDER')) %>%
    # replace NA for taxes with zero
    replace_na(list(total_taxes = 0)) %>%
    # subtract taxes from income
    # create group to calculate economic unit post-tax income
    group_by(year, SERIALNO, economic_unit) %>%
    # if part of economic unit, add incomes and taxes, otherwise use individual's income
    mutate(economic_unit_income = ifelse(economic_unit == TRUE,
                                         sum(PINCP), PINCP),
          economic_unit_taxes = ifelse(economic_unit == TRUE,
                                       sum(total_taxes), total_taxes)) %>%
    ungroup() %>%
    select(-PINCP, -total_taxes)
  
  return(pop)

}

rent <- function(pop) {
  
  ###############################################################
  # Calculate fair market rent per household 
  # based on the total number of bedrooms needed in house
  #
  # Reference person and spouse get one bedroom
  # Every other adult gets their own bedroom
  # Up to two children (under 18) can share a room
  # 4 is the max number of bedrooms
  #
  # rent between household and economic unit is shared
  # based on the proportion of rooms each economic unit needs
  #############################################################
  
  print('rent')
  
  # import rent values
  fmr <- read_csv('data/cleaned_data/fmr.csv') %>%
    # convert to long form where each row is a different year, county and bedroom number
    # this will allow for easy merge with PUMA dataset based on number of bedrooms
    gather(house_bedrooms, rent, -year, -countyname) %>%
    # remove 'fmr' from column showing number of bedrooms
    # needed so we can merge on number of bedrooms
    mutate(house_bedrooms = as.integer(str_replace_all(house_bedrooms, 'fmr', '')))
  
  # calculate numer of bedrooms each person needs
  pop <- pop %>%
    mutate(bedrooms = ifelse(
      # since children share rooms, they need 0.5
      AGEP < 18, 0.5,
      # spouse shares bedroom, so he/she does not have one
      ifelse(RELP == 1, 0,
             # all other adults get one
             1)
    )) %>%
    # calculate number of bedrooms in household by grouping by bedrooms
    # and summing total number of individual bedrooms
    group_by(SERIALNO) %>%
    # use ceiling to round up
    mutate(house_bedrooms = ceiling(sum(bedrooms)),
           # calculate each person's proportion of rent
           rent_prop = bedrooms / house_bedrooms,
           # cannot have more than 4 bedrooms
           house_bedrooms = ifelse(house_bedrooms > 4, 4, house_bedrooms)) %>%
    # add rent amounts
    left_join(fmr, by = c('year', 'cntyname' = 'countyname', 'house_bedrooms')) %>%
    # calculate each individual's proportion of rent
    mutate(ind_rent = rent * rent_prop) %>%
    # calculate each economic unit's proportion of rent
    group_by(year, SERIALNO, economic_unit) %>%
    mutate(economic_unit_rent = ifelse(economic_unit == TRUE,
                                        sum(ind_rent),
                                        ind_rent),
           # multiply rent by 12 to get yearly amount
           economic_unit_rent = as.integer(economic_unit_rent * 12)) %>%
    select(-bedrooms:-ind_rent) %>%
    ungroup()
}

food <- function(pop) {
  
  print('food')
  
  # import and clean food costs dataset
  food <- read_csv('data/cleaned_data/food.csv') %>%
    # Convert sex to 1 for male, 2 for female, and 0 for neither
    # This matches population dataset
    mutate(gender = ifelse(gender == 'N', 0,
                           ifelse(gender == 'M', 1, 2))) %>%
    select(-age_group) %>%
    rename(food_costs = estimate)
  
  # create a new object that age breaks for the cut function
  age_breaks <- food %>%
    select(start_age, end_age) %>%
    distinct() %>%
    .[[1]] %>%
    append(., 150)
  
  # create labels for bins; these are the start ages
  age_labels <- age_breaks[-(length(age_breaks))]
  
  pop <- pop %>%
    # need to convert the gender of people 11 and under to 0,
    # because food costs do not differ by gender at these ages
    mutate(gender = ifelse(AGEP < 12, 0, SEX)) %>%
    # create age labels based on food costs age bins
    mutate(start_age = cut(AGEP, breaks = !!age_breaks, 
                           labels = !!age_labels, 
                           include.lowest = TRUE, right = FALSE),
           # convert to integer so it can be merged with population dataset
           start_age = as.integer(as.character(start_age))) %>%
    # merge in food costs, based on year, sex, and age
    left_join(food, by = c('year', 'start_age', 'gender')) %>%
    select(-gender:-end_age) %>%
    # group by economic unit and sum across units
    group_by(year, SERIALNO, economic_unit) %>%
    mutate(economic_unit_food = ifelse(economic_unit == TRUE,
                                       sum(food_costs),
                                       food_costs)) %>%
    select(-food_costs) %>%
    ungroup()
  
  return(pop)
    
}

child_care <- function(pop) {
  
  print('child care')
  
  child_costs <- read_csv('data/cleaned_data/child_care_nc_average.csv') %>%
    # rename child care costs column to ensure it does not conflict with the name
    # of another column
    rename(child_care_costs = estimate)

  # create age breaks for differing costs of childcare
  age_breaks <- child_costs %>%
    select(start_age) %>%
    distinct() %>%
    .[[1]] %>%
    append(., 13) %>%
    append(., 150)

  # create labels for bins; these are the start ages
  age_labels <- age_breaks[-(length(age_breaks))]

  pop <- pop %>%
    # create age labels based on child care costs age bins
    mutate(start_age = cut(AGEP, breaks = !!age_breaks,
                           labels = !!age_labels,
                           include.lowest = TRUE, right = FALSE),
           # convert to integer so it can be merged with population dataset
           start_age = as.integer(as.character(start_age))) %>%
    # merge in child care costs, based on year and age
    left_join(child_costs, by = c('year', 'start_age')) %>%
    # grop state column
    select(-state) %>%
    # group by economic unit and sum across units
    group_by(year, SERIALNO, economic_unit) %>%
    # NA values represent no child care costs
    mutate(child_care_costs = replace_na(child_care_costs, 0),
           # ESR represents employment, and is used to determine whether person is persent
           # if there is someone in economic not working make child care costs 0
           # this is accomplished by multiply child care costs by zero if someone in the unit is not working
           # since ESR is boolean, min(ESR) will be 0 (FALSE) if an adult is not working
           # only multi-person economic units can have child care costs; single person economic units cannot
           economic_unit_child_care = ifelse(economic_unit == TRUE, sum(child_care_costs, na.rm = TRUE) * min(ESR),
                                             0)) %>%
    select(-start_age, -child_care_costs) %>%
    ungroup()
  ## Old child care function
  #
  # child_costs <- readRDS('data/cleaned_data/child_care.Rda') %>%
  #   # rename child care costs column to ensure it does not conflict with the name
  #   # of another column
  #   rename(child_care_costs = estimate)
  # 
  # # create age breaks for differing costs of childcare
  # age_breaks <- child_costs %>%
  #   select(start_age) %>%
  #   distinct() %>%
  #   .[[1]] %>%
  #   append(., 13) %>%
  #   append(., 150)
  # 
  # # create labels for bins; these are the start ages
  # age_labels <- age_breaks[-(length(age_breaks))]
  # 
  # pop <- pop %>% 
  #   # create age labels based on food costs age bins 1417191
  #   mutate(start_age = cut(AGEP, breaks = !!age_breaks, 
  #                          labels = !!age_labels, 
  #                          include.lowest = TRUE, right = FALSE),
  #          # convert to integer so it can be merged with population dataset
  #          start_age = as.integer(as.character(start_age))) %>%
  #   # merge in child care costs, based on year, age, and county
  #   left_join(child_costs, by = c('year', c('cntyname' = 'county'), 'start_age')) %>%
  #   # group by economic unit and sum across units
  #   group_by(year, SERIALNO, economic_unit) %>%
  #   # NA values represent no child care costs
  #   mutate(child_care_costs = replace_na(child_care_costs, 0),
  #          # ESR represents employment, and is used to determine whether person is persent
  #          # if there is someone in economic not working make child care costs 0
  #          # this is accomplished by multiply child care costs by zero if someone in the unit is not working
  #          # since ESR is boolean, min(ESR) will be 0 (FALSE) if an adult is not working
  #          # only multi-person economic units can have child care costs; single person economic units cannot
  #          economic_unit_child_care = ifelse(economic_unit == TRUE, sum(child_care_costs, na.rm = TRUE) * min(ESR),
  #                                            0)) %>%
  #   select(-start_age, -child_care_costs) %>%
  #   ungroup()

  return(pop)
  
}


ces <- function(pop) {
  
  # This function adds CES data, which includes adding two additional columns:
  #   1. All consumer expenditure survey expenses minus health insurance,
  #   2. Health insurance
  #

  print('ces')
  
  ces <- read_csv('data/cleaned_data/ces.csv')

  # non-health insurance items can be grouped together and summed based on year and consumer unit size
  # some of these items also divide by age and gender, but we do not need to account for these since
  # the estimate does not take into account whether the household has a person
  # of the given age or gender
  
  ces_group <- ces %>%
    filter(# remove healthcare because we will calculate this later
      item != 'Health insurance') %>%
    # group by year, consumer unit size, category, and sum
    group_by(year, consumer_unit_size, subcategory) %>%
    summarize(economic_unit_ces = sum(expense)) %>%
    # convert to wide form where each expense is in a different column
    # this is needed so that we can merge all expenses separately wit hteh population dataset
    spread(subcategory, economic_unit_ces) %>%
    ungroup()
  
  # change colun names since they are multiword
  colnames(ces_group) <- c('year', 'consumer_unit_size', 'ces_apparel', 'ces_housing_supplies',  
                           'ces_misc', 'ces_personal_care', 'ces_reading', 'ces_trans')
  
  # combine all non-transportation CES expenses into one column
  ces_group <- ces_group %>%
    mutate(ces_other = ces_apparel + ces_housing_supplies + ces_misc + ces_personal_care + ces_reading) %>%
    select(-ces_apparel:-ces_reading)
    
  # Health insurance is calculated as follows:ces_apparel + ces_housing_supplies + ces_misc + ces_personal_care + ces_reading
  # First, all economic unit members under 65 are grouped into one health insurance
  # unit and their health insurance costs are summed.
  # Then, health insurance costs for those over 65 are calculated using expenses by
  # age for single person units.  
  
  ces_health <- ces %>%
    filter(item == 'Health insurance') %>%
    # ages at 150 are really for those 0-64, since 65 + are the rows with listed ages
    mutate(start_age = ifelse(start_age == 150, 0, start_age)) %>%
    select(year, consumer_unit_size, expense, start_age)
  
  
  pop <- pop %>%
    # ces expenses are divided by consumer unit size, which is the same as economic unit size;
    # therefore, calculate economic unit size of population data
    group_by(year, SERIALNO, economic_unit) %>%
    # economic unit size is the total number of people in the unit if economic_unit is true,
    # otherwise, it is just one
    mutate(economic_unit_size = ifelse(economic_unit == TRUE, 
                                       sum(economic_unit), 1)) %>%
    ungroup() %>%
    # calculate health insurance unit size
    # create variable signifying whether over 65, will be used to 
    # create group of total number of economic unit members under 65
    mutate(under_65 = ifelse(AGEP < 65, TRUE, FALSE)) %>%
    # this group signifies health units
    group_by(year, SERIALNO, economic_unit_size, under_65) %>%
    # calculate number of people in unit
    # all persons 65 and over are in their own unit
    mutate(health_unit = ifelse(economic_unit == TRUE & under_65 == TRUE, 
                                sum(economic_unit), 1),
           # five is the highest number of units for economic and health units
           health_unit = ifelse(health_unit > 5, 5, health_unit)) %>%
    ungroup() %>%
    mutate(economic_unit_size = ifelse(economic_unit_size > 5, 5, economic_unit_size)) %>%
    # to match ages with health plans, change age to be either 0, 65, or 75
    mutate(age_health = ifelse(AGEP < 65, 0,
                               ifelse(AGEP < 75, 65, 75))) %>%
    # join health insurance costs by year and age
    left_join(ces_health, by = c('year', c('health_unit' = 'consumer_unit_size'), 
                                 c('age_health' = 'start_age'))) %>%
    # for economic units with no one under 65, expense equals economic unit expense,
    # but for those with people 65 and over, their expenses are separate;
    # therefore, 65 and older expenses need to be combined with the rest of the economic unit
    group_by(year, SERIALNO, economic_unit, under_65) %>%
    # sum together expenses of all people in economic unit 65 and over
    mutate(expense = ifelse(under_65 == FALSE, sum(expense), expense)) %>%
    # add number of people in each group, to be used in creating pro rata expense
    # pro rata expenses will then be summed by economic unit to create economic unit expenses
    add_tally() %>%
    # create pro rate expenses by dividing the number of people in health unit by expenses
    mutate(expense = expense / n) %>%
    # change group to economic unit and sum pro rata expenses
    group_by(year, SERIALNO, economic_unit) %>% 
    mutate(expense = sum(expense)) %>%
    rename(economic_unit_health_ins = expense) %>%
    ungroup() %>%
    # add on ces expenses
    left_join(ces_group, by = c('year', c('economic_unit_size' = 'consumer_unit_size'))) %>%
    select(-economic_unit_size:-age_health, -n)
  
  return(pop)
  
}

meps <- function(pop) {
  
  print('meps')
  
  meps <- read_csv('data/cleaned_data/meps.csv') %>%
    select(-item)
  
  # create age breaks for differing costs of childcare
  meps_breaks <- meps %>%
    select(start_age) %>%
    distinct() %>%
    .[[1]] %>%
    append(., 150)
  
  # create labels for bins; these are the start ages
  meps_labels <- meps_breaks[-(length(meps_breaks))]
  
  pop <- pop %>% 
    # create age labels based on costs age bins
    mutate(start_age = cut(AGEP, breaks = !!meps_breaks, 
                           labels = !!meps_labels, 
                           include.lowest = TRUE, right = FALSE),
           # convert to integer so it can be merged with population dataset
           start_age = as.integer(as.character(start_age))) %>%
    # merge in costs, based on year, and age
    left_join(meps, by = c('year', 'start_age')) %>%
    # group by economic unit and sum across units
    group_by(year, SERIALNO, economic_unit) %>%
    # missing values represent no costs
    mutate(expense = replace_na(expense, 0),
           expense = ifelse(economic_unit == TRUE,
                            sum(expense, na.rm = TRUE),
                            expense)) %>%
    select(-start_age) %>%
    rename(economic_unit_meps = expense) %>%
    ungroup()
  
  return(pop)
  
}

income_ins <- function(df, geo_area, col, demo = TRUE) {
  
  # this function calculates income insufficiency for a given demographic
  # input is a data.table that has already been extended based on replicate weights
  # geo_area: either 'PUMA', 'cntyname', or 'ST'
  # col is a column name that is the grouping variable, as a string
  # demo is whether we are calculating income insufficiency by demographic
  
  if (demo == TRUE) {
    
    df <- df[, .N, by = .(get(geo_area), get(col), income_insufficient)]
    
    # change column names
    setnames(df, c('get', 'get.1'), c('geo_area', 'sub_demographic'))
    
    # convert from long to wide; needed to calculate percentage
    df <- dcast(df, geo_area + sub_demographic ~ income_insufficient, value.var = "N")
    
  } else if (demo == FALSE) {
    
    df <- df[, .N, by = .(get(geo_area), income_insufficient)]
    
    # change column names
    setnames(df, 'get', 'geo_area')
    
    # convert from long to wide; needed to calculate percentage
    df <- dcast(df, geo_area ~ income_insufficient, value.var = "N")
    
    # add column for subdemographic, so it matches structure of demographic datasets
    df <- df[, sub_demographic := 0]
    
  } else {
    
    stop("demo must be TRUE of FALSE.")
    
  }
  
  # replace NA values with zero
  df <- df[is.na(`TRUE`), `TRUE`:=0][
    is.na(`FALSE`), `FALSE`:=0][
      # calculate income insufficiency for each group
      , income_ins := `TRUE` / (`TRUE` + `FALSE`)][
        # add demographic category to dataset
        , demographic := ..col][
          # variables representing number of true and false no longer needed
          , c('TRUE', 'FALSE') := NULL]
  
  # return as data.frame
  df <- as.data.frame(df)
  
  return(df)
  
}

replicate_weights <- function(pop, weights_tbl, weight) {
  
  # This function takes a population data.table as input and extends the data.table 
  # based on weights for one weight column
  # Input:
  #   pop: a data.table that we want extended based on replicate weights
  #       table must include SERIALNO and SPORDER
  #   weights_tbl: a table of replicate weights and SERIALNO and SPORDER
  #                the table should not be collected into RAM
  #   wgt: the replicate weight column used to extend data.table
  
  # bring into memory weights dataframe with serialno, sporder and just one weight column
  
  wgt <- weights_tbl %>%
    select(SERIALNO, SPORDER, !!weight) %>%
    # convert to data table
    as.data.table()
  
  # merge weights with population data.table
  pop_wgt <- merge(pop, wgt, by = c('SERIALNO', 'SPORDER'))
  
  # change name of column so that it is the same of each iteration
  setnames(pop_wgt, weight, 'wgt')
  
  pop_wgt <- pop_wgt[
    # filter out replicate weight values less than 1
    wgt > 0][
      # add additional rows based on the number of replicate weights
      rep(seq(.N), wgt), !"wgt"]
  
  return(pop_wgt)
  
}


standard_errors <- function(pop, geo_area, weights_tbl, pop_weights, col, demo = TRUE) {
  
  # This function calculates standard errors and also aggregates other functions,
  # such as creating extended replicate weight dataset and calculating income insufficiency
  # Input:
  #   pop: data.table of population, not extended with replciate weights
  #   geo_area: 'PUMA', 'cntyname', or 'ST'
  #   weights_tbl: table of weights, uncollected
  #   pop_weights: vector of all weight column names
  #   col: demographic to use when calculating income insufficiency
  #   demo: boolean, whether calcualtion is for demographic variable
  
  for (weight in pop_weights) {
    
    pop_wgt <- replicate_weights(pop, weights_tbl, weight)
    
    # if it is the first weight column, then this is the point estimate
    # separate this estimate in its own object
    if (weight == 'PWGTP') {
      
      point_estimate <- income_ins(pop_wgt, geo_area, col, demo)
      
    } else {
      
      # for the replicate weights we need to calculate the point estimate
      # and then the squared difference between the replicate weight
      # point estiamte and the primary weight point estimate
      # these will later be summed and squared to create the SE
      
      # create point estimate for replicate weights
      single_sq_diff <- income_ins(pop_wgt, geo_area, col, demo) %>%
        # merge with point estimates from primary weights
        left_join(point_estimate, by = c('geo_area', 'sub_demographic', 'demographic')) %>%
        # calculate squared difference
        mutate(sq_diff = (income_ins.x - income_ins.y)^2) %>%
        select(-income_ins.x, -income_ins.y)
      
      # if this is the first replicate weight, create dataframe to store squared differences
      if (str_detect(weight, '.*[Pp]1$')) {
        
        sq_diff <- single_sq_diff
        
      } else {
        
        # if it isn't the first, add squared differences to dataframe containing them
        sq_diff <- sq_diff %>%
          left_join(single_sq_diff, by = c('geo_area', 'sub_demographic', 'demographic'))
        
      }
      
    }
    
  }
  
  # calculate sum of squared difference
  sum_sq_diff <- sq_diff %>%
    select(starts_with("sq_diff")) %>%
    rowSums()
  
  # add sun of squared differences to squared differences dataset;
  # and delete the squared differences
  # we're adding it to the squared differences dataset because this dataset has
  # geographic and demographic information that can be used to merge with point estimates
  sq_diff <- sq_diff %>%
    # remove all sq difference columns
    select(-starts_with("sq_diff")) %>%
    # add sum of squared differences
    mutate(sum_sq_diff = !!sum_sq_diff,
           # square summed differences
           sum_sq_diff = sqrt( sum_sq_diff * (4/80) )) %>%
    rename(se = sum_sq_diff) %>%
    # merge with point estimates
    inner_join(point_estimate, ., by = c('geo_area', 'sub_demographic', 'demographic'))
  
  return(sq_diff)
  
}
