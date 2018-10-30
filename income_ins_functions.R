library(tidyverse)
library(data.table)

ff_clean_ces <- function(data_file, header_file) {
  
  ##########################################################################################
  #
  #  This function cleans the following CES data:
  #     health insurance, transportation, appareal and services, housekeeping supplies
  #     personal care products and services, reading, misc
  #
  #  Input:
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
    gather("Year", 'Expense', `Annual 2006`:`Annual 2017`) %>%
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
    # add column that adds an ID number to each series group
    # needed to spread columns
    mutate(id = rep(1:(nrow(.)/6), each = 6)) %>%
    # convert from long to wide where each header title is in a different row
    spread(titles, values) %>%
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

ff_inflation_adj_table <- function(year_adjust) {
  
  # This function takes as input a base year to adjust for inflation.
  # This base year will be 1.
  #
  # The function outputs a table with years and the adjustment factor.
  
  library(xts)
  library(lubridate)
  
  # import CPI All items index data
  monthly_cpi <- read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
                            skip = 53, header = TRUE)
  
  # extract year and place in its own column
  monthly_cpi$year <- year(monthly_cpi$DATE)
  
  # calculate mean CPI for the year
  yearly_cpi <- monthly_cpi %>% 
    group_by(year) %>% 
    summarize(cpi = mean(VALUE))
  
  # calculate inflation rate compared to adjustment year
  yearly_cpi$adj_factor <- yearly_cpi$cpi[yearly_cpi$year == year_adjust]/yearly_cpi$cpi
  
  return(yearly_cpi)
}

create_economic_units <- function(con, year, state) {
  
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
  
  economic_unit_vec <- if (year < 2008) economic_unit_a else econonmic_unit_b
  
  ###############################################
  
  # since estimated expenses are at the county level, we need to add counties
  counties <- read_csv('puma_counties.csv')
  
  # select PUMA area code based on year
  if (!!year < 2011) {
    
    counties <- counties %>%
      select(puma2k, cntyname) %>%
      rename(PUMA = puma2K)
    
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
  
  # create table name by extracting last two digits from year
  # and placing after 'p_'
  yr <-str_extract(as.character(year), '[0-9][0-9]$')
  table_name <- paste0('p_', yr)
  
  # connect to database table
  population <- tbl(con, table_name)
  
  pop_vars <- c('SERIALNO', # serial number grouped by household 
                'SPORDER', # order of person in household
                'ST', # state (needed to calculate state income taxes)
                'PUMA', # Public use micro area
                'year',
                # relationship to reference person; variable name changed in 2010
                ifelse(year < 2010, 'REL', 'RELP'),
                'PINCP', # total personal income
                'AGEP', #age
                'ESR' # employment status
  )
  
  # import population data
  pop <- population %>%
    select(!!pop_vars) %>%
    # need to collect now because cannot transform NA without collecting
    filter(ST == !!state,
           PUMA %in% c(1802)) %>%
    collect() %>%
    # add county names
    left_join(counties, by = 'PUMA')
  
  # change REL column name to RELP if REL is a column (less than 2010)
  # this allows us to use the same column names for all years
  pop <- if ('REL' %in% colnames(pop)) rename(pop, RELP = REL) else pop
  
  pop <- pop %>%
    # replace NA values for income with zero
    mutate(PINCP = replace_na(PINCP, 0),
           # create boolean of true or false based on whether person 
           # is in economic unit with reference person
           economic_unit = ifelse(RELP %in% !!economic_unit_vec, TRUE, FALSE),
           # if the year is 2017, remove the 2017 from the start of the SERIALNO
           SERIALNO = if (!!year == 2017) as.integer(str_replace_all(.$SERIALNO, '^2017', '')) else .$SERIALNO)
  
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
      # spuse shares bedroom, so he/she does not have one
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
    group_by(SERIALNO, economic_unit) %>%
    mutate(ecnonomic_unit_rent = ifelse(economic_unit == TRUE,
                                        sum(ind_rent),
                                        ind_rent)) %>%
    select(-bedrooms:-ind_rent) %>%
    ungroup()
}
  