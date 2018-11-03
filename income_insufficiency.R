library(tidyverse)
library(data.table)
library(DBI)

source('income_ins_functions.R')

con <- dbConnect(RSQLite::SQLite(), "pums_db.db")

# import needed PUMA data for all years
pop <- data.frame()

for (yr in seq(2012, 2017)) {
  
  print(yr)
  pop <- create_economic_units(con, yr, 37) %>%
    bind_rows(pop, .)
  
}

pop <- pop %>%
  post_tax_income() %>%
  rent() %>%
  food() %>%
  child_care() %>%
  ces() %>%
  meps() %>%
  # calculate income insufficiency
  mutate(income_insufficient = economic_unit_income - rowSums(.[,15:20], na.rm = TRUE),
         # true if income insufficienct false if income sufficient
         income_insufficient = ifelse(income_insufficient < 0, TRUE, FALSE)) %>%
  select(SERIALNO, SPORDER, year, PUMA, cntyname, AGEP, SEX, RAC1P, HISP, economic_unit, income_insufficient)

# save intermediate output
#saveRDS(pop, 'population_expense.Rda')



###################################################

income_ins <- function(df, col, demo = TRUE) {
  
  # this function calculates income insufficiency for a given demographic
  # input is a data.table that has already been extended based on replicate weights
  # col is a column name that is the grouping variable, as a string
  
  if (demo == TRUE) {
    
    df <- df[, .N, by = .(PUMA, get(col), income_insufficient)]
    # convert from long to wide; needed to calculate percentage
    df <- dcast(dfA, PUMA + get ~ income_insufficient, value.var = "N")
    
  } else if (demo == FALSE) {
    
    df <- df[, .N, by = .(PUMA, income_insufficient)]
    # convert from long to wide; needed to calculate percentage
    df <- dcast(df, PUMA ~ income_insufficient, value.var = "N")
    
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
col <- 'RAC1P'
dfd <- income_ins(df, 'total', FALSE)


# create age categories to be used when calculating income insufficiency rates for age groups
age_bins <- c(0, 17, 24, 44, 64, 150)
# create labels that are the end age
age_labels <- age_bins[-1]


# read dataset with expenses
pop <- readRDS('population_expense.Rda') %>%
  as.data.table()

year <- 2017

# data.tables year for filtering
year_dt <- year
state <- 37

pop <- pop[year == year_dt]

# replciate weight variable names are lower case until 2017 and upper case starting in 2017
weight_names <- ifelse(year >= 2017, 'PWGTP', 'pwgtp')
# replicate weight variables
pop_weights <- c('PWGTP', paste0(weight_names, seq(1, 80)))

# create housing table name based on year
tbl_name <- as.character(year) %>%
  str_extract(., '[0-9][0-9]$') %>%
  paste0('p_', .)

# create connection with housing replicate weights
weights_tbl <- tbl(con, tbl_name) %>%
  filter(ST == !!state,
         PUMA == 1801) %>%
  select(SERIALNO, SPORDER, !!pop_weights) # %>%
  # collect() %>%
  # # if the year is 2017, remove the 2017 from the start of the SERIALNO
  # mutate(SERIALNO = if (!!year == 2017) as.integer(str_replace_all(.$SERIALNO, '^2017', '')) else .$SERIALNO,
  #        SERIALNO = as.integer(SERIALNO))

# iterate through each replicate weight
weight <- pop_weights[1]
for (weight in pop_weights) {
  
  # bring into memory dataframe with serialno, sporder and just one weight column
  wgt <- weights_tbl %>%
    select(SERIALNO, SPORDER, !!weight) %>%
    collect() %>%
    # if 2017, remove first four letters, which are year
    # this matches with the format for the population dataset
    mutate(SERIALNO = if (!!year == 2017) as.integer(str_replace_all(.$SERIALNO, '^2017', '')) else .$SERIALNO,
           SERIALNO = as.integer(SERIALNO)) %>%
    # convert to data table
    as.data.table()
  
  # this provides dataframe with income, weights, and geography
  # it can then be filtered by geography
  pop_wgt <- merge(pop, wgt, by = c('SERIALNO', 'SPORDER'))
  
  # change name of column so that it is the same of each iteration
  setnames(pop_wgt, weight, 'wgt')
  
  palmas[[weight]] <- 
    a <- pop_wgt[
    # filter out replicate weight values less than 1
    wgt > 0][
      # add additional rows based on the number of replicate weights
      rep(seq(.N), wgt), !"wgt"][
        # order based on income
        order(income, group)]#[
          # calculate palma for each group
          # use 'get' to convert string to object name
          #,.(palma_cal(income)), by = 'group']
  
}

# join weights with population dataset
popA <- pop %>%
  left_join(weights_tbl, c('SERIALNO', 'SPORDER'))



