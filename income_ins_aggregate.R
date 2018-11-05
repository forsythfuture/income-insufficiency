###################################################################################
#
# This script takes as input the dataset showing whether each person is in an income
# insufficient economic unit and calculates aggregate income insufficiency rates
# for the total population and demographics.
#
####################################################################################

library(tidyverse)
library(data.table)
library(DBI)

source('income_ins_functions.R')

con <- dbConnect(RSQLite::SQLite(), "pums_db.db")

# create age categories to be used when calculating income insufficiency rates for age groups
age_bins <- c(0, 17, 24, 44, 64, 150)
# create labels that are the end age
age_labels <- age_bins[-1]

# read dataset with expenses
pop <- readRDS('population_expense.Rda') %>%
  # create age bins
  mutate(start_age = cut(AGEP, breaks = !!age_bins, 
                         labels = !!age_labels, 
                         include.lowest = TRUE),
         # convert to integer so it can be merged with population dataset
         start_age = as.integer(as.character(start_age))) %>%
  as.data.table()

### iterate through each yearm calculating income insufficiency and standard errors
years <- seq(2006, 2017)
state <- 37


# initialize dataframe to store demographic income insufficiency for all demographics and years
demo_income_ins <- data.frame()

for (yr in years) {

  # create population weights table name based on year
  tbl_name <- as.character(yr) %>%
    str_extract(., '[0-9][0-9]$') %>%
    paste0('p_', .)
  
  # create connection with population replicate weights
  weights_tbl <- tbl(con, tbl_name) %>%
    filter(ST == !!state)
  
  pop <- pop[year == yr]
  
  # replciate weight variable names are lower case until 2017 and upper case starting in 2017
  weight_names <- ifelse(year >= 2017, 'PWGTP', 'pwgtp')
  # replicate weight variables
  pop_weights <- c('PWGTP', paste0(weight_names, seq(1, 80)))
  
  # demographic columns to create income insufficiency for
  demo_cols <- c('RAC1P', 'HISP', 'SEX', 'start_age', 'total')
  
  # iterate through each demographic, calculating income insufficiency
  for (col in demo_cols) {
    
    # if demographic column is 'total' then demo parameter is FALSE
    
    demo <- if (col == 'total') FALSE else TRUE
    
    print(yr)
    
    # iterate through geo graphic areas
    for(geo_area in c('PUMA', 'cntyname')) {
      
      # calculate income insufficeincy for given year and demographic
      demo_income_ins_single <- standard_errors(pop, geo_area, weights_tbl, pop_weights, col, demo) %>%
        mutate(year = yr)
      
      # add specific year and demographic to dataset of all years and demographics
      demo_income_ins <- bind_rows(demo_income_ins, demo_income_ins_single)
      
    }
    
    # save file as R object (in case program or computer crashes);
    # after calculating for both geographic areas in a demographic
    saveRDS(demo_income_ins, 'income_ins.Rda')
      
  }
  
}