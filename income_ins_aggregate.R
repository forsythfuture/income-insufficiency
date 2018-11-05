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

### read in replicate weights
year <- 2017
year_dt <- year
state <- 37

con <- dbConnect(RSQLite::SQLite(), "pums_db.db")

# create population weights table name based on year
tbl_name <- as.character(year) %>%
  str_extract(., '[0-9][0-9]$') %>%
  paste0('p_', .)

# create connection with population replicate weights
weights_tbl <- tbl(con, tbl_name) %>%
  filter(ST == !!state,
         PUMA == 1801)

pop <- pop[year == year_dt]

# replciate weight variable names are lower case until 2017 and upper case starting in 2017
weight_names <- ifelse(year >= 2017, 'PWGTP', 'pwgtp')
# replicate weight variables
pop_weights <- c('PWGTP', paste0(weight_names, seq(1, 80)))

col <- 'RAC1P'

df <- standard_errors(pop, weights_tbl, pop_weights[1:11], col)