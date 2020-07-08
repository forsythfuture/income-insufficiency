###################################################################################
#
# This is the second script to run!!
#
# This script takes as input the dataset showing whether each person is in an income
# insufficient economic unit and calculates aggregate income insufficiency rates
# for the total population and demographics.
#
####################################################################################

library(tidyverse)
library(data.table)
library(glue)

source('income_ins_functions.R')

# year to update
current_year <- 2018

# create age categories to be used when calculating income insufficiency rates for age groups
age_bins <- c(0, 17, 24, 44, 64, 150)
# create labels that are the end age
age_labels <- age_bins[-1]

# read dataset with expenses
pop <- read_csv(glue("population_expense-{current_year}.csv")) %>%
  # create age bins
  mutate(start_age = cut(AGEP, breaks = !!age_bins, 
                         labels = !!age_labels, 
                         include.lowest = TRUE),
         # convert to integer so it can be merged with population dataset
         start_age = as.integer(as.character(start_age)),
         # make race 100 for Hispanic of perso nis of hispanic origin
         RAC1P = ifelse(.$HISP != 1, 100, .$RAC1P)) %>%
  as.data.table()

# recode race to group races we will not use into one race
# this will reduce the number of groups and speed up grouping

race_labels <- c(seq(1, 9), 100)
race_recode <- c(1, 2, rep(4, 7), 3)

pop$RAC1P <- plyr::mapvalues(pop$RAC1P, race_labels, race_recode)

### iterate through each year calculating income insufficiency and standard errors
state <- 37

# initialize dataframe to store demographic income insufficiency for all demographics and years
demo_income_ins <- data.frame()
  
state <- 37 # NC is 37

year_file <- current_year - 2000
year_file <- str_pad(year_file, 2, "0", side = 'left')
pop_file <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{year_file}pnc.csv.gz')

# create connection with population replicate weights
weights_tbl <- read_csv(pop_file,
                        col_types = cols(RT= col_character(),
                                         NAICSP = col_character(),
                                         SOCP = col_character(),
                                         SERIALNO = col_character(),
                                         .default = col_double())) %>%
  # 2017 adds the numbers '2017' prior to serial number
  # remove these numbers
  mutate(SERIALNO = str_remove(SERIALNO, "[A-Z]{2}"),
         SERIALNO = str_remove(SERIALNO, "^[0-9]{4}"),
         SERIALNO = as.integer(SERIALNO),
         year = !!current_year) %>%
  filter(ST == !!state)

pop_year <- pop[year == current_year]

# replciate weight variable names are lower case until 2017 and upper case starting in 2017
weight_names <- ifelse(current_year >= 2017, 'PWGTP', 'pwgtp')
# replicate weight variables
pop_weights <- c('PWGTP', paste0(weight_names, seq(1, 80)))

# demographic columns to create income insufficiency for
demo_cols <- c('RAC1P', 'SEX', 'start_age', 'total')

# iterate through each demographic, calculating income insufficiency
for (col in demo_cols) {
  
  # if demographic column is 'total' then demo parameter is FALSE
  
  demo <- if (col == 'total') FALSE else TRUE
  
  # iterate through geo graphic areas
  for(geo_area in c('cntyname', 'ST')) {
    
    print(current_year)
    print(col)
    print(geo_area)
    
    # calculate income insufficeincy for given year and demographic
    demo_income_ins_single <- standard_errors(pop_year, geo_area, weights_tbl, pop_weights, col, demo) %>%
      # geo_area for PUMA is integer, and for county is character
      # convert PUMA to character so they can be combined
      mutate(geo_area = as.character(geo_area),
             year = current_year)
    
    # add specific year and demographic to dataset of all years and demographics
    demo_income_ins <- bind_rows(demo_income_ins, demo_income_ins_single)
    
  }
  
}
  
######################## This section cleans and creates the final dataset #################################

# only keep needed geographic areas
# 37 is NC state code, and represents the NC rate
demo_income_ins <- demo_income_ins %>%
  # change NC state code to name
  mutate(geo_area = ifelse(geo_area == "37", "North Carolina", geo_area)) %>%
  # only keep needed races (white, AA, hispanic)
  # races not one of these three are labeld 4 and can be removed
  filter(!(sub_demographic == 4 & demographic == 'RAC1P'))

#############################################################
#
# Income insufficiency is calculated per demographic, 
# but the demographic labels are just numbers. This
# script recodes the labels to more descriptive labels.
#############################################################

race_labels <- seq(1, 3)
race_recode <- c('White, non-Hispanic', 'African American', 'Hispanic / Latino')

sex_labels <- c(1, 2)
sex_recode <- c('Male', 'Female')

age_labels <- c(17, 24, 44, 64, 150)
age_recode <- c('0 to 17', '18 to 24', '25 to 44', '45 to 64', '65 plus')

# map recoding of sub demographics
demo_income_ins$sub_demographic <- ifelse(demo_income_ins$demographic == 'RAC1P', 
                                     plyr::mapvalues(demo_income_ins$sub_demographic, race_labels, race_recode),
                                           ifelse(demo_income_ins$demographic == 'SEX', 
                                                   plyr::mapvalues(demo_income_ins$sub_demographic, sex_labels, sex_recode),
                                                        ifelse(demo_income_ins$demographic == 'start_age', 
                                                               plyr::mapvalues(demo_income_ins$sub_demographic, age_labels, age_recode),
                                                               'None')))

# recode demographic names
demo_income_ins$demographic <- recode(demo_income_ins$demographic, 
                                 RAC1P = 'Race / Ethnicity', SEX = 'Gender', 
                                 start_age = 'Age', total = 'Comparison Community')

demo_income_ins <- demo_income_ins %>%
  # calculate MOE and CV
  mutate(moe = se * 1.96,
         cv = (se / income_ins) * 100) %>%
  rename(year = year, geo_description = geo_area, subtype = sub_demographic, type = demographic, estimate = income_ins) %>%
  select(geo_description, year, estimate, moe, se, cv, type, subtype) %>%
  arrange(year, type, geo_description, subtype)

write_csv(demo_income_ins, glue("income_ins_cleaned-{current_year}.csv"))
