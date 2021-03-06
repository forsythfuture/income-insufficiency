#1st income insufficiency script for 2011

library(tidyverse)
library(glue)
library(data.table)
library(vroom)

source('income_ins_functions.R')

# update current year
current_year <- 2011

# create list of file names for s3 PUMS population files --------------------
year_file <- current_year - 2000
year_file <- str_pad(year_file, 2, "0", side = 'left')
pop_file <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{year_file}pnc.csv.gz')

# bin households in economic units
pop1 <- create_economic_units(pop_file, 37, current_year)

# calculate expenses
pop1 <- pop1 %>%
  tax_liability(current_year) %>%
  child_care() %>%
  rent() %>%
  food() %>%
  ces() %>%
  meps() %>%
  # calculate income insufficiency by subtracting expenses from income
  mutate(income_insufficient = economic_unit_income - rowSums(.[,15:22], na.rm = TRUE),
         # true if income insufficienct false if income sufficient
         income_insufficient = ifelse(income_insufficient < 0, TRUE, FALSE)) %>%
  select(-RELP, -ESR, -economic_unit_income:-economic_unit_meps)

#Weird column X1
#pop1 <- subset(pop1, select = -c(X1))


# save intermediate output
write_csv(pop1, glue("population_expense-{current_year}.csv"))

#######################################################################################################


#######################################################################################
# This script caclulates the 2011 income insuff rates for NC, FC, DC, and GC
#######################################################################################

library(tidyverse)
library(data.table)
library(vroom)
library(glue)

source('income_ins_functions.R')

# year to update
current_year <- 2011

# create age categories to be used when calculating income insufficiency rates for age groups
age_bins <- c(0, 17, 24, 44, 64, 150)
# create labels that are the end age
age_labels <- age_bins[-1]

# read dataset with expenses
pop1 <- vroom(glue("population_expense-{current_year}.csv")) %>%
  # create age bins
  mutate(start_age = cut(AGEP, breaks = !!age_bins, 
                         labels = !!age_labels, 
                         include.lowest = TRUE),
         # convert to integer so it can be merged with population dataset
         start_age = as.integer(as.character(start_age)),
         # make race 100 for Hispanic and persons of hispanic origin
         RAC1P = ifelse(.$HISP != 1, 100, .$RAC1P))

# recode race to combine races we will not use into one group (4)
# this will reduce the number of groups and speed up grouping
# also recode hispanic / latino to 3

# groups we don't need
dont_need_race <- seq(3, 9, 1)

pop1 <- pop1 %>%
  mutate(RAC1P = ifelse(RAC1P %in% !!dont_need_race, 4, RAC1P),
         RAC1P = ifelse(RAC1P == 100, 3, RAC1P))

### iterate through each year calculating income insufficiency and standard errors
state <- 37 # NC is state 37

# initialize dataframe to store demographic income insufficiency for all demographics and years
demo_income_ins <- data.frame()

year_file <- current_year - 2000
year_file <- str_pad(year_file, 2, "0", side = 'left')
pop_file <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{year_file}pnc.csv.gz')

# create connection with population replicate weights
weights_tbl <- vroom(pop_file,
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
  filter(ST == !!state) %>%
  as.data.table()

pop1_year <- pop1[pop1$year == current_year,]

# convert to datatables faster processing and better memory management
pop1 <- as.data.table(pop1)
pop1_year <- as.data.table(pop1_year)

# replicate weight variable names are lower case until 2017 and upper case starting in 2017
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
    demo_income_ins_single <- standard_errors(pop1_year, geo_area, weights_tbl, pop_weights, col, demo) %>%
      # geo_area for PUMA is integer, and for county is character
      # convert PUMA to character so they can be combined
      mutate(geo_area = as.character(geo_area),
             year = current_year)
    
    # add specific year and demographic to dataset of all years and demographics
    demo_income_ins <- bind_rows(demo_income_ins, demo_income_ins_single)
    
  }
  
}

######################## This section cleans and creates the final dataset #################################

# only keep needed geographic areas. 
demo_income_ins <- demo_income_ins %>%
  # 37 is NC state code, and represents the NC rate
  mutate(geo_area = ifelse(geo_area == '37', "North Carolina", geo_area)) %>%
  # filtering for needed counties and state of NC
  filter(geo_area %in% c('Forsyth', 'Durham', 'Guilford', 'North Carolina')) %>%
  # only keep needed races (white, AA, hispanic)
  # races not one of these three are labeled 4 and can be removed
  filter(!(sub_demographic == 4 & demographic == 'RAC1P'))


#######################################################################################
#
# Income insufficiency is calculated per demographic,  but the demographic labels 
# are just numbers. This script recodes the labels to more descriptive labels.
#
#######################################################################################

# mappings of demographic integer in dataset to description
# race was recoded earlier in script

gender_recode <- c(`1` = 'Male', `2` = 'Female')

age_recode <- c(
  `17` = '0 to 17',    
  `24` = '18 to 24',
  `44` = '25 to 44',
  `64` = '45 to 64',
  `150` = '65 plus'
)

dont_keep <- 'Do not keep'

race_recode <- c(
  `1` = 'White, non-Hispanic',
  `2` = 'African American',
  `3` = 'Hispanic / Latino',
  `4` = 'Do not keep'
)

# if this phrase is spotted in the final dataset then check these values because the row did not recode
default_recode <- 'did not recode - check'

demo_income_ins$sub_demographic <- ifelse(demo_income_ins$demographic == 'RAC1P',
                                          recode(demo_income_ins$sub_demographic, !!!race_recode, .default = default_recode),
                                          ifelse(demo_income_ins$demographic == 'SEX',
                                                 recode(demo_income_ins$sub_demographic, !!!gender_recode, .default = default_recode),
                                                 ifelse(demo_income_ins$demographic == 'start_age',
                                                        recode(demo_income_ins$sub_demographic, !!!age_recode, .default = default_recode),
                                                        'None')))

# recode demographic names
demo_income_ins$demographic <- recode(demo_income_ins$demographic, 
                                      RAC1P = 'Race / Ethnicity', SEX = 'Gender', 
                                      start_age = 'Age', total = 'Comparison Community')

# Relabeling county names to match previous files/years
demo_income_ins$geo_area <-if_else(demo_income_ins$geo_area=="Durham", "Durham County, NC",
                                   if_else(demo_income_ins$geo_area=="Forsyth", "Forsyth County, NC",
                                           if_else(demo_income_ins$geo_area=="Guilford", "Guilford County, NC",
                                                   if_else(demo_income_ins$geo_area=="North Carolina", "North Carolina", NA_character_)))) 

# Relabeling county names to match previous files/years, sub demo = none, relabeling total
demo_income_ins <- demo_income_ins %>%
  mutate(sub_demographic = ifelse(sub_demographic == 'None', "Total", sub_demographic))

demo_income_ins <- demo_income_ins %>%
  # calculate MOE and CV
  mutate(moe = se * 1.96,
         cv = (se / income_ins) * 100) %>%
  rename(year = year, geo_description = geo_area, subtype = sub_demographic, type = demographic, estimate = income_ins) %>%
  select(geo_description, year, estimate, moe, se, cv, type, subtype) %>%
  arrange(year, type, geo_description, subtype)

write_csv(demo_income_ins, glue("income_ins_cleaned-{current_year}.csv"))
