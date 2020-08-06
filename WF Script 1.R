

library(tidyverse)
library(glue)
library(data.table)
library(vroom)

source('income_ins_functions.R')

# update current year
current_year <- 2018

# create list of file names for s3 PUMS population files --------------------
year_file <- current_year - 2000
year_file <- str_pad(year_file, 2, "0", side = 'left')
pop_file <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{year_file}pnc.csv.gz')

# bin households in economic units
pop <- create_economic_units(pop_file, 37, current_year)

# calculate expenses
pop <- pop %>%
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

# save intermediate output
write_csv(pop, glue("population_expense-{current_year}.csv"))
