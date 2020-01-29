###################################################################################
#
# This is the first script to run!!
#
# This script calculates income insufficiency for each person.
# It calculates expenses for each individual and creates a column
# that is a boolean representing whether the person is income
# insufficient; with TRUE being yes and FALSE being no.
# A person is income insufficient if they live in an economic unit
# where unit average expenses are greater than unit income.
# Then script also saves the output as an R object for quick import.
#
####################################################################################

library(tidyverse)
library(glue)
library(data.table)

source('income_ins_functions.R')

# update current year
current_year <- 2018

# create list of file names for s3 PUMS population files --------------------
years <- seq(8, current_year - 2000)
years <- str_pad(years, 2, "0", side = 'left')
pop_files <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{years}pnc.csv.gz')

# import needed PUMA data for all years
pop <- data.frame()

for (i in seq_along(pop_files[c(10,11)])) {
  
  print(pop_files[i])
  year <- as.numeric(years[i]) + 2000
  pop <- create_economic_units(pop_files[i], 37, year) %>%
    bind_rows(pop, .)
  
}

pop1 <- pop %>%
  tax_liability() %>%
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
saveRDS(pop, 'population_expense.Rda')
