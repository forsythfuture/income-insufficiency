###################################################################################
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
library(data.table)
library(DBI)

source('income_ins_functions.R')

con <- dbConnect(RSQLite::SQLite(), "pums_db.db")

## to delete
#year <- 2016
#state <- 37

# import needed PUMA data for all years
pop <- data.frame()

for (yr in seq(2016, 2016)) {
  
  print(yr)
  pop <- create_economic_units(con, yr, 37) %>%
    bind_rows(pop, .)
  
}

pop <- pop %>%
  child_care() %>%
  tax_liability() %>%
  rent() %>%
  food() %>%
  ces() %>%
  meps() %>%
  # calculate income insufficiency
  mutate(income_insufficient = economic_unit_income - (tax_liability + rowSums(.[,20:25], na.rm = TRUE)),
         # true if income insufficienct false if income sufficient
         income_insufficient = ifelse(income_insufficient < 0, TRUE, FALSE)) %>%
  select(-SPORDER, -ST, -RELP)

# save intermediate output
#saveRDS(pop, 'population_expense.Rda')