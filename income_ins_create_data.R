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

# import needed PUMA data for all years
pop <- data.frame()

for (yr in seq(2012, 2016)) {
  
  print(yr)
  pop <- create_economic_units(con, yr, 37) %>%
    bind_rows(pop, .)
  
}

pop <- pop %>%
  filter(year == 2016) %>%
  tax_liability() %>%
  child_care() %>%
  rent() %>%
  food() %>%
  ces() %>%
  meps() %>%
  # calculate income insufficiency by subtracting expenses from income
  mutate(income_insufficient = economic_unit_income - rowSums(.[,21:27], na.rm = TRUE),
         # true if income insufficienct false if income sufficient
         income_insufficient = ifelse(income_insufficient < 0, TRUE, FALSE)) %>%
  select(-RELP)

pop <- pop %>%
  filter(num_persons == 2,
         num_adults == 2) %>%
  distinct(SERIALNO, .keep_all=TRUE) %>%
  summarize(mean(economic_unit_taxes),
            median(economic_unit_taxes))

# save intermediate output
#saveRDS(pop, 'population_expense.Rda')

income_ins <- pop %>%
  group_by(year) %>%
  summarize(sum(income_insufficient) / n())
