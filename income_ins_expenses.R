########################################################################
#
# This script creates tables of average expenses for four family types:
#   1) One adult,
#   2) Two adults,
#   3) Two adults, one working, with a 2 and 4 year old
#   4) Two working adults with a 2 and 4 year old
#   5) One adult wit ha 2 and 4 year old
#
########################################################################

library(tidyverse)
library(data.table)
library(DBI)

source('income_ins_functions.R')

pop <- readRDS('population_expense.Rda') %>%
  filter(year == 2016)

# One adult
one <- pop %>%
  filter(num_persons == 1 & AGEP > 18) %>%
  expense_groupings(., 1)

# Two working adults
two <- pop %>%
  filter(num_persons == 2,
         ESR == TRUE,
         AGEP > 18) %>%
  expense_groupings(., 2)

# Two parents, one working, with a 2 and 4 year old
three <- pop %>%
  filter(num_persons == 4,
         num_working == 1) %>%
  filter(AGEP == 2 | AGEP == 4) %>%
  group_by(year, cntyname, SERIALNO, economic_unit) %>%
  mutate(age_sum = sum(AGEP)) %>%
  filter(age_sum == 6) %>%
  expense_groupings(., 3)

# Two working adults, with a 2 and 4 year old
four <- pop %>%
  filter(num_persons == 4,
         num_working == 2) %>%
  filter(AGEP == 2 | AGEP == 4) %>%
  group_by(year, cntyname, SERIALNO, economic_unit) %>%
  mutate(age_sum = sum(AGEP)) %>%
  filter(age_sum == 6) %>%
  expense_groupings(., 4)

# one working adult wit ha 2 and 4 year old
five <- pop %>%
  filter(num_persons == 3,
         num_working == 1) %>%
  filter(AGEP == 2 | AGEP == 4) %>%
  group_by(year, cntyname, SERIALNO, economic_unit) %>%
  mutate(age_sum = sum(AGEP)) %>%
  filter(age_sum == 6) %>%
  expense_groupings(., 5) 

expenses <- bind_rows(list(one,two,three,four,five))

write_csv(expenses, 'expenses.csv')
