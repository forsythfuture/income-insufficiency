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

expense_data <- data.frame(SERIALNO = c(1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5),
                           SPORDER = c(1, 1, 2, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3),
                           RELP = c(0, 0, 1, 0, 1, 2, 2, 0, 1, 2, 2, 0, 2, 2),
                           year = rep(2017, 14),
                           PINCP = c(40000, 40000, 40000, 40000, 0, 0, 0, 40000, 40000, 0, 0, 40000, 0, 0),
                           AGEP = c(40, 40, 40, 40, 40, 2, 4, 40, 40, 2, 4, 40, 2, 4),
                           SEX = c(1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 2, 2, 1),
                           ESR = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                           cntyname = rep('Forsyth', 14),
                           economic_unit = rep(TRUE, 14))

expenses <- expense_data %>% 
  child_care() %>%
  rent() %>%
  food() %>%
  ces() %>%
  meps() %>%
  select(-SPORDER:-economic_unit) %>%
  distinct()

#write_csv(expenses, 'expenses.csv')
