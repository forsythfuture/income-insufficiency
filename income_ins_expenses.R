########################################################################
#
# This script creates tables of average expenses for four family types:
#   1) One adult,
#   2) Two adults,
#   3) Two adults, one working, with a 2 and 4 year old
#   4) Two working adults with a 2 and 4 year old
#   5) One adult wit ha 2 and 4 year old
#
# The script adds income amounts iteratively to calcualte expenses at a 
# variety of income levels. The same incomes will then be passed to the
# TAXSIM calculator to calculate taxes. The goal is to find the income
# level in each group that exquals estimated expenses.
#
########################################################################

library(tidyverse)
library(data.table)
library(DBI)

source('income_ins_functions.R')

# create initial 
expense_data <- data.frame(SERIALNO = c(1,2,2,3,3,3,3,4,4,4,4,5,5,5),
                           SPORDER = c(1,1,2,1,2,3,4,1,2,3,4,1,2,3),
                           RELP = c(0,0,1,0,1,2,2,0,1,2,2,0,1,2),
                           year = rep(2017, 14),
                           PINCP = c(12000,12000,12000,26000,0,0,0,21000,21000,0,0,40000,0,0),
                           AGEP = c(40, 40, 40, 40, 40, 2, 4, 40, 40, 2, 4, 40, 2, 4),
                           SEX = c(1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 2, 2, 1),
                           ESR = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                           cntyname = rep('Forsyth', 14),
                           economic_unit = rep(TRUE, 14),
                           iteration = rep(1, 14))

# multiply 3% to each income level, and do this iteratviely 25 times;
# then add these amounts to the dataframe
# must multiple percent instead of adding value so that zero values remain zero
addition_amount = 1.03

# create copy of initial datarame, so we can keep increasing the income amounts
# on a dataframe while retaining the original
expense_data_copy <- expense_data

for (i in seq_len(30)) {
  
  if (i == 1) {
    
    # calculate non-taxable income expenses
    expenses <- expense_data %>% 
      child_care() %>%
      rent() %>%
      food() %>%
      ces() %>%
      meps() %>%
      select(-SPORDER:-economic_unit) %>%
      distinct() %>%
      mutate(total_expenses = rowSums(.[3:9]))
    
  } else {
  
    # create dataset with multiplied income
    expense_data_copy <- expense_data_copy %>%
      mutate(PINCP = PINCP * !!addition_amount,
             iteration = i) 
    
    # calcualte expenses with revised income dataset
    expenses_copy <- expense_data_copy %>%
      child_care() %>%
      rent() %>%
      food() %>%
      ces() %>%
      meps() %>%
      select(-SPORDER:-economic_unit) %>%
      distinct() %>%
      mutate(total_expenses = rowSums(.[3:8]))
    
    # add total expenses to main dataset
    expenses <- bind_rows(expenses, expenses_copy)
    
    # add incomes to main dataset
    expense_data <- bind_rows(expense_data, expense_data_copy)

  }
  
}

# convert expense data to a format that is readable by TAXSIM
# create incomes with each spouses income on differenct columns
taxsim_income <- expense_data %>% 
  group_by(iteration, SERIALNO) %>% 
  select(SPORDER, PINCP) %>% 
  spread(SPORDER, PINCP) %>%
  # zero values in columns three and four represent kids,
  # convert these to2, signifying two kids under the required ages
  mutate(kids = ifelse(`3` == 0, 2, 0),
         # NA values represent 0 kids
         kids = ifelse(is.na(kids), 0, kids)) %>%
  rename(pwages = `1`, swages =`2`) %>%
  select(SERIALNO, iteration, pwages, swages, kids) %>%
  mutate(swages = ifelse(is.na(swages), 0 , swages),
         year = 2017,
         state = 34,
         mstatus = ifelse(SERIALNO %in% c(1,5), 1,2),
         page = 40,
         sage = ifelse(SERIALNO %in% c(1,5), 0,40),
         depx = kids,
         dep13 = kids,
         dep17 = kids,
         dep18 = kids) %>%
  select(-kids) %>%
  # combine iteration and serial number into one column so it can be the taxsim ID
  unite(id, SERIALNO, iteration, sep='') %>%
  # place columns in proper order, and all future columns will be zeroes
  select(id, year, state, mstatus, page:dep18, pwages, swages)

# create matrix of all zeroes for remaining columns
# then convert to dataframe
zero_matrix <- matrix(0, nrow(taxsim_income), 27 - ncol(taxsim_income)) %>%
  as.data.frame()

# bind zero matrix with regular dataframe
taxsim_income <- bind_cols(taxsim_income, zero_matrix)

#write_csv(taxsim_income, 'to_taxsim.csv', col_names = FALSE)
