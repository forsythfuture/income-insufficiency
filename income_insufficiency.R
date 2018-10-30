library(tidyverse)
library(DBI)

source('income_ins_functions.R')

con <- dbConnect(RSQLite::SQLite(), "pums_db.db")



# import needed PUMA data and create economic units
pop <- create_economic_units(con, 2017, 37) %>%
  rent()

