library(tidyverse)
library(DBI)

source('income_ins_functions.R')

con <- dbConnect(RSQLite::SQLite(), "pums_db.db")

#tax_liabilities <- readRDS('nc_tax_liab_ind.Rda')

# import needed PUMA data and create economic units
pop <- create_economic_units(con, 2017, 37) %>%
  meps()