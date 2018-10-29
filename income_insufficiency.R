library(tidyverse)
library(DBI)

con <- dbConnect(RSQLite::SQLite(), "pums_db.db")

# con: database connection
# year: year to pull pums

# establish connection to tables
year <- 2017
# create table name by extracting last two digits from year
# and placing after 'p_'
yr <-str_extract(as.character(year), '[0-9][0-9]$')
table_name <- paste0('p_', yr)

# connect to database table
population <- tbl(con, table_name)

pop_vars <- c('SERIALNO', # serial number grouped by household 
              'SPORDER', # order of person in household
              'ST', # state (needed to calculate state income taxes)
              'PUMA', # Public use micro area
              # relationship to reference person; variable name changed in 2010
              ifelse(year < 2010, 'REL', 'RELP'),
              'PINCP' # total personal income
              )

# import population data
pop <- population %>%
  select(!!pop_vars) %>%
  # need to collect now because cannot transform NA without collecting
  filter(ST == 37,
         PUMA == 1801) %>%
  collect()

### calculate economic units

# An economic unit is the reference person and everyone with a relationship to the reference person.
# This includes spouses, partners, and any children in the house (whether related or not)
# All people in the household outside this group are assigned their own ecenomic unit

# economic units are derived from the relationship variable
# these variables are changed coding in 2008;
# therefore we need two objects representing these variables, depending on year
econonmic_unit_a <- c(0,1,2,3,4,5,6,7,10,11)
econonmic_unit_a <- c(0,1,2,3,4,5,6,7,8,9,10,13,14,15)
