##########################################################################
#
#  This program uses the TAXSIM functions to create datasets of tax liabilities.
#  These datasets are exported to csv files that can be run in the TAXSIM online
#
###########################################################################

library(tidyverse)
library(glue)

source('update_taxes/taxsim_functions.R')

# update the year to the current year
current_year <- 2018

# create list of file names for s3 PUMS population files --------------------
year_file <- current_year - 2000
year_file <- str_pad(year_file, 2, "0", side = 'left')
pop_file <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{year_file}pnc.csv.gz')
  
# calculate taxable income and write out results
tax_df <- pop_taxes(pop_file, current_year, household = FALSE)

# create file name of output based on year
file_name <- glue('update_taxes/nc_to_taxsim_online/to_tasxim_{year_file}.csv')
write_csv(tax_df, file_name, col_names = FALSE)