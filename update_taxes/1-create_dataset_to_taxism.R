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
year_file <- str_pad(year, 2, "0", side = 'left')
pop_file <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{year_file}pnc.csv.gz')
  
# calculate taxable income and write out results
tax_df <- pop_taxes(pop_file, current_year)

# TAXSIM ID's cannot have letters, so take out letters of serial numbers, but save so we can reinsert
# also take out first four letters

# map to each serial number an integer
serial_num_mapping <- data.frame(
  'SERIALNO' = tax_df$SERIALNO,
  'id' = seq(1, nrow(tax_df)),
  'year' = current_year
)

# replace serial number with integer
tax_df$SERIALNO <- serial_num_mapping$id

# create file name of output based on year
file_name <- glue('update_taxes/nc_to_taxsim_online/to_tasxim_{year_file}.csv')
write_csv(tax_df, file_name, col_names = FALSE)

# write out mapping of serial number and id data frame
write_rds(serial_num_mapping, glue('update_taxes/nc_from_taxsim_online/serial_num_mapping_{year_file}.rds'))
  