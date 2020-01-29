  ##################################################################
#
# This script takes as input individual tax liabilities, computes
# household tax liabilities, and merges with puma data.
#
##################################################################

library(tidyverse)

# get list of files that were output above
list_files <- list.files('tax_puma_cal/nc_from_taxsim')
dir <- 'tax_puma_cal/nc_from_taxsim/'

# add directory to file names
file_path <- paste0(dir, list_files)

# iterate through each file
for (i in seq_along(file_path)) {
  
  # calcualte year for creating file name to write out
  year <- as.character(i + 2005)
  
  # file name to write out
  file_out <- paste0('tax_puma_cal/nc_tax_complete/nc_tax_complete', year, '.csv')
  
  # read in tax data
  read_delim(file_path[1], delim = ' ') %>%
    # truncate id to remove decimal
    mutate(taxsim_id = floor(taxsim_id),
           # add federal and state tax to get overall taxes
           tax_liability = fiitax + siitax + fica) %>%
    # group by id, so we can calculate household tax liability
    group_by(taxsim_id) %>%
    # sum tax liability by group
    summarize(tax_liability = sum(tax_liability)) %>%
    # rename to match names in toher datasets
    rename(SERIALNO = taxsim_id) %>%
    write_csv(., file_out)
}
