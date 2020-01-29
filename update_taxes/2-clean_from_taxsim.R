library(tidyverse)

source('update_taxes/taxsim_functions.R')

### take estimated tax liability from taxsim output and distill into estimate household tax liability

# create list of all tax output files
file_names <- list.files('tax_puma_cal/nc_from_taxsim_online', full.names = TRUE)

# import all files into a list
tax_liab <- lapply(file_names, read_delim, delim = ' ') %>%
  # currently each year is in a separerate list element
  # combine all years into a single datarame
  # this is possible because there is a year column
  bind_rows() %>%
  # calculate total tax liability, which is the sum of
  # federal income, state income, and payroll taxes (FICA)
  mutate(total_taxes = fiitax + siitax + fica) %>%
  # group by year and serial number to calculate household taxes
  group_by(taxsim_id, year) %>%
  summarize(tax_liability = sum(total_taxes)) %>%
  ungroup() %>%
  # change name of taxsim_id to SERIALNO so that it matches PUMA terminology
  rename(SERIALNO = taxsim_id) %>%
  # sort by year and serial no
  arrange(year, SERIALNO)

# write out tax liabilities
#write_csv(tax_liab, 'tax_puma_cal/nc_tax_liabilities.csv')
