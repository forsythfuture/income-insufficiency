library(tidyverse)
library(glue)

### take estimated tax liability from taxsim output and distill into estimate household tax liability

# change to current year
current_year <- 2018

# import current year's tax libility dataset that was output from TAXSIM

# create filepath to file that is the text file of output copied from TAXSIM
file_year <- current_year - 2000
from_taxsim_filepath <- glue('update_taxes/nc_from_taxsim_online/from_taxsim_{file_year}.txt')

### take estimated tax liability from taxsim output and distill into estimate tax liability

# import all files into a list
tax_liab <- read_delim(from_taxsim_filepath, delim = ' ',
                       col_types = cols(.default = "n")) %>%
  # payrool (FICA) taxes are employer's and employee's share
  # we only want employee's share, so cut in half
  mutate(fica = round(fica / 2,0),
         # calculate total tax liability, which is the sum of
         # federal income, state income, and payroll taxes (FICA)
         total_taxes = as.integer(fiitax + siitax + fica),
         # remove last two letters in taxsim_id (SERIALNO) and make SPORDER
         SPORDER = as.integer(str_extract(taxsim_id, '[0-9][0-9]$')),
         taxsim_id = as.integer(str_replace(taxsim_id, '[0-9][0-9]$', ''))) %>%
  # change name of taxsim_id to SERIALNO so that it matches PUMA terminology
  rename(SERIALNO = taxsim_id) %>%
  select(SERIALNO, SPORDER, year, total_taxes) %>%
  # sort by year and serial no
  arrange(year, SERIALNO)

# add current year's tax data to prior year's
prior_taxes <- read_csv('update_taxes/nc_tax_liab_ind.csv')

all_years <- bind_rows(prior_taxes, tax_liab)

# write out tax liabilities
write_csv(all_years, 'update_taxes/nc_tax_liab_ind.csv')
