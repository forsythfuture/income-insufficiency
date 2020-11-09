#This script imports the nc_tax_ind.csv file and filters for 2011
#Needed to caculate 2011 income insuff rates since all data except for 
#North Carolina data is missing from csv file

library(readr)
library(tidyverse)

setwd("C:/Users/crb61/Desktop/income-insufficiency/update_taxes")
nc_tax_liab_ind <- read_csv("nc_tax_liab_ind.csv")

nc_tax_liab_ind <- nc_tax_liab_ind %>%
  filter(year==2011)

write.csv(nc_tax_liab_ind, "nc_tax_liab_ind-2011.csv")
