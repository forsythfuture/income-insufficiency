df <- read_rds('population_expense.Rda') %>%
  filter(economic_unit == T) %>%
  select(SERIALNO, cnty)

