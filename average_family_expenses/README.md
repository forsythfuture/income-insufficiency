# Calcualting family expenses for typical family

This folder calculates average family expenses for five typical families:

1. One adult,
2. Two adults,
3. Two adults, one working, with a 2 and 4 year old
4. Two working adults with a 2 and 4 year old
5. One adult with a 2 and 4 year old

Expenses are calculated at a variety of income levels.

This folder does not contribute to calculatingthe income insufficiency rate. Nothing in this filder needs to be ran to calculate income insufficiency rates for geographic areas.

The first R script performs two tasks.  First, it outputs a dataset that lists the family types by income, and the average expenses for the given family type and income. The output is sent to `average_family_expenses/average_expenses_missing_taxes-[current year].csv`.

The first script also creates a dataset for each family type and income combination that can be sent to TAXSIM, so estimated taxes can be calculated. This dataset is called `average_family_expenses/to_taxsim-[current_year].csv`. Again, this dataset is in the proper format to send to TAXSIM. The README in the root directory contains more information about sending datasets to TAXSIM.

An additional script will need to be created if one wishes to combine the expense and tax information. Such scripts do not currently exists. 