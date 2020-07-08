# Income Insufficiency

The expense estimates are designed to estimate an income level below which we could not assume a family could meet their needs. 
Most expense categories are based on averages, and some families may need more or less income than estimated.  For instance, child care can be a very large expense based on market rates, but many people may mitigate or eliminate this with their work schedule or assistance from others outside their household/family.  Extremely large medical expenses are present in some families, but it may not be reasonable to assume that every family must be prepared for them.

Many estimates are based on average expenses across all individuals/households, who may spend more money than is normal for a family just able to meet their needs.  As a result, income insufficiency should be presented as the proportion of families earning less than their estimated expenses or who could not meet their estimated expenses.  We cannot say that this is the percent of families unable to meet their needs, because we do not know each family’s actual required expenses.

### General structure of analysis

* Each household is divided into economic units

  * The core economic unit is composed of the reference person and everyone with a relationship with them.  This includes all people related to them, their spouse/partner, and any non-related children.  
  
  * This is a broader than how the census defines families.  The census does not include unmarried partners or nonrelated children in families.

    * While lay readers may find it easiest to understand economic units as families, we should avoid referring to them as families in technical documentation as the census identifies families under different rules which exclude nonmarried partners.  

  * All other people (adult roommates and other nonrelatives) are assigned to their own individual economic units.
  
    * There is no way to determine whether children not related to the reference person are cared for by the reference person or their roommate.  These children are assumed to be the responsibility of the reference person.

  * We assume that economic units are completely independent and receive no financial assistance from outside family/friends. 
  
* Some expenses are calculated based on individual characteristics.

  * Health care, child care, and food are calculated this way, and then summed by economic unit.
  
    * Health insurance is also based on age for seniors.

  * Later in the analysis, household child care expenses are set to zero if there are any adults in the family who are unemployed or not in the labor force.
  
    * These adults are not working at the time of the survey, but it is still possible that they worked for some portion of the previous year, and it is also possible that some part-time workers would also be able to provide some or all required child care.

* Some expenses are calculated based on the size of economic-units/families.

  * Housing, transportation, health insurance, and other expenses are calculated in this way.
  
* Tax groups are identified, and taxes are calculated.

  * Each economic unit is broken down into one or more tax groups, and federal/state taxes are calculated for each tax group separately.
  
    * First a tax group is created for the reference person, along with his/her spouse, and dependents (all children, and any sons/daughters under 23 who are in school).
  
    * Every other person in the economic unit is assigned a one-person tax group.

  * Taxes are calculated based on actual income.
  
    * Using actual income allows us to take non-wage income into account, but results in tax estimates that would not be accurate when income was equal to expenses.  
    
    * This does not affect income insufficiency rates; the estimated taxes would be overestimated for economic units with sufficient income, but since taxes always increase slower than income, this overestimation would never be greater than the surplus income and no household would be given an incorrect classification.  The same is true for insufficient income levels as well.
    
    * Example families are handled differently to ensure that their tax and overall expense estimates are accurate; Each example family is split into a thousand families with varying incomes, and the family with the lowest sufficient income is selected for each example family.
    
  * Upon calculating the incomes and number of dependents in tax units, taxes are estiamted using the [NBER's TASIM model](https://users.nber.org/~taxsim/taxsim27/).

* Expenses are summed for economic units, and compared to actual income to determine income insufficiency.

## Running the repo

Updating the yearly numbers requires updating three sections, in order: (1) expenses, (2) taxes, and (3) income insufficiency rates for aggregates and demographics.

Many of the files that are ran have a object defined near the top of the script that signifies the year. This represents the current year of the data. This should be the only items that changes yearly in the scripts.

### Updating expenses

Updating expenses is the first step. Expenses are updated in the Rmarkdown file `data/import_data.Rmd`. This file also contains instructions on how to update expenses, which require manual updates.

### Updating taxes

The two scripts to run to update taxes are found in the `updates_taxes` folder. They should be ran in order. The first script creates a data set that is in the format to manually send to the TAXSIM program. The file generated from this prgram, which is sent to TAXSIM, is located at `update_taxes/nc_to_taxsim_online/to_tasxim_[update year].csv`.

With this csv file created, users go to https://users.nber.org/~taxsim/taxsim27/, scroll down to the 'OR...Upload a file with TAXSIM data:' section, input the csv file located at `update_taxes/nc_to_taxsim_online/`, and click "Calculate using this file's data".

The output will show on a new screen. Users will need to copy the text and paste it into a text, `.txt`, file. This file should be saved at `update_taxes/nc_from_taxsim_online`. The filename should be `from_taxsim_[last two years of year].txt` (example: `from_taxsim_18.txt`)

### Calculating income insufficiency rates

The two files used to calculate income insufficiency rates are in the root directory. They are `1-income_ins_create_data.R` and `2-income_ins_aggregate.R`. These files should be ran in order. `2-income_ins_aggregate.R` produces the final csv file showing income insufficiency rates, which will be called `income_ins_cleaned-[current year].csv`.