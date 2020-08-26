#######################################################################################
#
# 2nd script: Run this script after WF Script 1.R
#
# This script takes as input the dataset showing whether each person is in an income
# insufficient economic unit and calculates aggregate income insufficiency rates
# for the total population and demographics.
#
#########################################################################################

source('income_ins_functions.R')

# year to update
current_year <- 2018

# create age categories to be used when calculating income insufficiency rates for age groups ~
#Create Under 18 and 18+ age groups
age_bins <- c(0, 17, 150)
# create labels that are the end age
age_labels <- age_bins[-1]

# read dataset with expenses
pop <- vroom(glue("population_expense-{current_year}.csv")) %>%
  # create age bins
  mutate(start_age = cut(AGEP, breaks = !!age_bins, 
                         labels = !!age_labels, 
                         include.lowest = TRUE),
         # convert to integer so it can be merged with population dataset
         start_age = as.integer(as.character(start_age)),
         # make race 100 for Hispanic and persons of hispanic origin
         RAC1P = ifelse(.$HISP != 1, 100, .$RAC1P))

###Creating Sex x Age variable with 4 categorties, 18+ Male/Female; Under 18 Male/Female
###Labels will be attached after income insufficiency calculations (needs to be numeric)

pop$Agesex <- do.call(paste, c(pop[c("SEX", "start_age")], sep = ""))

testAgesex<- pop %>%
  select(SEX, start_age, Agesex) %>%
  group_by(SEX,as.character(start_age), Agesex) %>%
  summarise(count=n())

class(pop$Agesex)

#Coverting character to numeric 
pop$Agesex <- as.numeric(as.character(pop$Agesex))

class(pop$Agesex)

# recode race to combine races we will not use into one group (4)
# this will reduce the number of groups and speed up grouping
# also recode hispanic / latino to 3

# groups we don't need
dont_need_race <- seq(3, 9, 1)

pop <- pop %>%
  mutate(RAC1P = ifelse(RAC1P %in% !!dont_need_race, 4, RAC1P),
         RAC1P = ifelse(RAC1P == 100, 3, RAC1P))

##Creating Race x Sex and Age Variable (Race x Sex for 18+, Race x Sex for Under 18--3 categories~
# White, Black, Hispanic)
##Labels will be attached after income insufficiency calculations (needs to numeric)

pop$RaceSex <- do.call(paste, c(pop[c("RAC1P", "Agesex")], sep = ""))

testRaceSex<- pop %>%
  select(RAC1P, Agesex, RaceSex) %>%
  group_by(RAC1P,as.character(Agesex), RaceSex) %>%
  summarise(count=n())

class(pop$RaceSex)

#Coverting character to numeric 
pop$RaceSex <- as.numeric(as.character(pop$RaceSex))

class(pop$RaceSex)

###################################################################################

### iterate through each year calculating income insufficiency and standard errors
state <- 37 # NC is state 37

# initialize dataframe to store demographic income insufficiency for all demographics and years
demo_income_ins <- data.frame()

year_file <- current_year - 2000
year_file <- str_pad(year_file, 2, "0", side = 'left')
pop_file <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{year_file}pnc.csv.gz')

# create connection with population replicate weights
weights_tbl <- vroom(pop_file,
                     col_types = cols(RT= col_character(),
                                      NAICSP = col_character(),
                                      SOCP = col_character(),
                                      SERIALNO = col_character(),
                                      .default = col_double())) %>%
  # 2017 adds the numbers '2017' prior to serial number
  # remove these numbers
  mutate(SERIALNO = str_remove(SERIALNO, "[A-Z]{2}"),
         SERIALNO = str_remove(SERIALNO, "^[0-9]{4}"),
         SERIALNO = as.integer(SERIALNO),
         year = !!current_year) %>%
  filter(ST == !!state) %>%
  as.data.table()

pop_year <- pop[pop$year == current_year,]

# convert to datatables faster processing and better memory management
pop <- as.data.table(pop)
pop_year <- as.data.table(pop_year)

# replicate weight variable names are lower case until 2017 and upper case starting in 2017
weight_names <- ifelse(current_year >= 2017, 'PWGTP', 'pwgtp')

# replicate weight variables
pop_weights <- c('PWGTP', paste0(weight_names, seq(1, 80)))

# demographic columns to create income insufficiency for
demo_cols <- c('RAC1P', 'SEX', 'start_age', 'Agesex', 'RaceSex', 'total')

# iterate through each demographic, calculating income insufficiency
for (col in demo_cols) {
  
  # if demographic column is 'total' then demo parameter is FALSE
  
  demo <- if (col == 'total') FALSE else TRUE
  
  # iterate through geo graphic areas
  for(geo_area in c('cntyname', 'ST')) {
    
    print(current_year)
    print(col)
    print(geo_area)
    
    # calculate income insufficeincy for given year and demographic
    demo_income_ins_single <- standard_errors(pop_year, geo_area, weights_tbl, pop_weights, col, demo) %>%
      # geo_area for PUMA is integer, and for county is character
      # convert PUMA to character so they can be combined
      mutate(geo_area = as.character(geo_area),
             year = current_year)
    
    # add specific year and demographic to dataset of all years and demographics
    demo_income_ins <- bind_rows(demo_income_ins, demo_income_ins_single)
    
  }
  
}

######################## This section cleans and creates the final dataset #################################

#Filtering out needed Geography: Forsyth County
demo_income_ins1 <- demo_income_ins %>%
  filter(geo_area == 'Forsyth') %>%
  # only keep needed races (White, Black, Hispanic)
  # races not one of these three are labeled 4 and can be removed
  filter(!(sub_demographic == 4 & demographic == 'RAC1P')) %>%
  #Removing all RaceSex that includes label 4 from RAC1P variable (42150, 41150, 4117, 4217)
  filter(!(sub_demographic >= 4117 & demographic == 'RaceSex'))


######################################################################################
#
# Income insufficiency is calculated per demographic, but the demographic labels are 
# just numbers. This script recodes the labels to more descriptive labels.
#
#######################################################################################

# mappings of demographic integer in dataset to description
# race was recoded earlier in script

sex_recode <- c(`1` = 'Male', `2` = 'Female')

dont_keep <- 'Do not keep'

race_recode <- c(
  `1` = 'White, non-Hispanic',
  `2` = 'Black, non-Hispanic',
  `3` = 'Hispanic/Latino',
  `4` = 'Do not keep'
)

age_recode <- c(
  `17` = 'Child',
  `150` = 'Adult'
)

AgeSex_recode <- c(
  `1150` = 'Male Adult',
  `2150` = 'Female Adult',
  `117` = 'Male Child',
  `217` = 'Female Child'
)

RaceSex_recode <- c(
  `11150` = 'White Male Adult',
  `1117` = 'White Male Child', 
  `12150` = 'White Female Adult',
  `1217`= 'White Female Child',
  `21150` = 'Black Male Adult',
  `2117` = 'Black Male Child', 
  `22150` = 'Black Female Adult',
  `2217`= 'Black Female Child',
  `31150` = 'Hispanic/Latino Male Adult',
  `3117` = 'Hispanic/Latino Male Child', 
  `32150` = 'Hispanic/Latino Female Adult',
  `3217`= 'Hispanic/Latino Female Child',
  `41150` = 'Do not keep',
  `4117` = 'Do not keep', 
  `42150` = 'Do not keep',
  `4217`= 'Do not keep'
)
  

# if this phrase is spotted in the final dataset then check these values because the row did not recode
default_recode <- 'did not recode - check'

demo_income_ins1$sub_demographic <- ifelse(demo_income_ins1$demographic == 'RAC1P',
                                   recode(demo_income_ins1$sub_demographic, !!!race_recode, .default = default_recode),
                                   ifelse(demo_income_ins1$demographic == 'SEX',
                                   recode(demo_income_ins1$sub_demographic, !!!sex_recode, .default = default_recode),
                                   ifelse(demo_income_ins1$demographic == 'start_age',
                                   recode(demo_income_ins1$sub_demographic, !!!age_recode, .default = default_recode),
                                   ifelse(demo_income_ins1$demographic == 'Agesex',
                                   recode(demo_income_ins1$sub_demographic, !!!AgeSex_recode, .default = default_recode),
                                   ifelse(demo_income_ins1$demographic == 'RaceSex',
                                   recode(demo_income_ins1$sub_demographic, !!!RaceSex_recode, .default = default_recode),
                                          'None'))))) 
                                                             
                                    
# recode demographic names
demo_income_ins1$demographic <- recode(demo_income_ins1$demographic, 
                                      RAC1P = 'Race/Ethnicity', 
                                      SEX = 'Gender',
                                      Agesex = 'Age by Sex',
                                      RaceSex = 'Race/Ethnicity by Sex',
                                      start_age = 'Age', 
                                      total = 'Forsyth County Total')

demo_income_ins1 <- demo_income_ins1 %>%
  # calculate MOE and CV
  mutate(moe = se * 1.96,
         cv = (se / income_ins) * 100) %>%
  rename(year = year, geo_description = geo_area, subtype = sub_demographic, type = demographic, estimate = income_ins) %>%
  select(geo_description, year, estimate, moe, se, cv, type, subtype) %>%
  arrange(year, type, geo_description, subtype)



write_csv(demo_income_ins1, glue("income_ins_WF2018-{current_year}.csv"))

###########################################################################################
# 
#Formatting for Shiny 
#
############################################################################################
Sincomeinsuff <- mutate(demo_income_ins1) %>%
  mutate(year = "2018") %>%
  mutate(success = " ") %>%
  mutate(trials = " ") %>%
  mutate(geo_description = "Forsyth County, NC") %>%
  select(year, geo_description, type, subtype, estimate, success, trials, se)

write.csv(Sincomeinsuff, "Shiny_IncomeInsuffiency.csv")

###########################################################################################
# 
#Formatting for Tableau 
#
############################################################################################

Tincomeinsuff <- demo_income_ins1 %>%
  mutate(Year = "2018") %>%
  mutate(MoE = moe) %>%
  mutate(Percent = estimate) %>%
  select(Year, type, subtype, Percent, moe)

write.csv(Tincomeinsuff, "Tableau_IncomeInsuffiency.csv")
