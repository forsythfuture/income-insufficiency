#############################################################
#
# Income insufficiency is calculated per demographic, 
# but the demographic labels are just numbers. This
# script recodes the labels to more descriptive labels.
# Hispanic/Latino
# 1 = not 2-24 = hispanic origin
#############################################################

race_labels <- seq(1, 9)
race_recode <- c('White, non-Hispanic', 'African American', rep('Other', 7))

hisp_labels <- seq(1, 24)
hisp_recode <- c('not Hispanic/Latino', rep('Hispanic/Latino', 23))

sex_labels <- c(1, 2)
sex_recode <- c('Male', 'Female')

