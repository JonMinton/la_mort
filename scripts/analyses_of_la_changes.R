# Script which works with tidied data produced in la_tidy_data.R file

# standardised data on changes in expenditure in different LAs 

rm(list=ls())


# pre-requisites ----------------------------------------------------------


require(plyr)
require(stringr)
require(tidyr)
require(dplyr)
require(car)


require(ggplot2)
require(lattice)

require(readxl)
require(readr)


# Data --------------------------------------------------------------------

dta <- read_csv(file = "data/care_cuts/combined_linked_and_tidied_r03.csv")

# keep only the following inner categories

dta <- dta %>% filter(inner %in% 
                 c(
                   "adults_65plus",
                   "adults_u65_learning",
                   "adults_u65_mental",
                   "adults_u65_other",
                   "adults_u65_physical",
                   "adults_u65_social_care"
                   ) & start_year > 2008
               )

ecode_lookup <- read_excel("data/care_cuts/4_digit_code_extract.xlsx", sheet="Sheet1")
ecode_lookup <- ecode_lookup  %>% 
  rename(
    ecode = `E-code`, 
    local_authority = `Local authority`, 
    region= Region, class= Class)

dta <- dta %>% inner_join(ecode_lookup) %>% filter(class !="SD")

tot_exp <- dta %>% filter(expense_type == "total_expenditure")


tot_exp %>% 
  ggplot(data=.) +
  geom_point(aes(x=start_year, y=amount), alpha=0.1) +
  facet_wrap(~inner, scales="free") + 
  stat_smooth(aes(x=start_year, y=amount), method="lm")
