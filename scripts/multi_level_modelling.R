# Exploration of the effect at local authority level on ex given changes 
# in per capita spend at region and locaul authority type level 

rm(list= ls())


# pre reqs ----------------------------------------------------------------


# data loading
require(readr)
require(readxl)


#data management
require(stringr)
require(plyr)
require(tidyr)
require(dplyr)

#data visualisation
require(ggplot2)

#statistical analysis
require(lme4)



# data 

per_cap_spend <- read_csv("data/tidied/per_capita_spend_by_region_and_la_class.csv")

ex <- read_csv("data/tidied/england_ex.csv")

# Remove Isle of Scilly  (E06000053) and City of London (E09000001)
ex <- ex %>% filter(!(la %in% c("E06000053", "E09000001")))

# Analyses ----------------------------------------------------------------

#For each gender separately 
#Want to calculate average change in e50 from 2009 to 2013

mod_year <- ex %>% filter(sex=="male") %>% lm(e50 ~ year, data=.)

# link ex to region and to la type

tmp <- per_cap_spend %>% select(la = ons_code, region, class) %>% distinct
e50_only <- ex %>% 
  left_join(tmp) %>% 
  select(sex, la, year, region, class, e50) %>% filter(year > 2008)

mod_region_intercept <- e50_only %>% filter(sex=="male") %>% lmer(e50 ~ year + (1 | region), data = ., REML=F)
mod_laclass_intercept <- e50_only %>% filter(sex == "male") %>% lmer(e50 ~ year + (1 | class) , data = .)

> BIC(mod_region_intercept)
[1] 1836.939
> BIC(mod_laclass_intercept)
[1] 1903.903

mod_regionlaclass_intercept <- e50_only %>% filter(sex=="male") %>% lmer(e50 ~ year + (1 | region) + (1 | class) , data = .)

> BIC(mod_regionlaclass_intercept)
[1] 1840.041

# This suggests that the region intercept only model is better than the two alternatives (perhaps...)

mod_region_interslope <- e50_only %>% filter(sex=="male") %>% lmer(e50 ~ year + (year | region), data = .)
