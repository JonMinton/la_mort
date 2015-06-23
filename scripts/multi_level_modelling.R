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
#mod_laclass_intercept <- e50_only %>% filter(sex == "male") %>% lmer(e50 ~ year + (1 | class) , data = . , REML=F)

# > BIC(mod_region_intercept)
# [1] 1836.939
# > BIC(mod_laclass_intercept)
# [1] 1903.903
# mod_regionlaclass_intercept <- e50_only %>% filter(sex=="male") %>% lmer(e50 ~ year + (1 | region) + (1 | class) , data = .)
# > BIC(mod_regionlaclass_intercept)
# [1] 1840.041

# This suggests that the region intercept only model is better than the two alternatives (perhaps...)

# mod_region_interslope <- e50_only %>% filter(sex=="male") %>% lmer(e50 ~ year + (year | region), data = .)
# The model with a slope does not converge 

mod_region2_intercept_m <- e50_only %>% filter(sex=="male") %>% lmer(e50 ~ poly(year, 2) + (1 | region), data = ., REML=F)
mod_region2_intercept_f <- e50_only %>% filter(sex=="female") %>% lmer(e50 ~ poly(year, 2) + (1 | region), data = ., REML=F)


#mod_01 <- e50_only  %>% lmer(e50 ~ year + sex + (1  | region), REML=F)
mod_02 <- e50_only  %>% lmer(e50 ~ year + sex + (1  | region / la), data = ., REML=F)
mod_03 <- e50_only  %>% lmer(e50 ~ year + (1  | region / la) + (1 | sex), data = ., REML=F)
#mod_04 <- e50_only  %>% lmer(e50 ~ year + (1  | region / la) + (1  + year | sex), data = ., REML=F)
#mod_05 <- e50_only  %>% lmer(e50 ~ year + (1  | region / la) + (year | sex), data = ., REML=F)
mod_06 <- e50_only  %>% lmer(e50 ~ poly(year, 2) + (1  | region / la) + (1 + poly(year, 2) | sex), data = ., REML=F)
mod_07 <- e50_only  %>% lmer(e50 ~ poly(year, 2) + (1  | sex / region / la ) , data = ., REML=F)
mod_08 <- e50_only  %>% lmer(e50 ~ poly(year, 2) + (1  | region / la /sex) , data = ., REML=F)
mod_09 <- e50_only  %>% lmer(e50 ~ poly(year, 2) + (1  | region / la) + (1 + poly(year, 2) | sex), data = ., REML=F)
mod_10 <- e50_only  %>% lmer(e50 ~ poly(year, 2)*sex + (1  | region / la) + (1 + poly(year, 2) | sex), data = ., REML=F)
mod_11 <- e50_only  %>% lmer(e50 ~ poly(year, 2)*sex + (1  | region / la), data = ., REML=F)
mod_12 <- e50_only  %>% lmer(e50 ~ poly(year, 2) + sex + (1  | region / la), data = ., REML=F)
mod_13 <- e50_only  %>% lmer(e50 ~ poly(year, 3)*sex + (1  | region / la), data = ., REML=F)
mod_14 <- e50_only  %>% lmer(e50 ~ poly(year, 2)*sex + (1  | region / la) + (1 | class / la), data = ., REML=F)

# df      BIC
# mod_02  6 1869.070
# mod_03  6 1884.580
# mod_04  8 1899.468
# mod_06 12 1857.893
# mod_07  7 2055.752
# mod_08  7 2447.302
# mod_09 12 1857.893
# mod_10 15 1863.413
# mod_11  9 1820.725
# mod_12  7 1835.690
# mod_13 11 1828.312
# using mod_11 as it has the best fit

rm(list=ls(pattern=("mod")))
mod_trend <- e50_only  %>% lmer(e50 ~ poly(year, 2)*sex + (1  | region / la), data = ., REML=F)

dta_joined <- per_cap_spend  %>% inner_join(ex, by=c("ons_code" = "la", "year"="year"))

mod_trend_spend_01 <- dta_joined  %>% lmer(e50 ~ poly(year, 2)*sex + adults_65_pc + (1  | region / ons_code), data = ., REML=F)
mod_trend_spend_02 <- dta_joined  %>% lmer(e50 ~ poly(year, 2)*sex + adults_u65_social_care_pc + (1  | region / ons_code), data = ., REML=F)
mod_trend_spend_03 <- dta_joined  %>% lmer(e50 ~ poly(year, 2)*sex + adults_u65_physical_pc + (1  | region / ons_code), data = ., REML=F)

dta_joined  %>% filter(sex=="male") %>% 
  lmer(e50 ~ poly(year, 2) + adults_u65_social_care_pc + (1  | region / ons_code), data = ., REML=F) %>% 
  summary


dta_joined  %>% filter(sex=="female") %>% 
  lmer(e50 ~ poly(year, 1) + adults_u65_social_care_pc + (1  | region / ons_code), data = ., REML=F) %>% 
  summary

dta_joined  %>% filter(sex=="female") %>% 
  lmer(e50 ~ poly(year, 2) + adults_u65_physical_pc + (1  | region / ons_code), data = ., REML=F) %>% 
  summary

dta_joined  %>% filter(sex=="male") %>% 
  lmer(e50 ~ poly(year, 2) + adults_u65_physical_pc + (1  | region / ons_code), data = ., REML=F) %>% 
  summary

# to do separately by gender

mod_trend_spend_01_e50_m <- dta_joined %>% 
  filter(sex=="male") %>% 
  lmer(e50 ~ poly(year, 1) + adults_65_pc + (1  | region / ons_code), data = ., REML=F)

mod_trend_spend_01_e50_f <- dta_joined %>% 
  filter(sex=="female") %>% 
  lmer(e50 ~ poly(year, 3) + adults_65_pc + (1  | region / ons_code), data = ., REML=F)

# Now to include changes in spend 

# What about for e65 ? 

mod_trend_spend_e65_01_m <- dta_joined %>% 
  filter(sex=="male") %>% 
  lmer(e65 ~ poly(year, 2) + adults_65_pc + (1  | region / ons_code), data = ., REML=F)

mod_trend_spend_e_65_01_f <- dta_joined %>% 
  filter(sex=="female") %>% 
  lmer(e65 ~ poly(year, 2) + adults_65_pc + (1  | region / ons_code), data = ., REML=F)


mod_trend_spend_e80_01_m <- dta_joined %>% 
  filter(sex=="male") %>% 
  lmer(e80 ~ poly(year, 2) + adults_65_pc + (1  | region / ons_code), data = ., REML=F)

mod_trend_spend_e80_01_f <- dta_joined %>% 
  filter(sex=="female") %>% 
  lmer(e80 ~ poly(year, 2) + adults_65_pc + (1  | region / ons_code), data = ., REML=F)



dta_joined %>% 
  filter(sex=="male") %>% 
  lmer(e65 ~ poly(year, 2) + adults_65_pc + ons_code + (1  | region), data = ., REML=F) %>% 
  summary

dta_joined %>% 
  filter(sex=="male") %>% 
  lm(e65 ~ year + adults_65_pc + ons_code, data = .) %>% 
  summary


dta_joined %>% 
  filter(sex=="male") %>% 
  lm(e65 ~ year + adults_65_pc + region + class, data = .) %>% 
  summary
