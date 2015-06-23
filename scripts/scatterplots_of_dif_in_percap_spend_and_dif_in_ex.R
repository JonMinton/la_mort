

# Script to do simple script at regional level only

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
require(lattice)
#statistical analysis
require(lme4)



# data 

per_cap_spend <- read_csv("data/tidied/per_capita_spend_by_region_and_la_class.csv")
per_cap_spend <- per_cap_spend %>% filter(!(ons_code %in% c("E06000053", "E09000001")))

ex <- read_csv("data/tidied/england_ex.csv")
ex <- ex %>% filter(!(la %in% c("E06000053", "E09000001")))

# T = 0 if year == 2009 or year == 2010
# T = 1 if year == 2012 or year == 2013

per_cap_spend$trt  <- NA
per_cap_spend$trt[per_cap_spend$year %in% c(2009, 2010)] <- 0
per_cap_spend$trt[per_cap_spend$year %in% c(2012, 2013)] <- 1

ex$trt <- NA
ex$trt[ex$year %in% c(2009, 2010)] <- 0
ex$trt[ex$year %in% c(2012, 2013)] <- 1


ex_simple <- ex %>% 
  filter(!is.na(trt)) %>% 
  group_by(sex, la, trt) %>% 
  summarise(
    e50 = mean(e50),
    e65 = mean(e65),
    e80 = mean(e80)
    )

change_in_ex <- ex_simple  %>% 
  gather(key=etype, value=le, -sex, -la, -trt)  %>% 
  unite(col="sex_ex", sex, etype)   %>% 
  spread(key=trt, value=le)  %>% 
  mutate(change_in_ex = `1` - `0`) %>% 
  select(-`1`, -`0`)


pcs_simple <- per_cap_spend %>% 
  filter(!is.na(trt)) %>% 
  select(-region, -class, -year) %>% 
  group_by(ons_code, trt) %>% 
  summarise_each(funs(mean))

change_in_pcs <- pcs_simple  %>% 
  gather(key="spend_type", value="pcs", -ons_code, -trt)  %>% 
  spread(key="trt", value="pcs")  %>% 
  mutate(dif_in_pcs = `1` - `0`)  %>% 
  select(-`0`, -`1`)

joined_changes <- inner_join(change_in_ex, change_in_pcs, by=c("la" = "ons_code"))

joined_changes %>% ggplot() +
  geom_point(aes(x=dif_in_pcs, y=change_in_ex)) + 
  facet_grid(sex_ex ~ spend_type, scale="free_x") +
  stat_smooth(aes(x=dif_in_pcs, y=change_in_ex), method="lm")



dta_pop_counts %>% left_join(tmp, by=c("lad2013_code" = "ons_code"))

