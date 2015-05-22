

# Title: local authority mortality rate exploration -----------------------


rm(list=ls())



# pre-requisites ----------------------------------------------------------


require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

require(ggplot2)
require(lattice)


# Create tidy data (Run once only) --------------------------------------------------------------------
# 
# dta <- read.csv("data/unzipped/MYEB2_detailed_components_of_change_series_EW_(0213).csv") %>%
#   tbl_df
# 
# 
# data_tidied <- dta  %>% 
#   gather(key=label, value=count, -lad2013_code, -lad2013_name, -country, -sex, -age)  %>% 
#   mutate(
#     year = str_extract(label, "\\d{1,4}"), 
#     category = str_replace(str_extract(label, "\\D*"), "_$", ""),
#     sex=ifelse(sex==1, "male", "female"))   %>% 
#   select(lad2013_code, country, sex, age, year, category, count) %>%
#   spread(category, count)
# 
# write.csv(data_tidied, file="data/tidied/england_la_count.csv", row.names=FALSE)
# 

# e50, e65 and e80 by la, year and age ------------------------------------


data <- read.csv("data/tidied/england_la_count.csv") %>%
  tbl_df

ex <- data  %>% 
  select(la=lad2013_code, year, sex, age, deaths, population)  %>% 
  group_by(sex, la, year)  %>% 
  summarise(
    e50=sum(age[age>=50]*deaths[age>=50])/sum(deaths[age>=50]), 
    e65=sum(age[age>=65]*deaths[age>=65])/sum(deaths[age>=65]), 
    e80=sum(age[age>=80]*deaths[age>=80])/sum(deaths[age>=80])
    )

write.csv(ex, file="data/tidied/england_ex.csv", row.names=FALSE)


# Cumulative survival from ages 50, 65 and 80 -----------------------------


fn <- function(X){
  out <- X
  out <- out %>% mutate(cmr = deaths/population)
  
  out$surv <- NA
  out$surv[out$age==0] <- 1
  for (i in 1:max(out$age)){
    out$surv[out$age==i] <- out$surv[out$age == i - 1 ] * (1 -  out$cmr[out$age == i - 1])
  }    
  return(out)
}

dta <- data %>%
  select(la=lad2013_code, year, sex, age, deaths, population)  %>% 
  filter(!is.na(deaths)) %>%
  group_by(la, sex, year) %>%
  arrange(age) %>%
  do(fn(.))


write.csv(dta, file="data/tidied/cumulative_surv_by_la.csv", row.names=F)



# conditional median les --------------------------------------------------


data <- read.csv("data/tidied/cumulative_surv_by_la.csv") %>%
  tbl_df

med_le_0 <- data  %>% 
  group_by(la, year, sex)  %>% 
  mutate(tmp2 = lag(surv))  %>% 
  mutate(flag=ifelse(surv < 0.5 & tmp2 > 0.5, T, F))  %>% 
  summarise(med_le_0 = age[flag==T][1])  

med_le_50 <- data %>%
  group_by(la, year, sex) %>%
  mutate(surv = surv/surv[age==50]) %>%
  mutate(tmp2 = lag(surv))  %>% 
  mutate(flag=ifelse(surv < 0.5 & tmp2 > 0.5, T, F))  %>% 
  summarise(med_le_50 = age[flag==T][1])  

med_le_65 <- data %>%
  group_by(la, year, sex) %>%
  mutate(surv = surv/surv[age==65]) %>%
  mutate(tmp2 = lag(surv))  %>% 
  mutate(flag=ifelse(surv < 0.5 & tmp2 > 0.5, T, F))  %>% 
  summarise(med_le_65 = age[flag==T][1])  

med_le_80 <- data %>%
  group_by(la, year, sex) %>%
  mutate(surv = surv/surv[age==80]) %>%
  mutate(tmp2 = lag(surv))  %>% 
  mutate(flag=ifelse(surv < 0.5 & tmp2 > 0.5, T, F))  %>% 
  summarise(med_le_80 = age[flag==T][1])  



med_le <- med_le_0 %>%
  full_join(med_le_50) %>%
  full_join(med_le_65) %>%
  full_join(med_le_80)

write.csv(med_le, file="data/tidied/median_cond_le_by_la.csv", row.names=F)




# regression coeffs -------------------------------------------------------


