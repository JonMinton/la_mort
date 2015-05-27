

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

data <- read.csv(file="data/tidied/cumulative_surv_by_la.csv")  %>% 
  tbl_df

fn <- function(X){
  this_year <- X$year[1]
  this_la  <- X$la[1]
  this_sex <- X$sex[1]
  
  X <- X %>%
    mutate(adj_cmr = (deaths + 0.5) / (population + 0.5))

  
  c_35 <- X %>%
    filter(age >=35) %>%
    lm(log(adj_cmr) ~ age, data=.) %>%
    coef(.)  %>% .["age"] %>%
    as.numeric
  
  c_50 <- X %>%
    filter(age >=50) %>%
    lm(log(adj_cmr) ~ age, data=.) %>%
    coef(.) %>% .["age"] %>%
    as.numeric

  c_65 <- X %>%
    filter(age >=65) %>%
    lm(log(adj_cmr) ~ age, data=.) %>%
    coef(.) %>% .["age"] %>%
    as.numeric

  c_80 <- X %>%
    filter(age >=80) %>%
    lm(log(adj_cmr) ~ age, data=.) %>%
    coef(.) %>% .["age"] %>%
    as.numeric
  
  output <- data.frame(
    la = this_la, 
    year = this_year,
    sex = this_sex,
    c_35, c_50, c_65, c_80
    )
  return(output)
}


dta <- ddply(data, .(la, year, sex), fn, .progress="text")

write.csv(dta, file="data/tidied/coeffs_on_age_by_la.csv", row.names=F)


fn <- function(X){
  this_year <- X$year[1]
  this_la  <- X$la[1]
  this_sex <- X$sex[1]
  
  X <- X %>%
    mutate(adj_cmr = (deaths + 0.5) / (population + 0.5))
  
  s_35 <- X %>%
    filter(age >=35) %>%
    lm(log(adj_cmr) ~ age, data=.) %>%
    summary(.)  %>% .$coefficients %>% 
    .["age","Std. Error"]
    
  s_50 <- X %>%
    filter(age >=50) %>%
    lm(log(adj_cmr) ~ age, data=.) %>%
    summary(.)  %>% .$coefficients %>% 
    .["age","Std. Error"]
  
  s_65 <- X %>%
    filter(age >=65) %>%
    lm(log(adj_cmr) ~ age, data=.) %>%
    summary(.)  %>% .$coefficients %>% 
    .["age","Std. Error"]
  
  s_80 <- X %>%
    filter(age >=80) %>%
    lm(log(adj_cmr) ~ age, data=.) %>%
    summary(.)  %>% .$coefficients %>% 
    .["age","Std. Error"]
  
  output <- data.frame(
    la = this_la, 
    year = this_year,
    sex = this_sex,
    s_35, s_50, s_65, s_80
  )
  return(output)
}


dta <- ddply(data, .(la, year, sex), fn, .progress="text")

write.csv(dta, file="data/tidied/ses_on_coeffs_on_age_by_la.csv", row.names=F)

# e65 compared with change in e65  ----------------------------------------


dta <- read.csv("data/tidied/england_ex.csv") %>%
  tbl_df

dta  %>% 
  group_by(sex, la)  %>% 
  arrange(year)  %>% 
  mutate(e65_change = e65 - lag(e65))  %>% 
  filter(year >=2003) %>%
  ggplot(data=.) + 
  geom_point(aes(x=e65, y=e65_change, group=sex, colour=sex), alpha=0.2) + 
  facet_wrap(~year) + 
  geom_smooth(aes(x=e65, y=e65_change, group=sex, colour=sex), size=1.2, method="lm") + 
  geom_hline(xintercept=0, linetype="dashed") +
  coord_cartesian(ylim=c(-2.5,2.5)) +
  labs(x="life expectancy at age 65",y="change in life expectancy\nat age 65 from previous year")

ggsave(file="figures/delta_e65_vs_e65.png",
       height=20, width=20, units="cm", dpi=300 
       )


dta  %>% 
  group_by(sex, la)  %>% 
  arrange(year)  %>% 
  mutate(e80_change = e80 - lag(e80))  %>% 
  filter(year >=2003) %>%
  ggplot(data=.) + 
  geom_point(aes(x=e80, y=e80_change, group=sex, colour=sex), alpha=0.2) + 
  facet_wrap(~year) + 
  geom_smooth(aes(x=e80, y=e80_change, group=sex, colour=sex), size=1.2, method="lm") + 
  geom_hline(xintercept=0, linetype="dashed") +
  coord_cartesian(ylim=c(-2.5,2.5)) +
  labs(x="life expectancy at age 80",y="change in life expectancy\nat age 80 from previous year")

ggsave(file="figures/delta_e80_vs_e80.png",
       height=20, width=20, units="cm", dpi=300 
)



dta  %>% 
  group_by(sex, la)  %>% 
  arrange(year)  %>% 
  mutate(e50_change = e50 - lag(e50))  %>% 
  filter(year >=2003) %>%
  ggplot(data=.) + 
  geom_point(aes(x=e50, y=e50_change, group=sex, colour=sex), alpha=0.2) + 
  facet_wrap(~year) + 
  geom_smooth(aes(x=e50, y=e50_change, group=sex, colour=sex), size=1.2, method="lm") + 
  geom_hline(xintercept=0, linetype="dashed") +
  coord_cartesian(ylim=c(-2.5,2.5)) +
  labs(x="life expectancy at age 50",y="change in life expectancy\nat age 50 from previous year")

ggsave(file="figures/delta_e50_vs_e50.png",
       height=20, width=20, units="cm", dpi=300 
)



# Gradients of change ? ---------------------------------------------------

fn <- function(x){
  tmp <- lm(e50_change ~ e50, data=x)
  out <- as.double(coefficients(tmp)["e50"])
  return(out)
}

coeffs_e50 <- dta %>%
  group_by(sex, la) %>%
  arrange(year) %>%
  mutate(e50_change = e50-lag(e50)) %>%
  filter(year >=2003) %>%
  ungroup %>%
  ddply(., .(sex, year), fn)


coeffs_e50 %>%  
  rename(coef = V1) %>%
  ggplot(data=.) +
  geom_bar(aes(x=year, y=coef), stat="identity") + 
  facet_wrap(~ sex) + 
  labs(x="Year", y="Coefficient on gradient for change in e50") 

ggsave(file="figures/coef_gradient_e50.png",
       height=10, width=15, dpi=300, unit="cm"
)


fn <- function(x){
  tmp <- lm(e65_change ~ e65, data=x)
  out <- as.double(coefficients(tmp)["e65"])
  return(out)
}

coeffs_e65 <- dta %>%
  group_by(sex, la) %>%
  arrange(year) %>%
  mutate(e65_change = e65-lag(e65)) %>%
  filter(year >=2003) %>%
  ungroup %>%
  ddply(., .(sex, year), fn)

coeffs_e65 %>%  
  rename(coef = V1) %>%
  ggplot(data=.) +
  geom_bar(aes(x=year, y=coef), stat="identity") + 
  facet_wrap(~ sex) + 
  labs(x="Year", y="Coefficient on gradient for change in e65") 

ggsave(file="figures/coef_gradient_e65.png",
       height=10, width=15, dpi=300, unit="cm"
)

            
fn <- function(x){
  tmp <- lm(e80_change ~ e80, data=x)
  out <- as.double(coefficients(tmp)["e80"])
  return(out)
}

coeffs_e80 <- dta %>%
  group_by(sex, la) %>%
  arrange(year) %>%
  mutate(e80_change = e80-lag(e80)) %>%
  filter(year >=2003) %>%
  ungroup %>%
  ddply(., .(sex, year), fn)

coeffs_e80 %>%  
  rename(coef = V1) %>%
  ggplot(data=.) +
  geom_bar(aes(x=year, y=coef), stat="identity") + 
  facet_wrap(~ sex) + 
  labs(x="Year", y="Coefficient on gradient for change in e80") 

ggsave(file="figures/coef_gradient_e80.png",
       height=10, width=15, dpi=300, unit="cm"
)


