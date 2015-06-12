# Title: local authority mortality rate exploration -----------------------

rm(list=ls())


# pre-requisites ----------------------------------------------------------


require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

require(ggplot2)
require(lattice)

require(readxl)

# Data on mort in LAs -----------------------------------------------------


data <- read.csv("data/tidied/england_la_count.csv") %>%
  tbl_df

# Remove Isle of Scilly (E06000053)
# and City of London (E09000001)
# as populations sizes are very small

data <- data %>% 
  filter(
    !(lad2013_code %in% c("E06000053", "E09000001"))
  )

ex <- data  %>% 
  select(la=lad2013_code, year, sex, age, deaths, population)  %>% 
  group_by(sex, la, year)  %>% 
  summarise(
    e50=sum(age[age>=50]*deaths[age>=50])/sum(deaths[age>=50]), 
    e65=sum(age[age>=65]*deaths[age>=65])/sum(deaths[age>=65]), 
    e80=sum(age[age>=80]*deaths[age>=80])/sum(deaths[age>=80])
  )



# Data on expenditure -----------------------------------------------------

raw_08_09 <- read_excel(
  "data/care_cuts/Expenditure/RO3/Revenue_Outturn__RO3__data_2008-09_by_LA_-_Revised_17-Nov-2011.xls", 
  sheet="RO3 LA Data 2008-09", 
  skip = 3, 
  col_names = FALSE
  )

raw_09_10 <- read_excel(
  "data/care_cuts/Expenditure/RO3/RO3 [Revenue Outturn RO3 data 2009-10 by LA nd].xls",
  skip = 4,
  sheet="RO3 LA Data 2009-10",
  col_names= FALSE
)

raw_10_11 <- read_excel(
  "data/care_cuts/Expenditure/RO3/Revenue_Outturn__RO3__data_2010-11_by_LA_-_27-Nov-2012-v2.xls",
  sheet= "RO3 LA Data 2010-11",
  skip = 4,
  col_names = FALSE
)

raw_11_12 <- read_excel(
  "data/care_cuts/Expenditure/RO3/Revenue_Outturn__RO3__data_2011-12_by_LA_-_Revised_28-Nov-2013.xls",
  sheet= "RO3 LA Data 2011-12",
  skip = 4,
  col_names = FALSE
)

raw_12_13 <- read_excel(
  "data/care_cuts/Expenditure/RO3/Revenue_Outturn__RO3__data_2012-13_by_LA__Revised__-_18-Feb-2014.xls",
  sheet= "RO3 LA Data 2012-13",
  skip = 4,
  col_names = FALSE
)

raw_12_13 <- read_excel(
  "data/care_cuts/Expenditure/RO3/Revenue_Outturn__RO3__data_2012-13_by_LA__Revised__-_18-Feb-2014.xls",
  sheet= "RO3 LA Data 2012-13",
  skip = 4,
  col_names = FALSE
)

raw_13_14a <- read_excel(
  "data/care_cuts/Expenditure/RO3/Revenue_Outturn__RO3__data_2013-14_by_LA.xls",
  sheet= "RO3 LA Data 2013-14 (1)",
  col_names = FALSE
)

raw_13_14b <- read_excel(
  "data/care_cuts/Expenditure/RO3/Revenue_Outturn__RO3__data_2013-14_by_LA.xls",
  sheet= "RO3 LA Data 2013-14 (2)",
  col_names = FALSE
)

raw_13_14c <- read_excel(
  "data/care_cuts/Expenditure/RO3/Revenue_Outturn__RO3__data_2013-14_by_LA.xls",
  sheet= "RO3 LA Data 2013-14 (3)",
  col_names = FALSE
)


# For each col, want to split rows into metadata part and data part

split_in_two <- function(x, split_row){
  comment_part <- x[1:split_row, ]
  data_part <- x[(split_row+1):dim(x)[1],]
  
  fn <- function(xx){
    yy <- paste(xx, collapse= " ")
    yy <- yy %>% 
      str_replace_all(. , pattern = "NA", "") %>%
      str_trim()
    
    return(yy)
  }
  
  comment_part <- apply(comment_part, 2, fn)
  
  output <- data_part
  names(output) <- comment_part
  
  return(output)

}

#

mid_08_09 <- split_in_two(raw_08_09, 7)
mid_09_10 <- split_in_two(raw_09_10, 7)
mid_10_11 <- split_in_two(raw_10_11, 7)
mid_11_12 <- split_in_two(raw_11_12, 7)
mid_12_13 <- split_in_two(raw_12_13, 7)
mid_13_14a <- split_in_two(raw_13_14a, 14)
mid_13_14b <- split_in_two(raw_13_14b, 14)
mid_13_14c <- split_in_two(raw_13_14c, 14)


long_08 <- mid_08_09 %>% 
  rename(ecode =    `£ thousand     E-code`) %>% 
  select( - `Local authority`, -Class) %>% 
  gather(key="type", value="amount", -ecode) %>% 
  mutate(start_year = 2008) %>% 
  select(start_year, ecode, type, amount)


long_09 <- mid_09_10 %>% 
  gather(key="type", value="amount", -`E-code`) %>% 
  rename(ecode = `E-code`) %>% 
  filter(!(type %in% c("Local authority", "Class", "Region"))) %>% 
  mutate(start_year = 2009) %>% 
  select(start_year, ecode, type, amount)

long_10 <- mid_10_11 %>% 
  gather(key="type", value="amount", -`E-code`) %>% 
  rename(ecode = `E-code`) %>% 
  filter(!(type %in% c("Local authority", "Class"))) %>% 
  mutate(start_year = 2010) %>% 
  select(start_year, ecode, type, amount)

long_11 <-  mid_11_12 %>% 
  gather(key="type", value="amount", -`E-code`) %>% 
  rename(ecode = `E-code`) %>% 
  filter(!(type %in% c("Local authority", "Class"))) %>% 
  mutate(start_year = 2011) %>% 
  select(start_year, ecode, type, amount)

long_12 <- mid_12_13 %>% 
  gather(key="type", value="amount", -`E-code`) %>% 
  rename(ecode = `E-code`) %>% 
  filter(!(type %in% c("Local authority", "Class"))) %>% 
  mutate(start_year = 2012) %>% 
  select(start_year, ecode, type, amount)

long_13 mid_13_14a

# for each of the above, am interested in number of employees and total expenditure in adult social services 

tidy_08_09 <- mid_08_09 %>% 
  select(
    `ecode` = `£ thousand     E-code`,
    `children` = `Social Care strategy - children   TOTAL EXPENDITURE (Col 1+2)`,
    `older` = `Older people (aged 65 or over) including older mentally ill    TOTAL EXPENDITURE (Col 1+2)`,
    `total` = `TOTAL SOCIAL CARE*   TOTAL EXPENDITURE (Col 1+2)`
    ) %>% tbl_df


tidy_09_10 <- mid_09_10[, c(1, 7, 34, 43)] %>% tbl_df
names(tidy_09_10) <- c("ecode", "children", "adult", "older")

tidy_10_11 <- mid_10_11[, c(1, 6, 33, 42)] %>% tbl_df
names(tidy_10_11) <- c("ecode", "children", "adult", "older")

tidy_11_12 <- mid_11_12[,c(1, 6, 78, 87)] %>% tbl_df
names(tidy_11_12) <- c("ecode", "children", "adult", "older")


tidy_12_13 <- mid_12_13[, c(1, 6, 87, 96)] %>% tbl_df
names(tidy_12_13) <- c("ecode", "children", "adult", "older")

tidy_13_14 <- mid_13_14a[,c(1, 6, 33, 36)] %>% tbl_df
names(tidy_13_14) <- c("ecode", "children", "adult", "older")



tidy_08_09 <- tidy_08_09 %>% 
  mutate_each(funs(as.numeric), -ecode) %>% 
  mutate(start_year = 2008)

tidy_08_09 <- tidy_08_09 %>% 
  mutate(adult = total - older - children) %>% 
  select(start_year, ecode, children, adult, older)

tidy_09_10 <- tidy_09_10 %>% 
  mutate_each(funs(as.numeric), -ecode) %>% 
  mutate(start_year = 2009) %>% 
  select(start_year, ecode, children, adult, older)


tidy_10_11 <- tidy_10_11 %>% 
  mutate_each(funs(as.numeric), -ecode) %>% 
  mutate(start_year = 2010) %>% 
  select(start_year, ecode, children, adult, older)


tidy_11_12 <- tidy_11_12 %>% 
  mutate_each(funs(as.numeric), -ecode) %>% 
  mutate(start_year = 2011) %>% 
  select(start_year, ecode, children, adult, older)

tidy_12_13 <- tidy_12_13 %>% 
  mutate_each(funs(as.numeric), -ecode) %>% 
  mutate(start_year = 2012) %>% 
  select(start_year, ecode, children, adult, older)

tidy_13_14 <- tidy_13_14 %>% 
  mutate_each(funs(as.numeric), -ecode) %>% 
  mutate(start_year = 2013) %>% 
  select(start_year, ecode, children, adult, older)


tidy_ro3 <- tidy_08_09 %>% 
  bind_rows(tidy_09_10) %>% 
  bind_rows(tidy_10_11) %>% 
  bind_rows(tidy_11_12) %>% 
  bind_rows(tidy_12_13) %>% 
  bind_rows(tidy_13_14)



la_codes <- read.csv("data/support/lookups_between_ons_and_ecodes.csv") %>% tbl_df
  
tidy_linked_ro3 <- la_codes %>% 
  select(ecode, ons_code, ons_region_name)  %>% 
  left_join(tidy_ro3) %>% 
  filter(str_detect(ons_code, "^E"))


write.csv(tidy_linked_ro3, file="data/tidied/linked_r03.csv", row.names=F)




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

