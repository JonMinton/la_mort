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

dta <- dta %>% filter(outer =="Adults" & 
                        inner %in% 
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
tot_exp <- tot_exp %>% select(ons_code, year= start_year, inner, amount) %>% 
  group_by(ons_code, year, inner) %>% summarise(amount=sum(amount)) %>% 
  ungroup

# Now to make per (appropriate) capita
dta_pop_counts <- read_csv("data/tidied/england_la_count.csv")

pop_by_broad_age_group <- dta_pop_counts %>% 
  select(ons_code = lad2013_code, sex, age, year, population) %>%
  group_by(ons_code, year) %>% 
  summarise(
    pop_0_17 = sum(population[age >= 0 & age < 17]),
    pop_18_64 = sum(population[age >= 18 & age <= 64]),
    pop_50_64 = sum(population[age >= 50 & age <= 64]),
    pop_65_plus = sum(population[age >=65]),
    pop_80_plus = sum(population[age >=80])
  )

tot_exp <- tot_exp %>% 
  spread(key="inner", value = "amount")

per_cap_spend <- tot_exp %>% inner_join(pop_by_broad_age_group) %>% 
  mutate(
    adults_65_pc = 1000 * adults_65plus / pop_65_plus,
    adults_us6_learning_pc = 1000 * adults_u65_learning / pop_18_64,
    adults_u65_mental_pc = 1000 * adults_u65_mental / pop_18_64,
    adults_u65_other_pc = 1000 * adults_u65_other / pop_18_64,
    adults_u65_physical_pc = 1000 * adults_u65_physical / pop_18_64,
    adults_u65_social_care_pc = 1000 * adults_u65_social_care / pop_18_64
) %>% select(ons_code, year, contains("pc"))

per_cap_spend %>% 
  gather(key="type", value="amount", -ons_code, -year) %>% 
  group_by(year, type) %>% summarise(
    amount_upper = mean(amount) + 2 * sd(amount),
    amount_lower = mean(amount) - 2 * sd(amount),
    amt_mean = mean(amount),
    amt_median = median(amount),
    amt_lq = quantile(amount, 0.025),
    amt_uq = quantile(amount, 0.975)
    ) %>% ggplot(.) +
  geom_line(aes(x=year, y=amt_median)) + 
  facet_wrap(~ type, scales="free")

per_cap_spend %>% 
  gather(key="type", value="amount", -ons_code, -year) %>% 
  ggplot(data = ., aes(x=year, y=amount)) +
  geom_line(aes(group=ons_code), alpha=0.1) + 
  geom_smooth() + 
  facet_wrap(~ type, scales="free") 

fn <- function(X){
  out <- X %>% 
    ggplot(data = ., aes_string(x="year", y="amount")) +
    geom_violin(aes(group=year), colour=NA, fill="grey") + 
    stat_smooth(fill="lightblue") + 
    labs(title=X$type[1])
  
    return(out)
}

figs <- per_cap_spend %>% 
  gather(key="type", value="amount", -ons_code, -year) %>% 
  dlply(., .(type), fn)


