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

# adjust amounts for wage inflation
wage_deflator <-  read_csv(file="data/care_cuts/expenditure/wage_deflator.csv")
dta <- dta %>% left_join(wage_deflator, by=c("start_year" = "year") ) %>% 
  mutate(amount = amount / deflator) %>% select(-tax_year_average, -deflator)

ecode_lookup <- read_excel("data/care_cuts/4_digit_code_extract.xlsx", sheet="Sheet1")
ecode_lookup <- ecode_lookup  %>% 
  rename(
    ecode = `E-code`, 
    local_authority = `Local authority`, 
    region= Region, class= Class)



dta <- dta %>% inner_join(ecode_lookup) %>% filter(class !="SD")


tot_exp <- dta %>% filter(expense_type == "total_expenditure") %>% select(-expense_type)
tot_exp <- tot_exp %>% select(ons_code, year= start_year, inner, amount, region, class) %>% 
  group_by(ons_code, year, inner, region, class) %>% summarise(amount=sum(amount)) %>% 
  ungroup

tot_exp <- tot_exp %>% filter(class !="O")

# Now to make per (appropriate) capita
dta_pop_counts <- read_csv("data/tidied/england_la_count.csv")

# Need SC as well, for this need another link
ons_link <- read_csv(file="data/support/LAD12_CTY12_EN_LU.csv")

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

pop_by_county <- dta_pop_counts %>% inner_join(ons_link, by=c("lad2013_code"="LAD12CD"))

pop_by_broad_age_group_county <- pop_by_county %>% 
  select(ons_code=CTY12CD, sex, age, year, population) %>% 
  group_by(ons_code, year) %>% 
  summarise(
    pop_0_17 = sum(population[age >= 0 & age < 17]),
    pop_18_64 = sum(population[age >= 18 & age <= 64]),
    pop_50_64 = sum(population[age >= 50 & age <= 64]),
    pop_65_plus = sum(population[age >=65]),
    pop_80_plus = sum(population[age >=80])
  )

pop_by_broad_age_group <- pop_by_broad_age_group %>% 
  bind_rows(pop_by_broad_age_group_county)

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
) %>% select(ons_code, year, region, class, contains("pc"))


write_csv(per_cap_spend, path="data/tidied/per_capita_spend_by_region_and_la_class.csv")

per_cap_spend_region <- tot_exp %>% inner_join(pop_by_broad_age_group) %>%
  group_by(year, region) %>% 
  summarise(
    adults_65_pc = 1000 * sum(adults_65plus) / sum(pop_65_plus),
    adults_us6_learning_pc = 1000 * sum(adults_u65_learning) / sum(pop_18_64),
    adults_u65_mental_pc = 1000 * sum(adults_u65_mental) / sum(pop_18_64),
    adults_u65_other_pc = 1000 * sum(adults_u65_other) / sum(pop_18_64),
    adults_u65_physical_pc = 1000 * sum(adults_u65_physical) / sum(pop_18_64),
    adults_u65_social_care_pc = 1000 * sum(adults_u65_social_care) / sum(pop_18_64)
  ) %>% select(year, region, contains("pc"))

write_csv(per_cap_spend_region, path="data/tidied/per_capita_spend_aggregated_to_region.csv")

per_cap_spend_class <- tot_exp %>% inner_join(pop_by_broad_age_group) %>%
  group_by(year, class) %>% 
  summarise(
    adults_65_pc = 1000 * sum(adults_65plus) / sum(pop_65_plus),
    adults_us6_learning_pc = 1000 * sum(adults_u65_learning) / sum(pop_18_64),
    adults_u65_mental_pc = 1000 * sum(adults_u65_mental) / sum(pop_18_64),
    adults_u65_other_pc = 1000 * sum(adults_u65_other) / sum(pop_18_64),
    adults_u65_physical_pc = 1000 * sum(adults_u65_physical) / sum(pop_18_64),
    adults_u65_social_care_pc = 1000 * sum(adults_u65_social_care) / sum(pop_18_64)
  ) %>% select(year, class, contains("pc"))

write_csv(per_cap_spend_class, path="data/tidied/per_capita_spend_aggregated_to_la_class.csv")

per_cap_spend_regclass <- tot_exp %>% inner_join(pop_by_broad_age_group) %>%
  group_by(year, region, class) %>% 
  summarise(
    adults_65_pc = 1000 * sum(adults_65plus) / sum(pop_65_plus),
    adults_us6_learning_pc = 1000 * sum(adults_u65_learning) / sum(pop_18_64),
    adults_u65_mental_pc = 1000 * sum(adults_u65_mental) / sum(pop_18_64),
    adults_u65_other_pc = 1000 * sum(adults_u65_other) / sum(pop_18_64),
    adults_u65_physical_pc = 1000 * sum(adults_u65_physical) / sum(pop_18_64),
    adults_u65_social_care_pc = 1000 * sum(adults_u65_social_care) / sum(pop_18_64)
  ) %>% select(year, region, class, contains("pc"))

write_csv(per_cap_spend_regclass, path="data/tidied/per_capita_spend_aggregated_to_region_and_class.csv")


# Spend by region ---------------------------------------------------------

per_cap_spend %>% 
  gather(key="type", value= "per_cap_amt", -ons_code, -year, -region, -class) %>%  
  group_by(year, region) %>%
  summarise(mn_per_cap = mean(per_cap_amt)) %>% 
  ggplot(.) +
  geom_line(aes(x=year, y=mn_per_cap, group=region, colour=region)) + 
  labs(x="Start of financial year", y="mean per capita spend")

ggsave(filename="figures/per_cap_spend_by_region_and_year_one_facet.png",
       width=20, height=20, units="cm", dpi=150
       )

per_cap_spend %>% 
  gather(key="type", value= "per_cap_amt", -ons_code, -year, -region, -class) %>%  
  group_by(year, region) %>%
  summarise(mn_per_cap = mean(per_cap_amt)) %>% 
  ggplot(.) +
  geom_line(aes(x=year, y=mn_per_cap)) +
  facet_wrap(~region) + 
  labs(x="Start of financial year", y="mean per capita spend")

ggsave(filename="figures/per_cap_spend_by_region_and_year_facetted.png",
       width=20, height=20, units="cm", dpi=150
)


per_cap_spend %>% 
  gather(key="type", value= "per_cap_amt", -ons_code, -year, -region, -class) %>%  
  group_by(year, class) %>%
  summarise(mn_per_cap = mean(per_cap_amt)) %>% 
  ggplot(.) +
  geom_line(aes(x=year, y=mn_per_cap)) +
  facet_wrap(~class) + 
  labs(x="Start of financial year", y="mean per capita spend")


per_cap_spend %>% 
  gather(key="type", value= "per_cap_amt", -ons_code, -year, -region, -class) %>%  
  group_by(type, year, region) %>%
  summarise(mn_per_cap = mean(per_cap_amt)) %>% 
  ggplot(.) +
  geom_line(aes(x=year, y=mn_per_cap), size=1.3) + 
  facet_grid(type ~ region, scales="free_y") + 
  theme(axis.text.x=element_text(angle=90)) + 
  labs(title="Per capita spend by domain and region", y="Mean per capita spend (£ per person)", x="Start of Financial Year")

ggsave(filename="figures/per_cap_spend_by_region_year.png", 
       width=30, height=25, units="cm", dpi=150
       )


# By LA type

per_cap_spend %>% 
  gather(key="type", value= "per_cap_amt", -ons_code, -year, -region, -class) %>%  
  group_by(type, year, class) %>%
  summarise(mn_per_cap = mean(per_cap_amt)) %>% 
  ggplot(.) +
  geom_line(aes(x=year, y=mn_per_cap), size=1.3) + 
  facet_grid(type ~ class, scales="free_y") + 
  theme(axis.text.x=element_text(angle=90)) + 
  labs(title="Per capita spend by domain and LA class", y="Mean per capita spend (£ per person in 2014 prices)", x="Start of Financial Year")

ggsave(filename="figures/per_cap_spend_by_la_class_year.png", 
       width=30, height=25, units="cm", dpi=150
)

# By region and LA class
per_cap_spend %>% 
  gather(key="type", value= "per_cap_amt", -ons_code, -year, -region, -class) %>%  
  group_by(type, year, region, class) %>%
  summarise(mn_per_cap = mean(per_cap_amt)) %>% 
  ggplot(.) +
  geom_line(aes(x=year, y=mn_per_cap, group=class, colour=class), size=1.3) + 
  facet_grid(type ~ region, scales="free_y") + 
  theme(axis.text.x=element_text(angle=90)) + 
  labs(title="Per capita spend by domain, region and LA class", y="Mean per capita spend (£ per person in 2014 prices)", x="Start of Financial Year")

ggsave(filename="figures/per_cap_spend_by_domain_region_la_class_year.png", 
       width=30, height=25, units="cm", dpi=150
)

