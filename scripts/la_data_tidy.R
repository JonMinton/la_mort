# Title: local authority mortality rate exploration -----------------------

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


# Data on expenditure -----------------------------------------------------



raw_08 <- read_excel(
  "data/care_cuts/Expenditure/RO3/combined.xlsx", 
  sheet="flat_2008"
  )

raw_09 <- read_excel(
  "data/care_cuts/Expenditure/RO3/combined.xlsx", 
  sheet="flat_2009"
)

raw_10 <- read_excel(
  "data/care_cuts/Expenditure/RO3/combined.xlsx", 
  sheet="flat_2010"
)

raw_11 <- read_excel(
  "data/care_cuts/Expenditure/RO3/combined.xlsx", 
  sheet="flat_2011"
)

raw_12 <- read_excel(
  "data/care_cuts/Expenditure/RO3/combined.xlsx", 
  sheet="flat_2012"
)

raw_13 <- read_excel(
  "data/care_cuts/Expenditure/RO3/combined.xlsx", 
  sheet="flat_2013"
)

long_08 <- raw_08 %>% select(-`Local authority`, -`Class`) %>% 
     gather(key="type", value="amount", -start_year, -`E-code`) %>% 
     separate(col = type, into = c("lvl1", "lvl2"), sep = "\\|")

long_09 <- raw_09 %>% select(-`Local authority`, -`Class`, -Region) %>% 
  gather(key="type", value="amount", -Start_year, -`E-code`) %>% 
  separate(col=type, into=c("main_group", "sub_group", "expense_type"), sep="\\|")

long_10 <- raw_10 %>% select(-`NA`, -Class, -`Local authority` ) %>% 
  gather(key="type", value = "amount", -start_year, -`E-code`) %>% 
  separate(col=type, into=c("main_group", "sub_group", "expense_type"), sep="\\|")

long_11 <- raw_11[,!duplicated(names(raw_11))] %>% 
  select(-`Class`, -`Local authority`) %>% 
  gather(key="type", value="amount", -start_year, - `E-code`) %>% 
  separate(col=type, into=c("main_group", "sub_group", "expense_type"), sep="\\|")
  
long_12 <- raw_12  %>% 
  select(-`Class`, -`Local authority`) %>% 
  gather(key="type", value="amount", -start_year, - `E-code`) %>% 
  separate(col=type, into=c("main_group", "sub_group", "expense_type"), sep="\\|")

long_13 <- raw_13 %>% 
  select(-`Class`, -`Local authority`) %>% 
  gather(key="type", value="amount", -start_year, - `E-code`) %>% 
  separate(col=type, into=c("main_group", "sub_group", "expense_type"), sep="\\|")


# 2008, outer
tidy_08 <- long_08
tidy_08$lvl1 <- str_trim(tidy_08$lvl1)
tidy_08$outer <- NA
tidy_08$outer[str_detect(tidy_08$lvl1, "[Cc]hildren")] <- "Children"
tidy_08$outer[str_detect(tidy_08$lvl1, "[Aa]dult|aged 65|employ")] <- "Adults"


#2009, outer
tidy_09 <- long_09
tidy_09$main_group <- str_trim(tidy_09$main_group)
tidy_09$sub_group <- str_trim(tidy_09$sub_group)
tidy_09$outer <- NA
tidy_09$outer[str_detect(tidy_09$main_group, "Adult")] <- "Adults"
tidy_09$outer[str_detect(tidy_09$main_group, "Children")] <- "Children"


#2010, outer
tidy_10 <- long_10
tidy_10$main_group <- str_trim(tidy_10$main_group)
tidy_10$sub_group <- str_trim(tidy_10$sub_group)
tidy_10$outer <- NA
tidy_10$outer[str_detect(tidy_10$main_group, "Adult")] <- "Adults"
tidy_10$outer[str_detect(tidy_10$main_group, "Children")] <- "Children"

#2011, outer
tidy_11 <- long_11
tidy_11$main_group <- str_trim(tidy_11$main_group)
tidy_11$sub_group <- str_trim(tidy_11$sub_group)
tidy_11$outer <- NA
tidy_11$outer[str_detect(tidy_11$main_group, "Adult")] <- "Adults"
tidy_11$outer[str_detect(tidy_11$main_group, "Children")] <- "Children"

#2012, outer
tidy_12 <- long_12
tidy_12$main_group <- str_trim(tidy_12$main_group)
tidy_12$sub_group <- str_trim(tidy_12$sub_group)
tidy_12$outer <- NA
tidy_12$outer[str_detect(tidy_12$main_group, "Adult")] <- "Adults"
tidy_12$outer[str_detect(tidy_12$main_group, "Children")] <- "Children"

#2013, outer
tidy_13 <- long_13
tidy_13$main_group <- str_trim(tidy_13$main_group)
tidy_13$sub_group <- str_trim(tidy_13$sub_group)
tidy_13$outer <- NA
tidy_13$outer[str_detect(tidy_13$main_group, "Adult")] <- "Adults"
tidy_13$outer[str_detect(tidy_13$main_group, "Children")] <- "Children"



#2008, inner
tidy_08$inner <- NA
tidy_08$inner[
  str_detect(tidy_08$lvl1, "[cC]hildren") &
  str_detect(tidy_08$lvl1, "[Aa]sylum") ] <- "children_asylum_seekers"
tidy_08$inner[
  str_detect(tidy_08$lvl1, "[cC]hildren") &
    str_detect(tidy_08$lvl1, "Social Care") ] <- "children_social_care"
tidy_08$inner[
  str_detect(tidy_08$lvl1, "[cC]hildren") &
    str_detect(tidy_08$lvl1, "[Oo]ther") ] <- "children_other"

tidy_08$inner[
  str_detect(tidy_08$lvl1, "[Aa]dult") &
    str_detect(tidy_08$lvl1, "under 65") &
    str_detect(tidy_08$lvl1, "learning") ] <- "adults_u65_learning"

tidy_08$inner[
  str_detect(tidy_08$lvl1, "[Aa]dult") &
    str_detect(tidy_08$lvl1, "under 65") &
    str_detect(tidy_08$lvl1, "mental") ] <- "adults_u65_mental"

tidy_08$inner[
  str_detect(tidy_08$lvl1, "[Aa]dult") &
    str_detect(tidy_08$lvl1, "under 65") &
    str_detect(tidy_08$lvl1, "physical") ] <- "adults_u65_physical"

tidy_08$inner[
    str_detect(tidy_08$lvl1, "aged 65 or over") ] <- "adults_65plus"

tidy_08$inner[
  str_detect(tidy_08$lvl1, "[Aa]dult") &
    str_detect(tidy_08$lvl1, "asylum") ] <- "adults_u65_asylum_seekers"

tidy_08$inner[
  str_detect(tidy_08$lvl1, "[Aa]dult") &
    str_detect(tidy_08$lvl1, "other$") ] <- "adults_u65_other"

tidy_08$inner[
  str_detect(tidy_08$lvl1, "Supported employment")
  ] <- "adults_u65_supported_employment"

tidy_08$inner[
  str_detect(tidy_08$lvl1, "[sS]ocial [cC]are") &
    str_detect(tidy_08$lvl1, "adults$")
  ] <- "adults_u65_social_care"


# 2009, inner
tidy_09$inner <- NA
tidy_09$inner[
  str_detect(tidy_09$sub_group, "[cC]hildren") &
    str_detect(tidy_09$sub_group, "[Aa]sylum") ] <- "children_asylum_seekers"
tidy_09$inner[
  str_detect(tidy_09$sub_group, "[cC]hildren") &
    str_detect(tidy_09$sub_group, "[sS]ocial [cC]are") ] <- "children_social_care"
tidy_09$inner[
  str_detect(tidy_09$sub_group, "[cC]hildren") &
    str_detect(tidy_09$sub_group, "[Oo]ther") ] <- "children_other"

tidy_09$inner[
  str_detect(tidy_09$sub_group, "[Aa]dult") &
    str_detect(tidy_09$sub_group, "under 65") &
    str_detect(tidy_09$sub_group, "learning") ] <- "adults_u65_learning"

tidy_09$inner[
  str_detect(tidy_09$sub_group, "[Aa]dult") &
    str_detect(tidy_09$sub_group, "under 65") &
    str_detect(tidy_09$sub_group, "mental") ] <- "adults_u65_mental"

tidy_09$inner[
  str_detect(tidy_09$sub_group, "[Aa]dult") &
    str_detect(tidy_09$sub_group, "under 65") &
    str_detect(tidy_09$sub_group, "physical") ] <- "adults_u65_physical"

tidy_09$inner[
  str_detect(tidy_09$sub_group, "aged 65 or over") ] <- "adults_65plus"

tidy_09$inner[
  str_detect(tidy_09$sub_group, "[Aa]dult") &
    str_detect(tidy_09$sub_group, "asylum") ] <- "adults_u65_asylum_seekers"

tidy_09$inner[
  str_detect(tidy_09$sub_group, "[Aa]dult") &
    str_detect(tidy_09$sub_group, "other$") ] <- "adults_u65_other"

tidy_09$inner[
  str_detect(tidy_09$sub_group, "Supported employment")
  ] <- "adults_u65_supported_employment"

tidy_09$inner[
  str_detect(tidy_09$sub_group, "[sS]ocial [cC]are") &
    str_detect(tidy_09$sub_group, "adults$")
  ] <- "adults_u65_social_care"

tidy_09$inner[
  str_detect(tidy_09$sub_group, "Supporting people") 
  ] <- "supporting_people"


# 2010, inner
tidy_10$inner <- NA
tidy_10$inner[
  str_detect(tidy_10$sub_group, "[cC]hildren") &
    str_detect(tidy_10$sub_group, "[Aa]sylum") ] <- "children_asylum_seekers"
tidy_10$inner[
  str_detect(tidy_10$sub_group, "[cC]hildren") &
    str_detect(tidy_10$sub_group, "[sS]ocial [cC]are") ] <- "children_social_care"
tidy_10$inner[
  str_detect(tidy_10$sub_group, "[cC]hildren") &
    str_detect(tidy_10$sub_group, "[Oo]ther") ] <- "children_other"

tidy_10$inner[
  str_detect(tidy_10$sub_group, "[Aa]dult") &
    str_detect(tidy_10$sub_group, "under 65") &
    str_detect(tidy_10$sub_group, "learning") ] <- "adults_u65_learning"

tidy_10$inner[
  str_detect(tidy_10$sub_group, "[Aa]dult") &
    str_detect(tidy_10$sub_group, "under 65") &
    str_detect(tidy_10$sub_group, "mental") ] <- "adults_u65_mental"

tidy_10$inner[
  str_detect(tidy_10$sub_group, "[Aa]dult") &
    str_detect(tidy_10$sub_group, "under 65") &
    str_detect(tidy_10$sub_group, "physical") ] <- "adults_u65_physical"

tidy_10$inner[
  str_detect(tidy_10$sub_group, "aged 65 or over") ] <- "adults_65plus"

tidy_10$inner[
  str_detect(tidy_10$sub_group, "[Aa]dult") &
    str_detect(tidy_10$sub_group, "asylum") ] <- "adults_u65_asylum_seekers"

tidy_10$inner[
  str_detect(tidy_10$sub_group, "[Aa]dult") &
    str_detect(tidy_10$sub_group, "other$") ] <- "adults_u65_other"

tidy_10$inner[
  str_detect(tidy_10$sub_group, "Supported employment")
  ] <- "adults_u65_supported_employment"

tidy_10$inner[
  str_detect(tidy_10$sub_group, "[sS]ocial [cC]are") &
    str_detect(tidy_10$sub_group, "adults$")
  ] <- "adults_u65_social_care"

tidy_10$inner[
  str_detect(tidy_10$sub_group, "Supporting people") 
  ] <- "supporting_people"


# 2011, inner
tidy_11$inner <- NA
tidy_11$inner[
  str_detect(tidy_11$sub_group, "[cC]hildren") &
    str_detect(tidy_11$sub_group, "[Aa]sylum Seekers$") ] <- "children_asylum_seekers"
tidy_11$inner[
  str_detect(tidy_11$sub_group, "[cC]hildren") &
    str_detect(tidy_11$sub_group, "[sS]ocial [cC]are")  &
    !str_detect(tidy_11$sub_group, "[Aa]sylum Seekers$")] <- "children_social_care"
tidy_11$inner[
  str_detect(tidy_11$sub_group, "[cC]hildren") &
    str_detect(tidy_11$sub_group, "[Oo]ther") ] <- "children_other"

tidy_11$inner[
  str_detect(tidy_11$sub_group, "[Aa]dult") &
    str_detect(tidy_11$sub_group, "under 65") &
    str_detect(tidy_11$sub_group, "learning") ] <- "adults_u65_learning"

tidy_11$inner[
  str_detect(tidy_11$sub_group, "[Aa]dult") &
    str_detect(tidy_11$sub_group, "under 65") &
    str_detect(tidy_11$sub_group, "mental") ] <- "adults_u65_mental"

tidy_11$inner[
  str_detect(tidy_11$sub_group, "[Aa]dult") &
    str_detect(tidy_11$sub_group, "under 65") &
    str_detect(tidy_11$sub_group, "physical") ] <- "adults_u65_physical"

tidy_11$inner[
  str_detect(tidy_11$sub_group, "aged 65 or over") ] <- "adults_65plus"

tidy_11$inner[
  str_detect(tidy_11$sub_group, "[Aa]dult") &
    str_detect(tidy_11$sub_group, "asylum") ] <- "adults_u65_asylum_seekers"

tidy_11$inner[
  str_detect(tidy_11$sub_group, "[Aa]dult") &
    str_detect(tidy_11$sub_group, "other$") ] <- "adults_u65_other"

tidy_11$inner[
  str_detect(tidy_11$sub_group, "Supported employment")
  ] <- "adults_u65_supported_employment"

tidy_11$inner[
  str_detect(tidy_11$sub_group, "[sS]ocial [cC]are") &
    str_detect(tidy_11$sub_group, "adults$")
  ] <- "adults_u65_social_care"

tidy_11$inner[
  str_detect(tidy_11$sub_group, "[Aa]dult") &
  str_detect(tidy_11$sub_group, "Supporting people") 
  ] <- "adults_supporting_people"

tidy_11$inner[
  str_detect(tidy_11$sub_group, "[Cc]hildren") &
    str_detect(tidy_11$sub_group, "Supporting people") 
  ] <- "children_supporting_people"


# 2012, inner
tidy_12$inner <- NA
tidy_12$inner[
  str_detect(tidy_12$sub_group, "[cC]hildren") &
    str_detect(tidy_12$sub_group, "[Aa]sylum Seekers$") ] <- "children_asylum_seekers"
tidy_12$inner[
  str_detect(tidy_12$sub_group, "[cC]hildren") &
    str_detect(tidy_12$sub_group, "[sS]ocial [cC]are")  &
    !str_detect(tidy_12$sub_group, "[Aa]sylum Seekers$")] <- "children_social_care"
tidy_12$inner[
  str_detect(tidy_12$sub_group, "[cC]hildren") &
    str_detect(tidy_12$sub_group, "[Oo]ther") ] <- "children_other"

tidy_12$inner[
  str_detect(tidy_12$sub_group, "[Aa]dult") &
    str_detect(tidy_12$sub_group, "under 65") &
    str_detect(tidy_12$sub_group, "learning") ] <- "adults_u65_learning"

tidy_12$inner[
  str_detect(tidy_12$sub_group, "[Aa]dult") &
    str_detect(tidy_12$sub_group, "under 65") &
    str_detect(tidy_12$sub_group, "mental") ] <- "adults_u65_mental"

tidy_12$inner[
  str_detect(tidy_12$sub_group, "[Aa]dult") &
    str_detect(tidy_12$sub_group, "under 65") &
    str_detect(tidy_12$sub_group, "physical") ] <- "adults_u65_physical"

tidy_12$inner[
  str_detect(tidy_12$sub_group, "aged 65 or over") ] <- "adults_65plus"

tidy_12$inner[
  str_detect(tidy_12$sub_group, "[Aa]dult") &
    str_detect(tidy_12$sub_group, "asylum") ] <- "adults_u65_asylum_seekers"

tidy_12$inner[
  str_detect(tidy_12$sub_group, "[Aa]dult") &
    str_detect(tidy_12$sub_group, "other$") ] <- "adults_u65_other"

tidy_12$inner[
  str_detect(tidy_12$sub_group, "Supported employment")
  ] <- "adults_u65_supported_employment"

tidy_12$inner[
  str_detect(tidy_12$sub_group, "[sS]ocial [cC]are") &
    str_detect(tidy_12$sub_group, "adults$")
  ] <- "adults_u65_social_care"

tidy_12$inner[
  str_detect(tidy_12$sub_group, "[Aa]dult") &
    str_detect(tidy_12$sub_group, "Supporting people") 
  ] <- "adults_supporting_people"

tidy_12$inner[
  str_detect(tidy_12$sub_group, "[Cc]hildren") &
    str_detect(tidy_12$sub_group, "Supporting people") 
  ] <- "children_supporting_people"

# 2013, inner
tidy_13$inner <- NA
tidy_13$inner[
  str_detect(tidy_13$sub_group, "[cC]hildren") &
    str_detect(tidy_13$sub_group, "[Aa]sylum Seekers$") ] <- "children_asylum_seekers"
tidy_13$inner[
  str_detect(tidy_13$sub_group, "[cC]hildren") &
    str_detect(tidy_13$sub_group, "[sS]ocial [cC]are")  &
    !str_detect(tidy_13$sub_group, "[Aa]sylum Seekers$")] <- "children_social_care"
tidy_13$inner[
  str_detect(tidy_13$sub_group, "[cC]hildren") &
    str_detect(tidy_13$sub_group, "[Oo]ther") ] <- "children_other"

tidy_13$inner[
  str_detect(tidy_13$sub_group, "[Aa]dult") &
    str_detect(tidy_13$sub_group, "under 65") &
    str_detect(tidy_13$sub_group, "learning") ] <- "adults_u65_learning"

tidy_13$inner[
  str_detect(tidy_13$sub_group, "[Aa]dult") &
    str_detect(tidy_13$sub_group, "under 65") &
    str_detect(tidy_13$sub_group, "mental") ] <- "adults_u65_mental"

tidy_13$inner[
  str_detect(tidy_13$sub_group, "[Aa]dult") &
    str_detect(tidy_13$sub_group, "under 65") &
    str_detect(tidy_13$sub_group, "physical") ] <- "adults_u65_physical"

tidy_13$inner[
  str_detect(tidy_13$sub_group, "aged 65 or over") ] <- "adults_65plus"

tidy_13$inner[
  str_detect(tidy_13$sub_group, "[Aa]dult") &
    str_detect(tidy_13$sub_group, "asylum") ] <- "adults_u65_asylum_seekers"

tidy_13$inner[
  str_detect(tidy_13$sub_group, "[Aa]dult") &
    str_detect(tidy_13$sub_group, "other$") ] <- "adults_u65_other"

tidy_13$inner[
  str_detect(tidy_13$sub_group, "Supported employment")
  ] <- "adults_u65_supported_employment"

tidy_13$inner[
  str_detect(tidy_13$sub_group, "[sS]ocial [cC]are") &
    str_detect(tidy_13$sub_group, "adults$")
  ] <- "adults_u65_social_care"

tidy_13$inner[
  str_detect(tidy_13$sub_group, "[Aa]dult") &
    str_detect(tidy_13$sub_group, "Supporting people") 
  ] <- "adults_supporting_people"

tidy_13$inner[
  str_detect(tidy_13$sub_group, "[Cc]hildren") &
    str_detect(tidy_13$sub_group, "Supporting people") 
  ] <- "children_supporting_people"  


# NEXT TO DO: consistency w/ Expense Type


tidy_08$lvl2 <- str_trim(tidy_08$lvl2)
tidy_08 <- tidy_08 %>% rename(expense_type = lvl2) 

tidy_09$expense_type <- str_trim(tidy_09$expense_type)
tidy_10$expense_type <- str_trim(tidy_10$expense_type)
tidy_11$expense_type <- str_trim(tidy_11$expense_type)
tidy_12$expense_type <- str_trim(tidy_12$expense_type)
tidy_13$expense_type <- str_trim(tidy_13$expense_type)


tdy_08 <- tidy_08 %>% 
  select(start_year, ecode = `E-code`, outer, inner, expense_type, amount)

tdy_09 <- tidy_09 %>% 
  select(start_year=Start_year, ecode = `E-code`, outer, inner, expense_type, amount)

tdy_10 <- tidy_10 %>% 
  select(start_year, ecode = `E-code`, outer, inner, expense_type, amount)

tdy_11 <- tidy_11 %>% 
  select(start_year, ecode = `E-code`, outer, inner, expense_type, amount)

tdy_12 <- tidy_12 %>% 
  select(start_year, ecode = `E-code`, outer, inner, expense_type, amount)

tdy_13 <- tidy_13 %>% 
  select(start_year, ecode = `E-code`, outer, inner, expense_type, amount)

tdy <- tdy_08 %>% 
  bind_rows(tdy_09) %>% 
  bind_rows(tdy_10) %>% 
  bind_rows(tdy_11) %>% 
  bind_rows(tdy_12) %>% 
  bind_rows(tdy_13) 

tdy$expense_type <- revalue(
  tdy$expense_type,
  replace = c(
    "Capital Charges"         = "capital_charges",
    "Capital Charges (C8)"    = "capital_charges", 
    "Capital items (C8)"      = 'capital_charges',
    "Employees"               = "employees",
    "Employees (C1)"          = "employees",
    "Other Income (C5)"                   = "other_income",
    "Other income (inc joint arrangemts)" = "other_income",
    "Sales Fees and Charges"        = "sales_fees_charges", 
    "Sales, Fees & Charges (C4)"    = "sales_fees_charges",
    "Sales, Fees and Charges (C4)"  = "sales_fees_charges",
    "Running Expenses (C2)"                   = "running_expenses", 
    "Running expenses (inc joint arrangemts)" = "running_expenses",
    "NET CURRENT (Col 3-6)"                   = "net_current",
    "Net Current Expenditure (C7 = C3 - C6)"  = "net_current",
    
    "NET TOTAL COST  exc Specific Grants" = "net_total_cost",
    "Net Total Cost (C9 = C7 + C8)"       = "net_total_cost",
    
    "TOTAL  INCOME (Col 4+5)"             = "total_income",
    "Total Income (C6 = C4 + C5)"         = "total_income",
    
    "TOTAL EXPENDITURE (Col 1+2)"         = "total_expenditure",
    "Total Expenditure (C3 = C1 + C2)"    = "total_expenditure"
  )
  )

# Now to link to ONS codes and find only UAs 

links <- read_csv("data/support/lookups_between_ons_and_ecodes.csv")

tdy <- links %>% select(ecode, ons_code) %>% inner_join(tdy)

tdy <- tdy %>% select(ecode, ons_code, start_year, outer, inner, expense_type, amount)

tdy %>% 
  filter(expense_type=="net_total_cost") %>% 
  group_by(start_year, inner) %>% 
  filter(!is.na(inner)) %>% 
  summarise(amount=sum(amount)/ 1000000) %>% 
  ggplot(data=.) +
  geom_line(aes(x=start_year, y=amount)) +
  facet_wrap(~inner, scales="free_y")

tdy %>% 
  filter(expense_type=="net_total_cost") %>% 
  group_by(start_year, outer) %>% 
  filter(!is.na(outer) & !is.na(inner)) %>% 
  summarise(amount=sum(amount)/ 1000000) %>% 
  ggplot(data=.) +
  geom_bar(aes(x=factor(start_year), y=amount, group=outer, fill=outer), stat ="identity") +
  labs(x="Start of accounting year", y="Total spend in £billion") +
  theme(
    axis.text.x=element_text(angle=90), 
    legend.position="bottom", 
    legend.title=element_blank()
    )

ggsave("figures/total_spend_by_year.png",
       height=10, width=8, dpi = 150, units = "cm")

# Save tidied LA spend data

tdy %>% filter(!is.na(outer) & !is.na(inner)) %>% 
  write_csv(path="data/care_cuts/combined_linked_and_tidied_r03.csv")


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

