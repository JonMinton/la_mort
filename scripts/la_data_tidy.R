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

ggsave("figures/total_spend_by_inner_category.png",
       height=15, width=15, dpi = 300, units = "cm")


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

