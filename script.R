

# Title: local authority mortality rate exploration -----------------------


rm(list=ls())



# pre-requisites ----------------------------------------------------------


require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

require(ggplot2)
require(lattice)


# Data --------------------------------------------------------------------

dta <- read.csv("data/unzipped/MYEB2_detailed_components_of_change_series_EW_(0213).csv") %>%
  tbl_df


data_tidied <- dta  %>% 
  gather(key=label, value=count, -lad2013_code, -lad2013_name, -country, -sex, -age)  %>% 
  mutate(
    year = str_extract(label, "\\d{1,4}"), 
    category = str_replace(str_extract(label, "\\D*"), "_$", ""),
    sex=ifelse(sex==1, "male", "female"))   %>% 
  select(lad2013_code, country, sex, age, year, category, count) %>%
  spread(category, count)

write.csv(data_tidied, file="data/tidied/england_la_count.csv", row.names=FALSE)
