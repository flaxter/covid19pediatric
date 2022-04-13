# US death data : https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Age-in-Years-/3apk-4u4f
#
# US population data:  
#         https://www.census.gov/data/tables/2020/demo/popest/2020-demographic-analysis-tables.html
#         "xls Table 1. Total U.S. Resident Population by Age, Sex, and Series: April 1, 2020 (In thousands)"  


library(tidyverse)
library(here)
library(readxl)

#----------------------------------------
# US death data
#----------------------------------------

us_data <- read.csv(here("data/raw_data/Provisional_COVID-19_Death_by_Age_in_Years__2020-2022.csv")) 
        
us_data <- us_data %>% 
  select(Sex, Age.Years, Total.deaths, COVID.19.Deaths) %>%
  mutate(Age = str_replace(Age.Years, " Years", ""),
         Age = str_replace(Age, " Year", ""),
         Age = str_replace(Age, " Months", ""),
         Age = str_replace(Age, "85 and over", "85"),    
         Age = str_replace(Age, "0-05", "00"),    
         Age = str_replace(Age, "06-11", "00"),
         Age = as.numeric(Age)
  )

us_covid <- us_data %>% 
  group_by(Age) %>%
  summarise(covid = sum(COVID.19.Deaths))

sum(us_covid$covid) == sum(us_data$COVID.19.Deaths)

(p <- ggplot(filter(us_covid, Age < 20)) + 
    geom_bar(aes(x = Age, y = covid), stat="identity") + 
    labs(y = "Number of deaths") + 
    ggtitle("US COVID-19 deaths") )

saveRDS(us_covid, here("data/us_covid.rds"))

#----------------------------------------
# US - population Age distribution
#----------------------------------------
us_pop <- read_excel("data/raw_data/US_pop.xlsx", range = "A4:E90") %>% 
  select(c(1,4)) %>%
  rename(Age = Total, pop_age ="332601") %>%
  mutate(Age = ifelse(Age == "85+", "85", Age),
         Age = as.numeric(Age),
         pop_age = as.numeric(pop_age)* 1000)

saveRDS(us_pop, here("data/us_pop.rds"))
