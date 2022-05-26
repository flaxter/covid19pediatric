# US population data comes from https://www.census.gov/data/tables/2020/demo/popest/2020-demographic-analysis-tables.html
#         "xls Table 1. Total U.S. Resident Population by Age, Sex, and Series: April 1, 2020 (In thousands)"  
# Covid-19 death data comes from https://data.cdc.gov/api/views/3apk-4u4f 

library(tidyverse)
library(here)
library(readxl)
library(data.table)

us_data = read.csv(here("data/raw_data/Provisional_COVID-19_Death_Counts_by_Age_in_Years__2020-2022_May-04-2022.csv")) 

## how many months are covered by this data?
STUDY_PERIOD = as.numeric(round((as.Date(us_data$End.Date[1],format = "%m/%d/%Y") - as.Date("2020-03-01")) / (365/12)))

us_data = us_data %>% 
  select(Sex, Age.Years, Total.deaths, COVID.19.Deaths) %>%
  mutate(Age = str_replace(Age.Years, " Years", ""),
         Age = str_replace(Age, " Year", ""),
         Age = str_replace(Age, " Months", ""),
         Age = str_replace(Age, "85 and over", "85"),    
         Age = str_replace(Age, "0-05", "00"),    
         Age = str_replace(Age, "06-11", "00"),
         Age = as.numeric(Age)
         )

us_data_aggr = us_data %>% 
  group_by(Age) %>%
  summarise(covid = sum(COVID.19.Deaths),
            total = sum(Total.deaths))
  
pop_us = read_excel(here("data/raw_data/US_pop.xlsx"), range = "A4:E90") %>% 
  select(c(1,4)) %>%
  rename(Age = Total, pop_age ="332601") %>%
  mutate(Age = ifelse(Age == "85+", "85", Age),
         Age = as.numeric(Age),
         pop_age = as.numeric(pop_age)* 1000)

d_us = left_join(us_data_aggr,pop_us, by="Age") 

## CDC Wonder age groups
d_us_grouped = d_us %>% group_by(agegroup=cut(Age,c(0,1,5,10,15,20,100),right=F)) %>%   
  summarise(covid = sum(covid), pop_age = sum(pop_age)) %>%
  mutate(per100K = covid/pop_age * 100000, cause="Covid-19 (cumulative)")
d_us_grouped_annualized = d_us_grouped %>%
  mutate(per100K = covid/pop_age * 12 / STUDY_PERIOD  * 100000,covid = covid * 12 / STUDY_PERIOD,
         cause = "Covid-19 (annualized)")

## All children 0-19 years of age vs 50-54 (manuscript says 0.8 / 100k vs 89 per 100k)
d_us_grouped_0to19 = d_us %>% group_by(agegroup=cut(Age,c(0,20,50,55,100),right=F)) %>%   
  summarise(covid = sum(covid), pop_age = sum(pop_age)) %>%
  mutate(per100K = covid/pop_age * 100000, cause="Covid-19 (cumulative)")
d_us_grouped_annualized_0to19 = d_us_grouped_0to19 %>%   
  mutate(per100K = covid/pop_age * 12 / STUDY_PERIOD  * 100000,covid = covid * 12 / STUDY_PERIOD,
         cause = "Covid-19 (annualized)")

## Make plots

g = ggplot(filter(d_us, Age <= 18)) + 
  geom_bar(aes(x=Age,y=covid/pop_age*100000), stat="identity",
           position = position_dodge2(preserve='single'))  
g = g + scale_x_continuous(breaks = seq(0,20,2),expand=c(0,0))
g = g + scale_y_continuous(expand=c(0,0),limits=c(0,8))
g = g + labs(y = "Deaths per 100,000\n", x="\nAge") 
g = g + theme_bw()
g

## Text: "Cumulative Covid-19 death rates peaked in infants aged <1 year at 7.2 deaths per 100,000 population."
# The cumulative Covid-19 death rate was 1.6 per 100,000 in 1-year old children, below 1 per 100,000 in children aged 2-12, 
# gradually increasing to 3.9 per 100,000 in 18-year olds. 

d_us %>% filter(Age <= 18) %>% mutate(rate=round(covid/pop_age*100000,1)) %>% select(Age,rate)

# Figure 1
ggsave(g,filename = here("figures/US-death-rate.png"),width=7,height=4)

## Compare to CDC Wonder

d = rbind(d_us_grouped,d_us_grouped_annualized) %>% select(rate=per100K,cause,agegroup,deaths=covid) %>% filter(agegroup != "[20,100)")

d = rbind(
  fread(here("data/raw_data/wonder0.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[0,1)"),
  fread(here("data/raw_data/wonder1-4.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[1,5)"),
  fread(here("data/raw_data/wonder5-9.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[5,10)"),
  fread(here("data/raw_data/wonder10-14.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[10,15)"),
  fread(here("data/raw_data/wonder15-19.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[15,20)"),d)
d$rate = as.numeric(d$rate)
d = d[complete.cases(d),]

## "Cumulative ranks are: #4 (<1 year old), #5 (1-4 year olds), #5 (5-9 year olds), #4 (10-14 year olds), and #4 (15-19 year olds). Annualized ranks are: #8 (<1 year old), #7 (1-4 year olds), #6 (5-9 year olds), #6 (10-14 year olds), and #5 (15-19 year olds)."
d %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>% mutate(rank =  rank(-rate,ties.method="min")) %>% filter(cause == "Covid-19 (cumulative)")
d %>% filter(cause != "Covid-19 (cumulative)") %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>% mutate(rank =  rank(-rate,ties.method="min")) %>% filter(cause == "Covid-19 (annualized)")

### Table 1
write.csv(d %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
            mutate(rank =  rank(-rate,ties.method="min")) %>% arrange(agegroup,-deaths) %>% mutate(rate=round(rate,1),deaths=round(deaths)),file = here("results/WONDER-agegroups.csv"),row.names=F)


#### Supplementary

d = rbind(d_us_grouped_0to19,d_us_grouped_annualized_0to19) %>% select(rate=per100K,cause,agegroup,deaths=covid) %>% filter(agegroup == "[0,20)")

d = rbind(
  fread(here("data/raw_data/wonder0-19.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% 
    mutate(agegroup="[0,20)"),d)

# Table S1
write.csv(d %>% filter(agegroup == "[0,20)") %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
            mutate(rank =  rank(-rate,ties.method="min")) %>% arrange(agegroup,-rate) %>% mutate(rate=round(rate,1),deaths=round(deaths)),file = here("results/WONDER-0-19.csv"),row.names=F)

d = rbind(d_us_grouped_0to19,d_us_grouped_annualized_0to19) %>% select(rate=per100K,cause,agegroup,deaths=covid) %>% filter(agegroup == "[0,20)")

d = rbind(
  fread(here("data/raw_data/wonder-infectious-0-19.txt"),nrows=27) %>% select(cause=`ICD-10 113 Cause List`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[0,20)"),d)

# Table S2
write.csv(d %>% filter(agegroup == "[0,20)") %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
            mutate(rank =  rank(-rate,ties.method="min")) %>% arrange(agegroup,-rate)  %>% mutate(rate=round(rate,1),deaths=round(deaths)),file = here("results/WONDER-0-19-infectious.csv"),row.names=F)

