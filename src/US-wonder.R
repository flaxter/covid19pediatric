library(data.table)
library(tidyverse)
library(here)
library(readxl)

# US population data comes from https://www.census.gov/data/tables/2020/demo/popest/2020-demographic-analysis-tables.html
#         "xls Table 1. Total U.S. Resident Population by Age, Sex, and Series: April 1, 2020 (In thousands)"  

pop_us <- read_excel(here("data/raw_data/US_pop.xlsx"), range = "A4:E90") %>% 
  select(c(1,4)) %>%
  rename(Age = Total, pop_age ="332601") %>%
  mutate(Age = ifelse(Age == "85+", "85", Age),
         Age = as.numeric(Age),
         pop_age = as.numeric(pop_age)* 1000)

## data was downloaded on 23 June 2022 from https://wonder.cdc.gov/controller/saved/D176/D296F409
## Year/Month:	2020; 2021 (provisional); Jan., 2022 (provisional); Feb., 2022 (provisional); 
## Mar., 2022 (provisional); Apr., 2022 (provisional)
## we consider the time period March 2020-April 2022
## this is 26 months (though note there were no deaths in March for ages <= 19)
covid = NULL
for(i in 0:19) {
  covid = rbind(covid,fread(here(sprintf("data/raw_data/wonder-provisional%d.txt",i)),nrows=1))
}

covid = covid %>% select(deaths=Deaths) %>% mutate(cause="#COVID-19 (U07.1)",Age=0:19)
covid = left_join(covid,pop_us, by="Age") 
covid_annualized = covid
covid_annualized$cause = "#COVID-19 (U07.1) (annualized)"
covid$cause = "#COVID-19 (U07.1) (cumulative)"
covid_annualized$deaths = covid_annualized$deaths * 12/26
covid = rbind(covid,covid_annualized)
covid$rate = 100000 * covid$deaths / covid$pop_age
covid %>% filter(Age <= 5)
covid_grouped = covid %>% group_by(agegroup=cut(Age,c(0,1,5,10,15,20,100),right=F),cause) %>% summarize(deaths=sum(deaths),pop_age=sum(pop_age)) %>%
  mutate(rate = deaths / pop_age * 100000)

covid$deaths = round(covid$deaths)
covid_grouped$deaths = round(covid_grouped$deaths)

covid0_19 = fread(here("data/raw_data/wonder-provisional0-19.txt"),nrows=15) %>% 
  select(cause=`UCD - 15 Leading Causes of Death`,deaths=Deaths) %>% 
  mutate(agegroup="[0,20)") %>% filter(cause == "#COVID-19 (U07.1)")
covid0_19 = rbind(covid0_19,covid0_19)

covid0_19$cause = c(paste(covid0_19$cause[1], "(annualized)"), paste(covid0_19$cause[1], "(cumulative)"))
covid0_19$deaths[1] = covid0_19$deaths[1] * 12/26
covid0_19$rate = 100000 * covid0_19$deaths / as.numeric(pop_us %>% filter(Age <= 19) %>% summarize(n = sum(pop_age)) %>% select(n) )
covid0_19$deaths = round(covid0_19$deaths)
covid0_19

### compare to other leading causes of death: ages < 1, 1-4, 5-9, 10-14, 15-19
d = rbind(
  fread(here("data/raw_data/wonder0.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[0,1)"),
  fread(here("data/raw_data/wonder1-4.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[1,5)"),
  fread(here("data/raw_data/wonder5-9.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[5,10)"),
  fread(here("data/raw_data/wonder10-14.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[10,15)"),
  fread(here("data/raw_data/wonder15-19.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[15,20)"),
  covid_grouped %>% select(cause,rate,deaths,agegroup))
d$rate = as.numeric(d$rate)
d = d[complete.cases(d),]

for(ag in unique(d$agegroup)) {
  print(sprintf("Age group: %s",as.character(ag)))
  print(d %>% filter(agegroup == ag)  %>% mutate(rate=round(as.numeric(rate),1)) %>% arrange(-rate) %>% select(cause,rate,deaths))
  print(d %>% filter(agegroup == ag)  %>% mutate(rate=round(as.numeric(rate),4)) %>% arrange(-deaths) %>% select(cause,rate,deaths))
  
  print(max(diff((d %>% filter(agegroup == ag)  %>% mutate(rate=round(as.numeric(rate),1)) %>% arrange(-deaths) %>% select(cause,rate,deaths))$rate),na.rm=T))
}

write.csv(d %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
            mutate(rank =  rank(-rate,ties.method="min")) %>% arrange(agegroup,-deaths),file = here("results/WONDER-agegroups.csv"),row.names=F)

### compare to other leading causes of death: ages 0-19 together
d = rbind(
  fread(here("data/raw_data/wonder0-19.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths) %>% 
    mutate(agegroup="[0,20)"),covid0_19)

for(ag in unique(d$agegroup)[1]) {
  print(sprintf("Age group: %s",as.character(ag)))
  print(d %>% filter(agegroup == ag)  %>% mutate(rate=round(as.numeric(rate),2),
                                                 rank =  rank(-rate,ties.method="min")
  ) %>% arrange(-rate))
}
write.csv(d %>% filter(agegroup == "[0,20)") %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
            mutate(rank =  rank(-rate,ties.method="min")) %>% arrange(agegroup,-deaths),file = here("results/WONDER-0-19.csv"),row.names=F)

d = rbind(
  fread(here("data/raw_data/wonder-infectious-0-19.txt"),nrows=27) %>% select(cause=`ICD-10 113 Cause List`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[0,20)"),covid0_19)
write.csv(d %>% filter(agegroup == "[0,20)") %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
            mutate(rank =  rank(-rate,ties.method="min")) %>% arrange(agegroup,-deaths),file = here("results/WONDER-0-19-infectious.csv"),row.names=F)

# 
d %>% filter(cause != "Covid-19 (cumulative)") %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
  mutate(rank =  rank(-rate,ties.method="min")) %>% arrange(agegroup,-rate) %>% filter(cause == "Covid-19 (annualized)")
d %>% filter(cause != "Covid-19 (annualized)") %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
  mutate(rank =  rank(-rate,ties.method="min")) %>% arrange(agegroup,-rate) %>% filter(cause == "Covid-19 (cumulative)")



# write.csv(d %>% mutate(rate=round(as.numeric(rate),1)) %>% arrange(agegroup,-deaths),file = here("results/WONDER-agegroups-2.csv"),row.names=F)
# d %>% mutate(rate=round(as.numeric(rate),1)) %>% arrange(agegroup,-rate) %>% group_by(agegroup) %>% mutate(rank = 1:length(deaths)) %>% filter(cause == "Covid-19 (annualized)")
# d %>% mutate(rate=round(as.numeric(rate),1)) %>% arrange(agegroup,-deaths) %>% group_by(agegroup) %>% mutate(rank = 1:length(deaths)) %>% filter(cause == "Covid-19 (annualized)")
# 
# d %>% mutate(rate=round(as.numeric(rate),1)) %>% arrange(agegroup,-rate) %>% group_by(agegroup) %>% mutate(rank = 1:length(deaths)) %>% filter(cause == "Covid-19 (25 months)")
# d %>% mutate(rate=round(as.numeric(rate),1)) %>% arrange(agegroup,-deaths) %>% group_by(agegroup) %>% mutate(rank = 1:length(deaths)) %>% filter(cause == "Covid-19 (25 months)")
# 
# d %>% mutate(rate=round(as.numeric(rate),2)) %>% arrange(agegroup,-deaths)
# 
# d_us_grouped3$cause = "Covid-19 (cumulative)"
# d_us_grouped3_annualized$cause = "Covid-19 (annualized)"
# d = rbind(d_us_grouped3,d_us_grouped3_annualized) %>% select(rate=per100K,cause,agegroup,deaths=covid) 

# # lt1 = rbind(fread("~/Downloads/wonder-lt1.txt",nrows=15) %>% select(cause=`15 Leading Causes of Death (Infants)`,rate=`Crude Rate`),
# #             d_us_grouped %>% filter(agegroup == "[0,1)") %>% select(rate=per100K) %>% mutate(cause = "COVID"))
# # data1to4 = rbind(fread("~/Downloads/wonder-1to4.txt",nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`),
# #                  d_us_grouped %>% filter(agegroup == "[1,5)") %>% select(rate=per100K) %>% mutate(cause = "COVID"))
# # data5to14 = rbind(fread("~/Downloads/wonder-5to14.txt",nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`),
# #                   d_us_grouped %>% filter(agegroup == "[5,15)") %>% select(rate=per100K) %>% mutate(cause = "COVID"))
# # data15to24 = rbind(fread("~/Downloads/wonder15to24.txt",nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`),
# #                    d_us_grouped %>% filter(agegroup == "[15,25)") %>% select(rate=per100K) %>% mutate(cause = "COVID"))
# 
# print(lt1 %>% mutate(rate=round(rate,2)) %>% arrange(-rate))
# print(data1to4 %>% mutate(rate=round(rate,2)) %>% arrange(-rate))
# print(data5to14 %>% mutate(rate=round(rate,2)) %>% arrange(-rate))
# print(data15to24 %>% mutate(rate=round(rate,2)) %>% arrange(-rate))
# 
# 
# fread("~/Downloads/singleyear.csv",nrows=15) %>% select(Age="Single-Year Ages",rate=`Crude Rate`)
# 
# fread("~/Downloads/icd113.txt",nrows=113) %>% select(cause=`ICD-10 113 Cause List`,rate=`Crude Rate`,deaths=Deaths) %>% order(-deaths)
