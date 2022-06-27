library(data.table)
library(tidyverse)
library(here)
library(readxl)

# US population data comes from https://www.census.gov/data/tables/2020/demo/popest/2020-demographic-analysis-tables.html
#         "xls Table 1. Total U.S. Resident Population by Age, Sex, and Series: April 1, 2020 (In thousands)"  
# pop_us <- read_excel(here("data/raw_data/US_pop.xlsx"), range = "A4:E90") %>% 
#   select(c(1,4)) %>%
#   rename(Age = Total, pop_age ="332601") %>%
#   mutate(Age = ifelse(Age == "85+", "85", Age),
#          Age = as.numeric(Age),
#          pop_age = as.numeric(pop_age)* 1000)

pop_us = read.csv(here("data/nc-est2021-agesex-res.csv")) %>% filter(SEX == 0) %>% 
  mutate(Age=AGE,pop_age = POPESTIMATE2021) %>%
   select(Age,pop_age) %>% filter(Age <= 100)
head(pop_us)


## For Tables: compare to other leading causes of death: ages < 1, 1-4, 5-9, 10-14, 15-19
covid = rbind(
  fread(here("data/raw_data/2021-22/0.txt"),nrows=1) %>% select(cause=`Underlying Cause of death`,deaths=Deaths) %>% mutate(agegroup="[0,1)"),
  fread(here("data/raw_data/2021-22/1to4.txt"),nrows=1) %>% select(cause=`Underlying Cause of death`,deaths=Deaths) %>% mutate(agegroup="[1,5)"),
  fread(here("data/raw_data/2021-22/5to9.txt"),nrows=1) %>% select(cause=`Underlying Cause of death`,deaths=Deaths) %>% mutate(agegroup="[5,10)"),
  fread(here("data/raw_data/2021-22/10to14.txt"),nrows=1) %>% select(cause=`Underlying Cause of death`,deaths=Deaths) %>% mutate(agegroup="[10,15)"),
  fread(here("data/raw_data/2021-22/15to19.txt"),nrows=1) %>% select(cause=`Underlying Cause of death`,deaths=Deaths) %>% mutate(agegroup="[15,20)"))

wonder = rbind(
  fread(here("data/raw_data/wonder0.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[0,1)"),
  fread(here("data/raw_data/wonder1-4.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[1,5)"),
  fread(here("data/raw_data/wonder5-9.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[5,10)"),
  fread(here("data/raw_data/wonder10-14.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[10,15)"),
  fread(here("data/raw_data/wonder15-19.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[15,20)"))

wonder$rate = as.numeric(wonder$rate)
wonder = wonder[complete.cases(wonder),]

## rounding issues
rate2 = wonder$deaths / wonder$pop * 100000
which(wonder$rate != round(rate2,2))
wonder[which(round(wonder$rate,1) != round(rate2,1)),]
wonder$rate = round(rate2,1)

all_deaths = as.numeric(fread(here("data/raw_data/2021-22/all-ages.txt"),nrows=1) %>% select(Deaths))
covid = left_join(covid, pop_us %>% filter(Age < 20) %>% group_by(agegroup=cut(Age,c(0,1,5,10,15,20,100),right=F)) %>%   
  summarise(pop = sum(pop_age)),by="agegroup")

sum(covid$deaths) / sum(covid$pop) * 100000
all_deaths / sum(pop_us$pop_age) * 100000

covid$rate = covid$deaths / covid$pop * 100000  


agegroups = levels((pop_us %>% filter(Age < 20) %>% group_by(agegroup=cut(Age,c(0,1,5,10,15,20,100),right=F)))$agegroup)

covid.plot = covid
covid.plot$agegroup = factor(covid.plot$agegroup,levels=agegroups[1:5],
                        labels = c("< 1 year olds","1-4 year olds","5-9 year olds","10-14 year olds","15-19 year olds"))

g = ggplot(covid.plot) + 
  geom_bar(aes(x=agegroup,y=rate), stat="identity",
           position = position_dodge2(preserve='single'),fill="maroon")  
g = g + scale_y_continuous(expand=c(0,0),limits=c(0,4))
g = g + labs(y = "Covid-19 deaths per 100,000\n", x="\nAge group") 
g = g + ggtitle("Covid-19 death rate in the US: April 1, 2021-March 31st, 2022")
g = g + theme_bw()
g
ggsave(g,filename = here("figures/US-death-rate-age-groups-21-22.png"),width=7,height=4)
covid

d = rbind(covid,wonder)

pct = d %>%
  group_by(agegroup) %>% mutate(percent = deaths/sum(deaths)) %>% arrange(-percent) 
pct %>% filter(cause ==  "COVID-19") %>% arrange(agegroup)

d = d %>% mutate()
for(ag in unique(d$agegroup)) {
  print(sprintf("Age group: %s",as.character(ag)))
  print(d %>% filter(agegroup == ag)  %>% mutate(rate=round(as.numeric(rate),1)) %>% arrange(-rate) %>% select(cause,rate,deaths))
  print(d %>% filter(agegroup == ag)  %>% mutate(rate=round(as.numeric(rate),4)) %>% arrange(-deaths) %>% select(cause,rate,deaths))
  
  print(max(diff((d %>% filter(agegroup == ag)  %>% mutate(rate=round(as.numeric(rate),1)) %>% arrange(-deaths) %>% select(cause,rate,deaths))$rate),na.rm=T))
}

d$agegroup = factor(d$agegroup,levels=agegroups)
d$cause[d$cause == "COVID-19"] = "#COVID-19 (U07.1)"


write.csv(d %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
  mutate(rank =  rank(-rate,ties.method="min"))  %>% filter(rank <= 10) %>% mutate(percent=round(deaths/sum(deaths) * 100,1)) %>% 
  arrange(agegroup,-deaths) %>% select(cause,rate,deaths,rank,percent,agegroup),
  file=here("results/WONDER-agegroups-2021-22.csv"),row.names=F)

## text in manuscript:
## ranks are: #7 (<1 year old), #7 (1-4 year olds), #6 (5-9 year olds), #6 (10-14 year olds), and #5 (15-19 year olds). 
d %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
  mutate(rank =  rank(-rate,ties.method="min")) %>% arrange(agegroup,-deaths) %>% filter(grepl("COVID",cause)) %>% arrange(cause)

### compare to other leading causes of death: ages 0-19 together

covid0_19 = fread(here("data/raw_data/2021-22/0to19.txt"),nrows=1) %>%
  select(cause=`Underlying Cause of death`,deaths=Deaths) %>% mutate(agegroup="[0,20)")

covid0_19$pop = as.numeric(pop_us %>% filter(Age < 20) %>% summarise(pop = sum(pop_age)))
covid0_19$rate = covid0_19$deaths / covid0_19$pop * 100000

d = rbind(
  fread(here("data/raw_data/wonder0-19.txt"),nrows=15) %>% select(cause=`15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% 
    mutate(agegroup="[0,20)"),covid0_19)
which(round(d$rate,1) != round(d$deaths/d$pop*100000,1))
d

# 
pct = d %>%
  group_by(agegroup) %>% mutate(percent = deaths/sum(deaths)) %>% arrange(-percent) 
pct %>%  arrange(agegroup)

# 
# g = ggplot(pct, aes(x="", y=percent, fill=cause))+
#   geom_bar(width = 1, stat = "identity") +
#   geom_text(aes(label = scales::percent(percent,1)),
#             position = position_stack(vjust = 0.5))
# 
# g + coord_polar("y", start=0) + scale_fill_viridis_d()

d$cause[d$cause == "COVID-19"] = "#COVID-19 (U07.1)"

print(d %>% mutate(rate=round(as.numeric(rate),2),
                                               rank =  rank(-rate,ties.method="min")) %>% arrange(-rate))
write.csv(d %>% filter(agegroup == "[0,20)") %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
            mutate(rank =  rank(-rate,ties.method="min")) %>% 
            filter(rank <= 10) %>% mutate(percent=round(deaths/sum(deaths) * 100,1)) %>% 
            arrange(-deaths) %>% select(cause,rate,deaths,rank,percent), file = here("results/WONDER-0-19.csv"),row.names=F)

d = rbind(
  fread(here("data/raw_data/wonder-infectious-0-19.txt"),nrows=27) %>% select(cause=`ICD-10 113 Cause List`,rate=`Crude Rate`,deaths=Deaths) %>% mutate(agegroup="[0,20)"),covid0_19)
write.csv(d %>% filter(agegroup == "[0,20)") %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
            mutate(rank =  rank(-rate,ties.method="min")) %>% arrange(agegroup,-deaths),file = here("results/WONDER-0-19-infectious.csv"),row.names=F)
