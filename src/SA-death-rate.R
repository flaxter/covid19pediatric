library(tidyverse)
library(here)
source(here("src/pal.R"))

#----------------------------------------
# Read pre-processed data
#----------------------------------------

d <- readRDS(here("data/sa_covid.rds"))

pop_sa <- readRDS( here("data/sa_pop.rds"))

#----------------------------------------
# South Africa
#----------------------------------------

d <- d %>% 
  filter(wave %in% c(1,2,3,4)) %>% 
  select(Age, wave, status)

head(d)

# Table 1B
d_table <- d %>%  filter(wave == 3 | wave == 4) %>%
  mutate(Wave = case_when(wave==1 ~ 'Wild-type',
                          wave==2 ~ 'Beta',
                          wave==3 ~ 'Delta',
                          wave==4 ~ 'Omicron')
  ) %>% 
  group_by(Age = cut(Age,c(0,1,5,10,20,100),include.lowest=T,right=F,
                     labels=c("<1 year", "1-4","5-9", "10-19 years","20+")), Wave, status) %>% 
  summarize(death_num=n())
d_table
xtabs(death_num ~ Age + Wave , data= filter(d_table, status ==1))

d_aggr_sa <- d %>% 
  filter(status==1) %>%
  group_by(Age, wave) %>%
  summarize(num=n())

d_sa <- left_join(d_aggr_sa, pop_sa, by="Age")
head(d_sa)
sum(d_sa$pop_age)/4/1000000

d_sa <- d_sa %>%
  mutate(Wave = case_when(wave==1 ~ 'Wild-type',
                          wave==2 ~ 'Beta',
                          wave==3 ~ 'Delta',
                          wave==4 ~ 'Omicron'),
         Wave = factor(Wave),
         Wave = relevel(Wave, ref='Delta'),
         per100K = round(num / pop_age * 100000,2))
head(d_sa)

(p <- ggplot(d_sa %>% filter(Age <= 19)) +
 geom_bar(aes(x=Age,y=per100K), stat="identity") +
 scale_x_continuous(breaks = seq(0,20,2),expand=c(0,0)) + 
 scale_y_continuous(expand=c(0,0),limits=c(0,15)) + 
 ggtitle("South Africa death rate")  + 
 labs(y = "Deaths per 100,000\n", x="\nAge") +
 theme_bw())


