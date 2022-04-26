library(tidyverse)
library(here)
source(here("src/pal.R"))

#----------------------------------------
# Read pre-processed data
#----------------------------------------

d_agg <- readRDS(here("data/sa_covid_agg.rds"))

pop_sa <- readRDS( here("data/sa_pop.rds"))

#----------------------------------------
# South Africa
#----------------------------------------

head(d_agg)

# Table 1B
d_table <- d_agg %>%  filter(wave == 3 | wave == 4) %>%
  mutate(Wave = case_when(wave==1 ~ 'Wild-type',
                          wave==2 ~ 'Beta',
                          wave==3 ~ 'Delta',
                          wave==4 ~ 'Omicron')
  ) %>% 
  group_by(Age = cut(Age,c(0,1,5,10,20,100),include.lowest=T,right=F,
                     labels=c("<1 year", "1-4","5-9", "10-19 years","20+")), Wave) %>% 
  summarize(hospit_num=sum(num))
d_table
xtabs(hospit_num ~ Age + Wave , data= d_table)

# Wave
# Age           Delta Omicron
# <1 year       944    1535
# 1-4           819    1452
# 5-9           512     931
# 10-19 years  2552    2575
# 20+         70837   35742

d_aggr_sa <- d_agg %>%
  group_by(Age, wave) %>%
  summarize(num=sum(num))

sum(d_aggr_sa$num) == nrow(d_agg)

d_sa <- left_join(d_aggr_sa, pop_sa, by="Age")
head(d_sa)
sum(d_sa$pop_age)/4/1000000
d_sa_20 <- d_sa %>% filter(Age < 20) %>%
  mutate(Wave = case_when(wave==1 ~ 'Wild-type',
                          wave==2 ~ 'Beta',
                          wave==3 ~ 'Delta',
                          wave==4 ~ 'Omicron'),
         Wave = factor(Wave),
         Wave = relevel(Wave, ref='Delta')) %>%
  group_by(Age = cut(Age,c(0,1,5,10,19),include.lowest=T,right=F,
                     labels=c("<1 year", "1-4","5-9", "10-19 years")), Wave) %>% 
  summarize(per100K = sum(num) / sum(pop_age) * 100000,
            hospit_num = sum(num)) 

# Faigure 1b
(p <- ggplot(d_sa_20 %>% filter(Wave %in% c("Delta","Omicron"))) + 
    geom_bar(aes(x=Age,y=per100K,fill=Wave), stat="identity",
                 position = position_dodge2(preserve='single')) +
    ggtitle("South Africa hospitalization rate") +
    labs(y = "Hospitalizations per 100,000", x="Age") +
    scale_y_continuous(expand=c(0,0),limits=c(0,150)) +
    theme_bw() +
    scale_fill_manual( values = pal(2)))

