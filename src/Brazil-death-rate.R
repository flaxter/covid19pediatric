library(tidyverse)
library(here)
source(here("src/pal.R"))

#----------------------------------------
# Read pre-processed data
#----------------------------------------

df_SIVEP <- readRDS(here("data/df_SIVEP.rds"))

pop_bra <- readRDS( here("data/pop_bra.rds"))

#----------------------------------------
# Hospitalization rate
#----------------------------------------

d <- df_SIVEP %>%  # filter(time > 2) %>%
  select(Age, variant) %>%
  mutate(Age = floor(Age) )
d_aggr_bra <- d %>%
  group_by(Age, variant) %>%
  summarize(num=n()) 

sum(d_aggr_bra$num) == nrow(df_SIVEP)

d_aggr_bra <- d_aggr_bra %>% filter(Age < 100)

head(d_aggr_bra)
table(d_aggr_bra$variant)

d_bra <- left_join(d_aggr_bra, pop_bra, by="Age")
head(d_bra)
sum(d_bra$pop_age)/3/1000000 # should be around 212

d_bra_20 <- d_bra %>% filter(Age < 20) %>%
  mutate(Wave = case_when(variant=='Delta_Period' ~ 'Delta',
                          variant=='Gamma_Period' ~ 'Gamma',
                          variant=='Omicron_Period' ~ 'Omicron'),
         Wave = factor(Wave),
         Wave = relevel(Wave, ref='Delta')) %>%
  group_by(Age = cut(Age,c(0,1,5,10,19),include.lowest=T,right=F,
                     labels=c("<1 year", "1-4","5-9", "10-19 years")), Wave) %>% 
  summarize(per100K = sum(num) / sum(pop_age) * 100000) 

(p <- ggplot(d_bra_20 %>% filter(Wave != "Gamma")) + 
    geom_bar(aes(x=Age,y=per100K,fill=Wave), stat="identity",
                 position = position_dodge2(preserve='single')) +
    ggtitle("Brazil hospitalization rate") +
    labs(y = "Hospitalizations per 100,000", x="Age") +
    scale_y_continuous(expand=c(0,0),limits=c(0,301)) +
    theme_bw()+
    scale_fill_manual( values = pal(2)))

