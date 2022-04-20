library(tidyverse)
library(here)

#----------------------------------------
# Read pre-processed data
#----------------------------------------

df_SIVEP <- readRDS(here("data/df_SIVEP_all.rds"))

pop_bra <- readRDS( here("data/pop_bra.rds"))

#----------------------------------------
# Death rate
#----------------------------------------

d <- df_SIVEP %>%  # filter(time > 2) %>%
  filter(Outcome == "Death_COVID") %>%
  select(Age, variant) %>%
  mutate(Age = floor(Age) ) 

d_aggr_bra <- d %>%
  group_by(Age, variant) %>%
  summarize(num=n()) 

d_bra <- left_join(d_aggr_bra, pop_bra, by="Age")
  
d_bra_20 <- d_bra %>% filter(Age < 20) %>%
  mutate(Wave = case_when(variant=='Delta_Period' ~ 'Delta',
                          variant=='Gamma_Period' ~ 'Gamma',
                          variant=='Omicron_Period' ~ 'Omicron'),
         Wave = factor(Wave),
         Wave = relevel(Wave, ref='Delta')) %>%
  group_by(Age = cut(Age,c(0,1,5,10,19),include.lowest=T,right=F,
                     labels=c("<1 year", "1-4","5-9", "10-19 years")), Wave) %>% 
  summarize(per100K = sum(num) / sum(pop_age) * 100000,
            deaths_num = sum(num)) 


bra_covid <- d %>%
  group_by(Age=floor(Age)) %>%
  summarize(covid=n())  %>% 
  filter(Age < 100)

bra_dr <- left_join(bra_covid, pop_bra, by="Age")
sum(bra_dr$pop_age) # should be about 212

bra_dr <- bra_dr %>%
  mutate(per100K = covid / pop_age * 100000)
head(bra_dr)

(g <- ggplot(filter(bra_dr, Age < 20)) + 
    geom_bar(aes(x=Age,y=per100K), stat="identity",
             position = position_dodge2(preserve='single')) + 
    labs(y = "Deaths per 100K") + 
    ggtitle("Brazil COVID-19 death rate") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0, 0)) + 
    theme_bw())


