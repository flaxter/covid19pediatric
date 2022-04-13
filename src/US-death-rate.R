library(tidyverse)
library(here)

#----------------------------------------
# Read pre-processed data
#----------------------------------------

us_covid <- readRDS( here("data/us_covid.rds"))

us_pop <- readRDS( here("data/us_pop.rds"))

#----------------------------------------
# death rate per 100K
#----------------------------------------

us_dr <- left_join(us_covid, us_pop, by="Age") 

us_dr <- us_dr %>%
  mutate(per100K = covid / pop_age * 100000)

(g <- ggplot(filter(us_dr, Age < 20)) + 
    geom_bar(aes(x=Age,y=per100K), stat="identity",
             position = position_dodge2(preserve='single')) + 
    labs(y = "Deaths per 100K") + 
    ggtitle("US COVID-19 death rate") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0, 0)) + 
    theme_bw())


