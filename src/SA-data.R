library(tidyverse)
library(here)
library(readxl)
library(stringi)
library(lubridate)

#----------------------------------------
# South Africa - population Age distribution
#----------------------------------------
pop_sa_func <- function() {
  pop_data <- read_excel(here("data/raw_data/SA_projection_by population_sex_age_2002_2021.xlsx"),sheet=2,skip=4) 
  
  pop_data <- pop_data %>% 
    select("1 year age bands", "Population" ) %>% 
    rename(Age = "1 year age bands") %>%
    mutate(Age = ifelse(Age=="80+", 80, Age))
  
  pop_df <- pop_data %>%
    group_by(Age) %>%
    summarize(pop_age=sum(Population)) %>%
    mutate(Age = as.numeric(Age)) %>%
    arrange(Age)
  
  pop_df <- data.frame(pop_df)
  
  # assume even distributions among 80+
  pop_80 <- pop_df$pop_age[80] / 20
  append_80 <- data.frame(Age = c(80:99), pop_age = rep(pop_80, 20))
  
  pop_df <- pop_df %>% slice(-nrow(pop_df))
  pop_df <- rbind(pop_df, append_80)
  
  # if (sum(pop_df$pop_age) == sum(pop_data$Population)) {
  #   print("the totals are correct")
  # } else {
  #   print("the totals are wrong")
  # }
  
  return(pop_df)
}

sa_pop <- pop_sa_func()

head(sa_pop)

sum(sa_pop$pop_age)/1000000  # should be around 60 mio

saveRDS(sa_pop, here("data/sa_pop.rds"))

