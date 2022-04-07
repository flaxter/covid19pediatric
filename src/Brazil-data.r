# Load required libraries
library(data.table); library(ggplot2); library(ggsci); library(viridis); library(gtools)
library(MGLM); library(fitdistrplus); library(actuar); library(posterior); library(bayesplot)
library(openxlsx); library(lubridate); library(tidyr); library(repr); library(ggpubr)
library(readr); library(tidyverse); library(survival); library(survminer); library(here)
library(mgcv); library(visreg); library(locfit)

library(here)

## You will need to download the original data and put it in the data folder in covid19pediatric: 
## https://opendatasus.saude.gov.br/dataset/srag-2021-e-2022 (2021/22)
## https://opendatasus.saude.gov.br/dataset/srag-2020 (2020)
## scroll down to the CSV link and click Explorar -> Baixar
## Update the dates below depending on the vintage you download:
sivep_2020 <- read_csv2(here("data/INFLUD20-21-03-2022.csv"))
sivep_2021 <- read_csv2(here("data/INFLUD21-21-03-2022.csv"))
sivep_2022 <- read_csv2(here("data/INFLUD22-21-03-2022.csv"))
cols_of_interest <- c("SG_UF", "SG_UF_NOT", "ID_MUNICIP", "EVOLUCAO", "DT_SIN_PRI", "DT_EVOLUCA", "HOSPITAL",
                     "DT_INTERNA", "NU_IDADE_N", "CS_SEXO", "CLASSI_FIN", "DT_NASC",
                     "UTI", "DT_ENTUTI", "DT_SAIDUTI", "SUPORT_VEN", "SATURACAO")
sivep_2020 <- sivep_2020[, cols_of_interest]
sivep_2021 <- sivep_2021[, cols_of_interest]
sivep_2022 <- sivep_2022[, cols_of_interest]
df_SIVEP_original <- rbind(sivep_2020, sivep_2021, sivep_2022)
rm(sivep_2020); rm(sivep_2021); rm(sivep_2022)

# # get only suspected and confirmed deaths
df_SIVEP <- df_SIVEP_original[which(df_SIVEP_original$CLASSI_FIN %in% c(4,5)),] #4 is suspected, 5 is confirmed
df_SIVEP$SRAG <- ifelse(df_SIVEP$CLASSI_FIN %in% c(4,5), 'COVID', 'Other')

# transform and harmonise dates to same format
table(df_SIVEP$DT_EVOLUCA, useNA = "ifany")
table(df_SIVEP$DT_INTERNA, useNA = "ifany")

df_SIVEP$DT_EVOLUCA <- dmy(df_SIVEP$DT_EVOLUCA)  # date of outcome: death or cure
df_SIVEP$DT_INTERNA <- dmy(df_SIVEP$DT_INTERNA)  # date of admission
df_SIVEP$DT_SIN_PRI <- dmy(df_SIVEP$DT_SIN_PRI)  # date of symptoms
df_SIVEP$DT_NASC <- dmy(df_SIVEP$DT_NASC)  # date of birth
df_SIVEP$DT_ENTUTI <- dmy(df_SIVEP$DT_ENTUTI)  # date of ICU admission
df_SIVEP$DT_SAIDUTI <- dmy(df_SIVEP$DT_SAIDUTI)  # date of ICU exit

sum(is.na(df_SIVEP$EVOLUCAO)) # 159,150 without outcome (NA) + 75,000 missing (9)
sum(is.na(df_SIVEP$DT_EVOLUCA)) # 351,000 without date of outcome
sum(is.na(df_SIVEP$DT_INTERNA)) # 170,000 without date of admission
sum(is.na(df_SIVEP$DT_SIN_PRI)) # none without date of symptoms
# 
# # remove all dates that are too early
df_SIVEP_initial_remove <- df_SIVEP %>%
  filter(DT_EVOLUCA >= dmy("01-12-2019") | is.na(DT_EVOLUCA), # retain the NAs where they exist
         DT_SIN_PRI >= dmy("01-12-2019") | is.na(DT_SIN_PRI), # retain the NAs where they exist
         DT_INTERNA >= dmy("01-12-2019") | is.na(DT_INTERNA)) # retain the NAs where they exist

# # status processing
df_SIVEP_initial_remove$EVOLUCAO <- ifelse(df_SIVEP_initial_remove$EVOLUCAO %in% c(9), NA, df_SIVEP_initial_remove$EVOLUCAO) # changing 9 (unknown outcome) to NA
sum(is.na(df_SIVEP_initial_remove$EVOLUCAO)) # 159,150 without outcome (NA) + 75,000 missing (9)
table(df_SIVEP_initial_remove$EVOLUCAO, useNA = "ifany")

# rename the columns and filter by date to retain only Delta/Omicron period
gamma_start <- as.Date("2021-02-21")
gamma_end <- as.Date("2021-08-15")
delta_start <- as.Date("2021-09-01")
delta_end <- as.Date("2021-12-20")
omicron_start <- as.Date("2021-01-05")

sum(df_SIVEP_initial_remove$DT_SIN_PRI >= as.Date("2021-09-01"))
df_SIVEP_proc = df_SIVEP_initial_remove %>%
  rename(
    State = SG_UF,
    State_Notif = SG_UF_NOT,
    Municip_Notif = ID_MUNICIP,
    Admission = HOSPITAL,
    Outcome = EVOLUCAO,
    DateSymptoms = DT_SIN_PRI,
    DateAdmission = DT_INTERNA,
    DateOutcome = DT_EVOLUCA,
    DOB = DT_NASC,
    Sex = CS_SEXO,
    Srag = SRAG,
    ICU = UTI,
    ICU_entry_date = DT_ENTUTI,
    ICU_exit_date = DT_SAIDUTI,
    Low_Oxygen_Sats = SATURACAO) %>%
  mutate(time = DateOutcome - DateSymptoms) %>%
  filter(!is.na(Admission) & Admission == 1) %>%
  mutate(variant = case_when(DateSymptoms >= gamma_start & DateSymptoms <= gamma_end ~ "Gamma_Period",
                             DateSymptoms >= delta_start & DateSymptoms <= delta_end ~ "Delta_Period",
                             DateSymptoms >= omicron_start & DateSymptoms <= Sys.Date() ~ "Omicron_Period",
                             TRUE ~ NA_character_)) %>%
  mutate(Outcome = case_when(Outcome == 1 ~ "Cure",
                             Outcome == 2 ~ "Death_COVID",
                             Outcome == 3 ~ "Death_Other",
                             TRUE ~ "Unknown_Or_Missing")) %>%
  mutate(Outcome = ifelse((Outcome == 'Death_Other') & (CLASSI_FIN %in% c(4,5)), "Death_COVID", Outcome))

df_SIVEP_proc$Age = time_length(df_SIVEP_proc$DateSymptoms - df_SIVEP_proc$DOB, "years")
df_SIVEP_proc = df_SIVEP_proc %>% filter(Age >= 0 & Age <= 120)

sum(is.na(df_SIVEP_proc$DateSymptoms))
sum(is.na(df_SIVEP_proc$DateAdmission)) # 3500 missing date of admission
sum(is.na(df_SIVEP_proc$DateOutcome)) # 30000 missing date of outcome
sum(df_SIVEP_proc$Outcome == "Unknown_Or_Missing") # 26000 missing outcome
sum(is.na(df_SIVEP_proc$time)) # 30000 missing either date of symptoms or date of outcome

saveRDS(df_SIVEP_proc, here("data/df_SIVEP.rds"))
