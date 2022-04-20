library(tidyverse) 
library(here)
library(lubridate)

#----------------------------------------
# Brazil hospitalization/death data
#----------------------------------------

## You will need to download the original data and put it in the data folder in covid19pediatric: 
## https://opendatasus.saude.gov.br/dataset/srag-2021-e-2022 (2021/22)
## https://opendatasus.saude.gov.br/dataset/srag-2020 (2020)
## scroll down to the CSV link and click Explorar -> Baixar
## Update the dates below depending on the vintage you download:
sivep_2020 <- read_csv2(here("data/raw_data/INFLUD20-21-03-2022.csv"))
sivep_2021 <- read_csv2(here("data/raw_data/INFLUD21-21-03-2022.csv"))
sivep_2022 <- read_csv2(here("data/raw_data/INFLUD22-21-03-2022.csv"))
cols_of_interest <- c("SG_UF", "SG_UF_NOT", "ID_MUNICIP", "EVOLUCAO", "DT_SIN_PRI", "DT_EVOLUCA", "HOSPITAL",
                     "DT_INTERNA", "NU_IDADE_N", "CS_SEXO", "CLASSI_FIN", "DT_NASC",
                     "UTI", "DT_ENTUTI", "DT_SAIDUTI", "SUPORT_VEN", "SATURACAO")
sivep_2020 <- sivep_2020[, cols_of_interest]
sivep_2021 <- sivep_2021[, cols_of_interest]
sivep_2022 <- sivep_2022[, cols_of_interest]
df_SIVEP_original <- rbind(sivep_2020, sivep_2021, sivep_2022)
rm(sivep_2020); rm(sivep_2021); rm(sivep_2022); rm(cols_of_interest)

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
# gamma_start <- as.Date("2021-02-21")
# gamma_end <- as.Date("2021-08-15")
# delta_start <- as.Date("2021-09-01")
# delta_end <- as.Date("2021-12-20")
# omicron_start <- as.Date("2021-01-05")

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
  # mutate(variant = case_when(DateSymptoms >= gamma_start & DateSymptoms <= gamma_end ~ "Gamma_Period",
  #                            DateSymptoms >= delta_start & DateSymptoms <= delta_end ~ "Delta_Period",
  #                            DateSymptoms >= omicron_start & DateSymptoms <= Sys.Date() ~ "Omicron_Period",
  #                            TRUE ~ NA_character_)) %>%
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

rm(list=setdiff(ls(), "df_SIVEP_proc"))

#saveRDS(df_SIVEP_proc, here("data/df_SIVEP.rds"))
saveRDS(df_SIVEP_proc, here("data/df_SIVEP_all.rds"))

#----------------------------------------
# Brazil - population Age distribution
#----------------------------------------

pop_bra_func <- function() {
  library(readxl)
  pop_data <- read_excel(here("data/raw_data/BRA_projecoes_2018_populacao_idade_simples_2010_2060_20201209.xls"),sheet=1,skip=195) 
  
  pop_data <- pop_data %>%
    select(IDADE,  "2020") %>%
    slice(-c(1, 93:95))
  
  pop_df <-  pop_data %>% 
    rename(Age = IDADE, pop_age = "2020")  %>%
    mutate(Age = ifelse(Age=="90+", 90, Age),
           Age = as.numeric(Age)) %>%
    arrange(Age)
  
  pop_df <- data.frame(pop_df)
  
  # assume even distributions among 90+
  pop_90 <- pop_df$pop_age[91] / 10
  append_90 <- data.frame(Age = c(90:99), pop_age = rep(pop_90, 10))
  
  pop_df <- pop_df %>% slice(-nrow(pop_df))
  pop_df <- rbind(pop_df, append_90)
  
  if (sum(pop_df$pop_age) == sum(pop_data["2020"])) {
    print("the totals are correct")
  } else {
    print("the totals are wrong")
  }
  
  return(pop_df)
}

pop_bra <- pop_bra_func()

saveRDS(pop_bra, here("data/pop_bra.rds"))


