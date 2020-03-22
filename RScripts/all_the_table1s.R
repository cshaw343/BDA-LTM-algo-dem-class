#---- Package Loading, Options, and Seed Setting ----
if (!require("pacman")){
  install.packages("pacman", repos = 'http://cran.us.r-project.org')
}

p_load("here", "tidyverse", "magrittr", "future.apply", "readr", "haven",
       "lubridate", "gmodels", "tidyselect")

options(scipen = 999)
set.seed(20200107)

#---- Read in the HRS data (wave 12) ----
#list of variables to read in from HRS data:
# ID, Age, Sex/Gender, Race/Ethnicity, Years of education

vars = c("HHIDPN", "R13AGEY_B", "RAGENDER", "RARACEM", "RAHISPAN", "RAEDYRS")

HRS_data <- read_sas(paste0("/Users/CrystalShaw/Box/NIA_F31_April2020/Data/",
                            "HRS/HRS RAND/randhrs1992_2016v1_SAS_data/",
                            "randhrs1992_2016v1.sas7bdat"),
                     n_max = Inf, col_select = all_of(vars)) %>%
  mutate_at("HHIDPN", as.character)


HRS_clean <- HRS_data %>%
  #Filter out those younger than 50
  filter(R13AGEY_B >= 50) %>%
  #Remove those missing race data
  filter(!is.na(RARACEM)) %>%
  #My age categories
  mutate_at("R13AGEY_B", floor) %>%
  mutate("my_age_cat" = case_when(R13AGEY_B %in% seq(50, 54) ~ "50-54",
                                  R13AGEY_B %in% seq(55, 59) ~ "55-59",
                                  R13AGEY_B %in% seq(60, 64) ~ "60-64",
                                  R13AGEY_B %in% seq(65, 69) ~ "65-69",
                                  R13AGEY_B %in% seq(70, 74) ~ "70-74",
                                  R13AGEY_B %in% seq(75, 79) ~ "75-79",
                                  R13AGEY_B %in% seq(80, 84) ~ "80-84",
                                  R13AGEY_B %in% seq(85, 89) ~ "85-89",
                                  R13AGEY_B >= 90 ~ "90+")) %>%
  #My education categories
  mutate("my_edu_cat" = case_when(RAEDYRS %in% seq(0, 8) ~ "0-8",
                                  RAEDYRS %in% seq(9, 12) ~ "9-12",
                                  RAEDYRS %in% seq(13, 16) ~ "13-16",
                                  RAEDYRS > 16 ~ ">16",
                                  TRUE ~ "missing"))

#---- Read in NHATS data (wave 1 and 5 (baseline data), 6) ----
#list of variables to read in from HRS data:
# ID, Age, Sex/Gender, Race/Ethnicity, Years of education
w1_vars = c("spid", "el1higstschl")
w5_vars = c("spid", "el5higstschl")
w6_vars = c("spid", "r6d2intvrage", "r5dgender", "rl5dracehisp")

NHATS_w1 <- read_sas(paste0("/Users/CrystalShaw/Box/NIA_F31_April2020/Data/",
                            "NHATS/Round 1/NHATS_ROUND_1_SP_File.sas7bdat"),
                     n_max = Inf, col_select = all_of(w1_vars)) %>%
  mutate_at("spid", as.character)

NHATS_w5 <- read_sas(paste0("/Users/CrystalShaw/Box/NIA_F31_April2020/Data/",
                            "NHATS/Round 5/NHATS_ROUND_5_SP_File_V2.sas7bdat"),
                     n_max = Inf, col_select = all_of(w5_vars)) %>%
  mutate_at("spid", as.character)

NHATS_w6 <- read_sas(paste0("/Users/CrystalShaw/Box/NIA_F31_April2020/Data/",
                            "NHATS/Round 6/NHATS_ROUND_6_SP_File_V2.sas7bdat"),
                     n_max = Inf, col_select = all_of(w6_vars)) %>%
  mutate_at("spid", as.character)

NHATS_clean <- left_join(NHATS_w6, NHATS_w5, by = "spid") %>%
  left_join(., NHATS_w1, by = "spid") %>%
  #Remove those who didn't take the survey based on age variable
  filter(!is.na(r6d2intvrage) & r6d2intvrage != -1) %>%
  #Remove those who don't know/refuse race/ethnicity question
  filter(!(rl5dracehisp %in% c(5, 6))) %>%
  #Carry forward education data from wave 1
  mutate("elhigstschl" = case_when(el5higstschl == -1 ~ el1higstschl,
                                   TRUE ~ el5higstschl)) %>%
  #Remove those not in the survey based on education variable
  filter(elhigstschl != -1) %>%
  #My education categories
  mutate("my_edu_cat" = case_when(elhigstschl %in% c(-8, -7, 1, 2) ~ "0-8",
                                  elhigstschl %in% c(3, 4) ~ "9-12",
                                  elhigstschl %in% c(5, 6, 7, 8) ~ "13-16",
                                  elhigstschl == 9 ~ ">16",
                                  elhigstschl == -9 ~ "missing"))


#---- Read in HCAP data (wave 1) ----


#---- Filling in F31 Research Strategy Table 2 ----
nrow(HRS_clean)
table(HRS_clean$my_age_cat)
table(HRS_clean$my_age_cat)/nrow(HRS_clean)
# 1 = Male; 2 = Female
table(HRS_clean$RAGENDER)
table(HRS_clean$RAGENDER)/nrow(HRS_clean)
# Race: 1 = White; 2 = Black; 3 = Other
CrossTable(HRS_clean$RARACEM, HRS_clean$RAHISPAN)
table(HRS_clean$my_edu_cat)
table(HRS_clean$my_edu_cat)/nrow(HRS_clean)

nrow(NHATS_clean)
table(NHATS_clean$r6d2intvrage)
table(NHATS_clean$r6d2intvrage)/nrow(NHATS_clean)
# 1 = Male; 2 = Female
table(NHATS_clean$r5dgender)
table(NHATS_clean$r5dgender)/nrow(NHATS_clean)
# 1 = Non-hispanic White; 2 = Non-hispanic Black; 3 = Non-hispanic Other;
# 4 = Hispanic
table(NHATS_clean$rl5dracehisp)
table(NHATS_clean$rl5dracehisp)/nrow(NHATS_clean)
table(NHATS_clean$my_edu_cat)
table(NHATS_clean$my_edu_cat)/nrow(NHATS_clean)
