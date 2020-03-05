#---- Package Loading, Options, and Seed Setting ----
if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')

  p_load("here", "tidyverse", "magrittr", "future.apply", "readr", "haven",
         "lubridate")
}
options(scipen = 999)
set.seed(20200107)

#---- Read in the HRS data (wave 12) ----
#list of variables to read in from HRS data:
# ID, Age, Sex/Gender, Race/Ethnicity, Years of education

vars = c("HHIDPN", "R13AGEY_B", "RAGENDER", "RARACEM", "RAEDYRS")

HRS_data <- read_sas(paste0("/Users/CrystalShaw/Box/NIA_F31_April2020/Data/",
                            "HRS/HRS RAND/randhrs1992_2016v1_SAS_data/",
                            "randhrs1992_2016v1.sas7bdat"),
                     n_max = Inf, col_select = vars) %>%
  mutate_at("HHIDPN", as.character)
