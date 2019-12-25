#---- Package Loading ----
if (!require("pacman"))
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("here", "haven", "tidyverse", "magrittr")

options(scipen = 999)

#---- Read in the data ----
#Need to use HRS data until ADAMS becomes available
#number of rows in the dataset to read in
n_max = Inf

#Read in imputed scores from Wu logit algorithm... temporary gold standard
#These imputed scores start in Wave 3
#If predicted dementia probability >= 0.5, classify as demented
Wu_algorithm <- read_sas(here(
  "Data", "Wu_logit_HRS_imputed_mem_scores", "dpmemimp_aug2018.sas7bdat"),
  n_max = n_max)

#list of variables to read in from HRS data:
  # ID, total word recall, serial7, backward counting
#stop at Wave 12 because that's all I have from RAND
waves <- seq(from = 3, to = 12, by = 1)
word_recall_vars <- paste0("R", waves, "TR20")
serial7_vars <- paste0("R", waves, "SER7")
backwards_count_vars <- paste0("R", waves, "BWC20")

vars = c("HHIDPN", word_recall_vars, serial7_vars, backwards_count_vars)

HRS_data <- read_sas(here(
  "Data", "randhrs1992_2016v1_SAS_data", "randhrs1992_2016v1.sas7bdat"),
  n_max = n_max, col_select = vars) %>%
  mutate_at("HHIDPN", as.character)

#---- Remove people missing ALL test scores ----
#Removes 8376 people (42053 --> 33677)
HRS_data$num_missing <- rowSums(is.na(HRS_data[, -1]))
HRS_data$missing_all <- (HRS_data$num_missing == 30)*1
HRS_data %<>% filter(missing_all == 0)

#---- Remove those who don't appear in Wu algorithm data ----
#Removes 3803 people (33677 --> 29874)
#Create HHIDPN variable for Wu data
Wu_algorithm$HHID = str_remove(Wu_algorithm$HHID, "^0+")
Wu_algorithm %<>% unite("HHIDPN", c("HHID", "PN"), sep = "")

HRS_data <- HRS_data[-which(!(HRS_data$HHIDPN %in% Wu_algorithm$HHIDPN)), ]

#---- L-K-W summary scores ----



