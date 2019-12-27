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

#---- Remove people missing ALL Wu dementia probabilities ----
#Removes 3621 people (34032 --> 30411)
#Don't count the last variable because we are not using Wave 13
wu_demprobs_vars <- head(colnames(Wu_algorithm)[which(
  str_detect(colnames(Wu_algorithm), "dementpimp", negate = FALSE))], -1)

Wu_algorithm$num_missing_probs <-
  rowSums(is.na(Wu_algorithm[, wu_demprobs_vars]))
Wu_algorithm$missing_all_probs <-
  (Wu_algorithm$num_missing_probs == length(wu_demprobs_vars))*1
Wu_algorithm %<>% filter(missing_all_probs == 0)

#---- Remove those who don't appear in Wu algorithm data ----
#Removes 4453 people (33677 --> 29224)
#Create HHIDPN variable for Wu data
Wu_algorithm$HHID = str_remove(Wu_algorithm$HHID, "^0+")
Wu_algorithm %<>% unite("HHIDPN", c("HHID", "PN"), sep = "")

#Inner join the data (only keeps those in HRS_data that appear in Wu_algorithm)
HRS_data <- inner_join(HRS_data, Wu_algorithm, by = "HHIDPN")

#---- L-K-W summary scores and classification ----
#Classify individual as having dementia if summary score <= 6
#(Crimmins et al 2011)
lkw_scores_vars <- paste0("R", waves, "LKWSCORE")
lkw_dem_vars <- paste0("R", waves, "LKWDEM")

HRS_data %<>% cbind(as.data.frame(matrix(nrow = nrow(HRS_data),
                                         ncol = 2*length(lkw_scores_vars))))

colnames(HRS_data)[(ncol(HRS_data) - 19):ncol(HRS_data)] <-
  c(dput(lkw_scores_vars), dput(lkw_dem_vars))

for(i in 1:length(lkw_scores_vars)){
  vars <- c(word_recall_vars[i], serial7_vars[i], backwards_count_vars[i])
  HRS_data[, lkw_scores_vars[i]] = rowSums(HRS_data[, vars])
  HRS_data[, lkw_dem_vars[i]] = (HRS_data[, lkw_scores_vars[i]] <= 6)*1
}

#---- Wu classification ----
wu_dem_vars <- paste0("R", waves, "WUDEM")
wu_demprobs_vars <- colnames(HRS_data)[which(
  str_detect(colnames(HRS_data), "dementpimp", negate = FALSE))]

HRS_data %<>% cbind(as.data.frame(matrix(nrow = nrow(HRS_data),
                                         ncol = length(wu_dem_vars))))

colnames(HRS_data)[(ncol(HRS_data) - 9):ncol(HRS_data)] <- dput(wu_dem_vars)

for(i in 1:length(wu_dem_vars)){
  if(i == 1){
    probs <- pmax(HRS_data[, wu_demprobs_vars[i]],
                  HRS_data[, wu_demprobs_vars[i + 1]], na.rm = TRUE)

  } else{
    probs <- HRS_data[, wu_demprobs_vars[i + 1]]
  }
  HRS_data[, wu_dem_vars[i]] = (probs >= 0.5)*1
}

#---- Sensitivity/Specificity LKW vs. Wu ----
#Can only consider those with directly measured cognitive assessments for now
lkw_vs_wu <- sens_spec(HRS_data[, lkw_dem_vars], HRS_data[, wu_dem_vars])


