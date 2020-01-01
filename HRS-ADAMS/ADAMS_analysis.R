#---- Package Loading, Options, and Seed Setting ----
if (!require("pacman"))
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("here", "haven", "tidyverse", "magrittr", "future.apply")

options(scipen = 999)
set.seed(20200107)

#---- Source scripts ----
source(here("HRS-ADAMS", "sens_spec.R"))
source(here("HRS-ADAMS", "beta_parameters.R"))
source(here("HRS-ADAMS", "collapsed_gibbs.R"))

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
  # ID, total word recall, serial7, backward counting, IADL summary
#stop at Wave 12 because that's all I have from RAND
waves <- seq(from = 3, to = 12, by = 1)
word_recall_vars <- paste0("R", waves, "TR20")
serial7_vars <- paste0("R", waves, "SER7")
backwards_count_vars <- paste0("R", waves, "BWC20")
IADL_summary_vars <- paste0("R", waves, "IADLA")

vars = c("HHIDPN", word_recall_vars, serial7_vars, backwards_count_vars,
         IADL_summary_vars)

HRS_data <- read_sas(here(
  "Data", "randhrs1992_2016v1_SAS_data", "randhrs1992_2016v1.sas7bdat"),
  n_max = n_max, col_select = vars) %>%
  mutate_at("HHIDPN", as.character)

#---- Remove people missing ALL test scores ----
#Removes 6835 people (42053 --> 35218)
HRS_data$num_missing <- rowSums(is.na(HRS_data[, -1]))
HRS_data$missing_all <- (HRS_data$num_missing == 40)*1
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
#Removes 4809 people (35218 --> 30409)
#Create HHIDPN variable for Wu data
Wu_algorithm$HHID = str_remove(Wu_algorithm$HHID, "^0+")
Wu_algorithm %<>% unite("HHIDPN", c("HHID", "PN"), sep = "")

#Inner join the data (only keeps those in HRS_data that appear in Wu_algorithm)
HRS_data <- inner_join(HRS_data, Wu_algorithm, by = "HHIDPN")

#---- L-K-W summary scores and classification ----
#Classify individual as having dementia if LKW summary score <= 6
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

#---- IADL classification ----
#Classify individual as having dementia if IADL summary score > 0
#(Barberger-Gateau et al 1992)
IADL_dem_vars <- paste0("R", waves, "IADLDEM")

HRS_data %<>% cbind(as.data.frame(matrix(nrow = nrow(HRS_data),
                                         ncol = length(IADL_dem_vars))))

colnames(HRS_data)[(ncol(HRS_data) - 9):ncol(HRS_data)] <- dput(IADL_dem_vars)

for(i in 1:length(IADL_dem_vars)){
  HRS_data[, IADL_dem_vars[i]] = (HRS_data[, IADL_summary_vars[i]] > 0)*1
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

#---- Sensitivity/Specificity IADLs vs. Wu ----
#Can only consider those with directly measured IADLs for now
IADL_vs_wu <- sens_spec(HRS_data[, IADL_dem_vars], HRS_data[, wu_dem_vars])

#---- BDI-LTM algorithm ----
#Creating the fact table
fact_table <- HRS_data %>%
  dplyr::select("HHIDPN", lkw_dem_vars, IADL_dem_vars) %>%
  pivot_longer(c(lkw_dem_vars, IADL_dem_vars), names_to = "key",
               values_to = "Dementia") %>%
  na.omit() %>%
  mutate("Source" = str_extract(key, "(?<=\\d).*$")) %>%
  mutate_at("Source", ~str_sub(., end = -4)) %>%
  mutate_at("key", ~str_sub(., end = 2)) %>%
  unite(col = "Entity", c("HHIDPN", "key"), sep = "_") %>%
  rownames_to_column("FID")

fact_table %<>%
  #Randomly assigning truth labels
  mutate("t_f" = rbinom(n = nrow(fact_table), size = 1, prob = 0.5)) %>%
  #Figure out which counts to initialize... order for numbers: t_f, o_c
  mutate("init_count" =
           case_when((t_f == Dementia) & t_f == 0 ~ "n00",
                     (t_f == Dementia) & t_f == 1 ~ "n11",
                     (t_f != Dementia) & t_f == 1 ~ "n10",
                     TRUE ~ "n01")) %>%
  unite(source_names, c(Source, FID), sep = "_", remove = FALSE) %>%
  #Column of truth label = 1 probabilities
  mutate("p_tf1" = 0)

#Specify priors on sources
#Order: priors on sensitivity, priors on 1 - specificity, priors on truth label
# prior on sensitivity = c(\alpha_{11}, \alpha_{10})
# prior on 1 - specificity = c(\alpha_{01}, \alpha_{00})
# prior on truth label = c(\beta_0, \beta_1)

#Values based on LKW performance in Melinda and Kan's paper
LKW_sensitivity_pars <- beta_parameters(mean = 0.41, variance = 0.02)
LKW_specificity_pars <- beta_parameters(mean = 0.11, variance = 0.01)
LKW_truth_label_pars <- beta_parameters(mean = 0.5, variance = 0.05)

#Values based on 4-item IADL from (Barberger-Gateau et al 1992)
IADL_sensitivity_pars <- beta_parameters(mean = 0.94, variance = 0.01)
IADL_specificity_pars <- beta_parameters(mean = 0.29, variance = 0.02)
IADL_truth_label_pars <- beta_parameters(mean = 0.5, variance = 0.05)

#Making source prior table
source_priors <- as.data.frame(matrix(nrow = 2, ncol = 7)) %>%
  set_colnames(c("Source", "alpha11", "alpha10", "alpha01", "alpha00", "beta0",
                 "beta1")) %>%
  mutate("Source" = c("LKW", "IADL"))

source_priors[1, colnames(source_priors)[-1]] <-
  c(LKW_sensitivity_pars, LKW_specificity_pars, LKW_truth_label_pars)
source_priors[2, colnames(source_priors)[-1]] <-
  c(IADL_sensitivity_pars, IADL_specificity_pars, IADL_truth_label_pars)





test <- apply(fact_table[1:2, ], 1, collapsed_gibbs, source_priors)





#Creating the gold standard table
gold_table <- HRS_data %>%
  dplyr::select("HHIDPN", wu_dem_vars) %>%
  pivot_longer(wu_dem_vars, names_to = "key", values_to = "gold") %>%
  na.omit() %>%
  mutate_at("key", ~str_replace(., "WUDEM", "")) %>%
  unite(col = "Entity", c("HHIDPN", "key"), sep = "_")

#Merging gold standard data with fact_table
fact_table <- inner_join(fact_table, gold_table, by = "Entity")



#Sample p_tf1 for everyone

#---- Do analysis on subset ----
fact_table_samp100 <- sample_n(fact_table, size = 100, replace = FALSE)

plan(multiprocess, workers = 0.5*availableCores()) #Start cluster
start <- Sys.time()
fact_table_samp100$p_tf1 <- apply(fact_table_samp100, 1,
                                  collapsed_gibbs, LKW_priors)
finish <- Sys.time() - start
plan(sequential)                                   #Shut down cluster

#Update t_f based on p(t_f = 1)
fact_table_samp100 %<>%
  mutate("t_f" = case_when(p_tf1 >= 0.5 ~ 1,
                           TRUE ~ 0))

#Sensitivity and specificity
results <- sens_spec(fact_table_samp100$t_f, fact_table_samp100$gold)
