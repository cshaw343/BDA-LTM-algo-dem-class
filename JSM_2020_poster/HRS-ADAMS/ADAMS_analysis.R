#---- Package Loading, Options, and Seed Setting ----
if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("here", "tidyverse", "magrittr", "future.apply", "readr", "haven",
       "lubridate")
}
options(scipen = 999)
set.seed(20200107)

#---- Source scripts ----
source(here::here("RScripts", "sens_spec.R"))
source(here::here("RScripts", "beta_parameters.R"))
source(here::here("RScripts", "collapsed_gibbs.R"))
source(here::here("RScripts", "get_ADAMS_demdx.R"))
source(here::here("RScripts", "get_ADAMS_tracker.R"))

#---- Pull ADAMS interview year from tracker file ----
ADAMS_year <- adams_tracker %>%
  dplyr::select(c("HHIDPN", contains("YEAR")))
ADAMS_year[ADAMS_year == 9997] <- NA #9997 is an incomplete interview

#---- Read in the ADAMS data ----
ADAMS_waves <- c("A", "B", "C", "D")

for(waves in ADAMS_waves){
  assign(paste0("ADAMS_", waves), get_ADAMS_demdx(waves))
}

#---- Read in the HRS data ----
#list of variables to read in from HRS data:
  # ID, total word recall, serial7, backward counting, IADL summary,
  # interview end date
#stop at Wave 12 because that's all I have from RAND
waves <- seq(from = 3, to = 12, by = 1)
word_recall_vars <- paste0("R", waves, "TR20")
serial7_vars <- paste0("R", waves, "SER7")
backwards_count_vars <- paste0("R", waves, "BWC20")
IADL_summary_vars <- paste0("R", waves, "IADLA")
interview_end_date_vars <- paste0("R", waves, "IWEND")

vars = c("HHIDPN", word_recall_vars, serial7_vars, backwards_count_vars,
         IADL_summary_vars, interview_end_date_vars)

HRS_data <- read_sas(paste0("/Users/CrystalShaw/Box/NIA_F31_April2020/Data/",
                            "HRS/HRS RAND/randhrs1992_2016v1_SAS_data/",
                            "randhrs1992_2016v1.sas7bdat"),
                     n_max = Inf, col_select = vars) %>%
  mutate_at("HHIDPN", as.character)

#Format interview end date so we can get the year
HRS_data %<>% mutate_at(interview_end_date_vars,
                        ~year(as.Date(., origin = "1960-01-01")))

#---- Remove those who don't appear in ADAMS ----
#Removes 40543 people (42053 --> 1510)
#Inner join the data (only keeps those in HRS_data that appear in ADAMS)
HRS_data <- inner_join(HRS_data, ADAMS_year, by = "HHIDPN")

#---- Remove people missing ALL test scores ----
#Removes 0 people (1510 --> 1510)
HRS_data$num_missing <-
  rowSums(is.na(
    HRS_data[, c(word_recall_vars, serial7_vars, backwards_count_vars,
                 IADL_summary_vars)]))
HRS_data$missing_all <- (HRS_data$num_missing == 40)*1
HRS_data %<>% filter(missing_all == 0)

#---- Join with ADAMS dem data ----
HRS_data %<>%
  left_join(ADAMS_A, by = "HHIDPN") %>%
  left_join(ADAMS_B, by = "HHIDPN") %>%
  left_join(ADAMS_C, by = "HHIDPN") %>%
  left_join(ADAMS_D, by = "HHIDPN")

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

#---- Add columns for LKW and IADL class by ADAMS wave ----
lkw_ADAMS_varnames <- paste0("lkw_", ADAMS_waves)
IADL_ADAMS_varnames <- paste0("IADL_", ADAMS_waves)
HRS_data %<>%
  cbind(as.data.frame(matrix(nrow = nrow(HRS_data), ncol = 8)) %>%
          set_colnames(c(lkw_ADAMS_varnames, IADL_ADAMS_varnames))) %>%
  mutate_at("HHIDPN", as.numeric)

#---- Grab the appropriate HRS years ----
#Do computations along columns for efficiency
HRS_data <- t(HRS_data)

for(i in 1:ncol(HRS_data)){
  ADAMS_dates <- HRS_data[paste0(ADAMS_waves, "YEAR"), i]
  HRS_dates <- HRS_data[interview_end_date_vars, i]
  for(j in 1:length(ADAMS_dates)){
    if(is.na(ADAMS_dates[j])){next}
    else{
      HRS_wave <- max(which(ADAMS_dates[j] - HRS_dates > 0)) + 2
      HRS_data[paste0("lkw_", ADAMS_waves[j]), i] <-
        HRS_data[paste0("R", HRS_wave, "LKWDEM"), i]
      HRS_data[paste0("IADL_", ADAMS_waves[j]), i] <-
        HRS_data[paste0("R", HRS_wave, "IADLDEM"), i]
    }
  }
}

HRS_data <- t(HRS_data)

#---- Sensitivity/Specificity LKW vs. ADAMS ----
#Can only consider those with directly measured cognitive assessments for now
lkw_vs_ADAMS <- sens_spec(HRS_data[, lkw_ADAMS_varnames],
                          HRS_data[, paste0("dem_", ADAMS_waves)])

#---- Sensitivity/Specificity IADLs vs. ADAMS ----
#Can only consider those with directly measured IADLs for now
IADL_vs_ADAMS <- sens_spec(HRS_data[, IADL_ADAMS_varnames],
                           HRS_data[, paste0("dem_", ADAMS_waves)])

#---- BDI-LTM algorithm ----
#Creating the fact table
fact_table <- HRS_data %>%
  dplyr::select("HHIDPN", lkw_dem_vars, IADL_dem_vars) %>%
  pivot_longer(c(lkw_dem_vars, IADL_dem_vars), names_to = "key",
               values_to = "Dementia") %>%
  na.omit() %>%
  mutate("Source" = case_when(str_detect(key, "LKW") ~ "LKW",
                              TRUE ~ "IADL")) %>%
  mutate_at("key", ~sub("(\\d)[^0-9]+$", "\\1", .)) %>%
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

#Creating the gold standard table
gold_table <- HRS_data %>%
  dplyr::select("HHIDPN", wu_dem_vars) %>%
  pivot_longer(wu_dem_vars, names_to = "key", values_to = "gold") %>%
  na.omit() %>%
  mutate_at("key", ~str_replace(., "WUDEM", "")) %>%
  unite(col = "Entity", c("HHIDPN", "key"), sep = "_")

#---- Do analysis on subset ----
people_in_sample <- sample(fact_table$Entity, size = 25000, replace = FALSE)
fact_table_samp <- fact_table[which(fact_table$Entity %in% people_in_sample), ]

plan(multiprocess, workers = 0.5*availableCores()) #Start cluster
start <- Sys.time()
fact_table_samp$p_tf1 <- apply(fact_table_samp, 1,
                               collapsed_gibbs, source_priors,
                               thinning = 100, runs = 10000)
finish <- Sys.time() - start
plan(sequential)                                   #Shut down cluster

#Create a unique fact table
fact_table_unique <-
  as.data.frame(matrix(ncol = 2,
                       nrow = length(unique(fact_table_samp$Entity)))) %>%
  set_colnames(c("Entity", "t_f")) %>%
  mutate("Entity" = unique(fact_table_samp$Entity))

#Update t_f based on expected p(t_f = 1)
for(i in 1:nrow(fact_table_unique)){
  rows <- which(fact_table_samp[, "Entity"] == fact_table_unique[i, "Entity"])
  mean_t_f <- colMeans((fact_table_samp[rows, "p_tf1"]))

  fact_table_unique[i, "t_f"] = (mean_t_f >= 0.5)*1
}

#Merging gold standard data with fact_table
fact_table_unique <- inner_join(fact_table_unique, gold_table, by = "Entity")

#Sensitivity and specificity
results <- sens_spec(fact_table_unique$t_f, fact_table_unique$gold)
