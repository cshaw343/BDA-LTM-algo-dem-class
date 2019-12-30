#temp code
fact_table_entry <- fact_table[1, ]
source_priors <- LKW_priors

collapsed_gibbs <- function(fact_table_entry, source_priors,
                            #set 10x less for testing
                            burn_in = 10, thinning = 5, runs = 100){

  #---- Initializing values vector ----
  values_table <- vector(length = 8)
  names(values_table) <- c("n00", "n01", "n10", "n11", "p0", "p1",
                           "p0_update", "p1_update")
  values_table[fact_table_entry$init_count] = 1
  values_table["p0"] = source_priors[["beta_0"]]
  values_table["p1"] = source_priors[["beta_1"]]


  #---- Begin sampling ----
  #Initial truth label
  tf <- fact_table_entry$t_f

  #Update based on the claim
  p_tf <-

}
