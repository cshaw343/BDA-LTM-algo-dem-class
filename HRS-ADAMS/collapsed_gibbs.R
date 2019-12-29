#temp code
fact_table_entry <- fact_table[1, ]
source_priors <-

collapsed_gibbs <- function(fact_table_entry, source_priors,
                            #set 10x less for testing
                            burn_in = 10, thinning = 5, runs = 100){

  #---- Initializing count vector ----
  counts <- vector(length = 4)
  names(counts) <- c("n00", "n01", "n10", "n11")
  counts[fact_table_entry$init_count] = 1

  #---- Begin sampling ----
  #Uninformative priors for the truth label
  p_tf <- 0.5
  p_1minustf <- 0.5

  #Update based on the claim
  p_tf <-

}
