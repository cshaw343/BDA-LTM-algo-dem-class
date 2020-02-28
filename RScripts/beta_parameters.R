beta_parameters <- function(mean, variance){

  alpha <- ((1 - mean)/variance - 1/mean)*mean^2
  beta <- alpha*(1/mean - 1)

  return(c("alpha" = alpha, "beta" = beta))
}
