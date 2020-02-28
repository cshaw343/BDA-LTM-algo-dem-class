sens_spec <- function(test, gold){
  #unlist so that we can compare two vectors
  if(length(dim(test)) != 0){
    test <- unlist(test)
  }

  if(length(dim(gold)) != 0){
    gold <- unlist(gold)
  }

  matrix <- na.omit(cbind(test, gold))

  match <- rowSums(matrix)
  sens <- sum(match == 2)/sum(matrix[, "gold"])
  spec <- sum(match == 0)/(nrow(matrix) - sum(matrix[, "gold"]))

  return(c("sensitivity" = sens, "specificity" = spec))
}

