sens_spec_acc <- function(test, gold){
  #unlist so that we can compare two vectors
  if(length(dim(test)) != 0){
    test <- as.vector(test)
  }

  if(length(dim(gold)) != 0){
    gold <- as.vector(gold)
  }

  matrix <- na.omit(cbind(test, gold))

  match <- rowSums(matrix)
  sens <- sum(match == 2)/sum(matrix[, "gold"])
  spec <- sum(match == 0)/(nrow(matrix) - sum(matrix[, "gold"]))
  acc <- (sum(match == 2) + sum(match == 0))/nrow(matrix)

  return(c("sensitivity" = sens, "specificity" = spec, "accuracy" = acc))
}

