fun_softmax <- function(U, alpha) {
  
  # scalaires Ui * alpha (un pour chaque ligne de U)
  scal <- exp(U %*% alpha)
  total <- sum(scal)
  
  res <- as.vector(scal/total)
  return(res)
}

h_softmax <- function(x) {
  
}
