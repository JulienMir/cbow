fun_softmax <- function(U, alpha) {
  
  res <- sapply(1:nrow(U), function(i) {exp(t(U[i,]) %*% alpha) /sum(apply(U, 1, function(x) {
    return(exp(x %*% alpha))
  }))})
  
  return(res)
}

h_softmax <- function(x) {
  
}
