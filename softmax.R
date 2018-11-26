softmax <- function(U, alpha, i) {
  return(
    exp(U[i,]*alpha)/sum(apply(U, 1, function(x) {
    return(exp(x*alpha))
  })))
}

h_softmax <- function(x) {
  
}