# Softmax classique
grad_u <- function(U, V, softmax, alpha_c) {
  return(alpha_c*(1-softmax))
}

grad_v <- function(U, V, i, softmax, l) {
  ui <- U[i,]

  s_ui <- rep(0, ncol(U))
  for(k in 1:nrow(U)){
    s_ui <- s_ui + (U[k,] * softmax[k])
  }
  # s_ui <- apply(U, 1, function(x) {
  #   return(x * softmax)
  # })
  
  
  
  return((ui - s_ui)/(2*l))
}
