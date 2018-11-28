# Softmax classique
grad_u <- function(softmax, alpha) {
  return(alpha * (1-softmax))
}

grad_v <- function(U, i, s_ui, l) {
  ui <- U[i,]
  return((ui - s_ui)/(2*l))
}
