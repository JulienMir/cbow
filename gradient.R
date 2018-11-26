# Softmax classique
grad_u <- function(U, V, id) {
  sz_c <- length(id)-1
  
  alpha_c <- rowsum(V[id[2:(sz_c+1)],], length(V[1]), reorder = FALSE)/sz_c
  
  softmax <- exp(U[id,]*alpha_c)/sum(apply(U, 1, function(x) {
    return(exp(x*alpha_c))
  }))
  
  return(alpha_c*(1-softmax))
}

grad_v <- function(U, V, id) {
  sz_c <- length(id) - 1
  
  ui <- U[id[1, ]]
  
  alpha_c <- rowsum(V[id[2:(sz_c+1)],], length(V[1]), reorder = FALSE)/sz_c
  
  softmax <- exp(U[id[1,]]*alpha_c)/sum(apply(U, 1, function(x) {
    return(exp(x*alpha_c))
  }))
  
  s_ui <- apply(U, 1, function(x) {
    return(x * softmax)
  }) 
  
  return((ui - s_ui)/sz_c)
}