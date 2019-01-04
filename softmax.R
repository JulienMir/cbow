fun_softmax <- function(U, alpha) {
  
  # scalaires Ui * alpha (un pour chaque ligne de U)
  scal <- exp(U %*% alpha)

  return(as.vector(scal/sum(scal)))
}