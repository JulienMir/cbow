source("gradient.R")
source("softmax.R")

sgd_CBOW <- function(D, vocab, p, n_iter, eta = 0.025) {
  # Initialisation de U et V
  n <- length(vocab)
  U <- matrix(runif(n*p), nrow = n, ncol = p)
  V <- matrix(runif(n*p), nrow = n, ncol = p)
  
  # Boucle
  for(iter in 1:n_iter){
    
    # Mélange de D
    order <- sample(1:nrow(D), nrow(D), replace = F)
    Dp <- D[order, ]
    
    # Boucle sur les couples mot_cible/contexte
    for(row in 1:nrow(D)){
      
      # ID du mot cible
      i <- Dp[row, 1]
      
      # ID des mots contexte
      j <- Dp[row, -1]
      
      # Calcul du alpha contexte
      alpha <- apply(V[j,], 2, sum)/(2*l)
      
      # Calcul des Softmax
      softmax <- fun_softmax(U, alpha)
      
      # MAJ de Ui
      U[i,] <- U[i,] + eta * grad_u(softmax[i], alpha)
      
      # Boucle sur les mots contexte
      for(word in 1:(ncol(D)-1)){
        
        # ID du mot cible
        jl <- Dp[row, word + 1]
        
        # MAJ de Vjl
        s_ui <- colSums(U * softmax)
        V[jl,] <- V[jl,] + eta * grad_v(U, i, s_ui, l)
      }
    }
  }
  
  return(U)
}



