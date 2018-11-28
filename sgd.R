source("gradient.R")
source("softmax.R")

sgd_CBOW <- function(D, vocab, p, n_iter) {
  # Initialisation de U et V
  n <- length(vocab)
  U <- matrix(runif(n*p), nrow = n, ncol = p)
  V <- matrix(runif(n*p), nrow = n, ncol = p)
  
  # Pas d'apprentissage
  eta <- 0.025
  
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
      alpha_c <- apply(V[j,], 2, sum)/(2*l)
      
      
      # Calcul des Softmax
      softmax <- fun_softmax(U, alpha_c)
      
      # MAJ de Ui
      U[i,] <- U[i,] + eta * grad_u(U, V, softmax[i], alpha_c)
      
      # Boucle sur les mots contexte
      for(word in 1:(ncol(D)-1)){
        
        # ID du mot cible
        jl <- Dp[row, word + 1]
        
        # MAJ de Vjl
        V[jl,] <- V[jl,] + eta * grad_v(U, V, i, softmax, l)
      }
    }
  }
  
  return(U)
}



