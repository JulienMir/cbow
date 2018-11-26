# Initialisation de U et V
# nombre de dimensions
p <- 10

U <- matrix(runif(n*p), nrow = n, ncol = p)
V <- matrix(runif(n*p), nrow = n, ncol = p)

# Pas d'apprentissage
eta <- 0.025

# Boucle
n_iter <- 10
for(iter in 1:n_iter){
  
  # Mélange de D
  order <- sample(1:nrow(D), nrow(D), replace = F)
  D <- D[order, ]
  
  # Boucle sur les couples mot_cible/contexte
  for(row in 1:nrow(D)){
    
    # Calcul du alpha contexte
    ...
    
    # ID du mot cible
    i <- D[row, 1]
    
    # MAJ de Ui
    U[i,] <- U[i,] + eta * grad_u(U, V, D[row,], 2*l)
    
    # Boucle sur les mots contexte
    for(word in 1:(ncol(D)-1)){
      
      # ID du mot cible
      j <- D[row, word + 1]
      
      # MAJ de Vj
      V[j,] <- V[j,] + eta * grad_v(U, V, D[row,], 2*l)
    }
  }
}