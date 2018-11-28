create_data <- function(path, l) {
  lines <- readLines(path)
  
  corpus <- text2vec::word_tokenizer(lines)[[1]]
  
  # Rajout de mot vide au début et à la fin
  corpus <- c(rep("<>NULL<>", l),
              corpus,
              rep("<>NULL<>", l))
  
  dict <- unique(corpus)
  
  D <- matrix(NA, nrow = length(corpus)-2*l, ncol = 1+2*l)
  colnames(D) <- c("target", paste0("context_", 1:(2*l)))
  
  # Construction du jeu de données D
  for(w in 1:(length(corpus)-2*l)){
    D[w,] <- c(corpus[w+l], corpus[w+l-(1:l)], corpus[w+l+(1:l)])
  }
  
  # Transformation des mots en indices
  Dp <- matrix(rep(0, ncol(D)*nrow(D)), ncol = ncol(D))
  for(ligne in 1:nrow(D)){
    for(col in 1:ncol(D)){
      Dp[ligne, col] <- which(D[ligne, col] == dict)
    }
  }
  Dp <- apply(Dp, 2, as.numeric)

  return(list(D = Dp, vocab = dict))
}
