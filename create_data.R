import_data <- function(path){
  # import
  lines <- readLines(path)
  # Tokenisation
  corpus <- word_tokenizer(lines)[[1]]
  return(corpus)
}

create_data <- function(token, l) {
  
  # Rajout de mot vide au début et à la fin
  corpus <- c(rep("<>NULL<>", l),
              token,
              rep("<>NULL<>", l))
  
  dict <- unique(corpus)
  
  # Transformation du corpus de mots en corpus d'indices
  require(parallel)
  cls <- makeCluster(detectCores())
  clusterExport(cls, list("dict"), envir = environment(create_data))
  
  corpus <- unlist(clusterApply(cls, corpus, function(x){which(x == dict)}))
  
  stopCluster(cls)
  
  D <- matrix(NA, nrow = length(corpus)-2*l, ncol = 1+2*l)
  colnames(D) <- c("target", paste0("context_", 1:(2*l)))
  
  # Construction du jeu de données D
  for(w in 1:(length(corpus)-2*l)){
    D[w,] <- c(corpus[w+l], corpus[w+l-(1:l)], corpus[w+l+(1:l)])
  }

  return(list(D = D, vocab = dict))
}
