import_data <- function(path){
  # import
  lines <- readLines(path, warn=FALSE)
  # Tokenisation
  corpus <- word_tokenizer(lines)[[1]]
  return(corpus)
}

corpus_indexation <- function(tokens, dict){
  print("Debut indexation")
  
  if(parallelize) {
    # Transformation du corpus de mots en corpus d'indices
    require(parallel)
    
    cls <- makeCluster(detectCores())
    clusterExport(cls, c("dict"), envir = environment())
    
    corpus <- unlist(clusterApply(cls, corpus, function(x){which(x == dict)}))
    
    stopCluster(cls)
  }
  else {
    new_corpus <- vector("integer", length(corpus))
    
    for(i in 1:length(dict)) {
      new_corpus[which(corpus == dict[i])] <- dict[i]
    }
  }
  
  print("Fin indexation")
  
  return(corpus)
}

create_data <- function(tokens, l) {
  
  # Rajout de mot vide au début et à la fin
  tokens <- c(rep("<>NULL<>", l),
              tokens,
              rep("<>NULL<>", l))
  
  dict <- unique(tokens)
  
  corpus <- corpus_indexation(tokens, dict)
  
  D <- matrix(NA, nrow = length(corpus)-2*l, ncol = 1+2*l)
  colnames(D) <- c("target", paste0("context_", 1:(2*l)))
  
  # Construction du jeu de données D
  print("Début contruction de D...")
  for(w in 1:(length(corpus)-2*l)){
    D[w,] <- c(corpus[w+l], corpus[w+l-(1:l)], corpus[w+l+(1:l)])
  }
  print("Fin contruction de D")

  return(list(D = D, vocab = dict))
}
