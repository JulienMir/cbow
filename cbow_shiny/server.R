library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(doParallel)
library(ggplot2)

shinyServer(server <- function(input, output, session) {
  
<<<<<<< HEAD
  # ######################## INITIALIZATION ########################
  # ### Filter words
  # # Import words counts
  # words_counts <- read.csv2("./data/words_counts.csv") %>%
  #   mutate(word = as.character(word))
  # 
  # # We keep only the Nwords most frequent words
  # Nwords <- 10000
  # words_kept <- words_counts$word[1:Nwords]
  # rm(words_counts)
  # 
  # ### Load learned representations
  # # Found at https://vsmlib.readthedocs.io/en/latest/tutorial/getting_vectors.html#pre-trained-vsms
  # models <- vector("list", 2)
  # modelnames <- c("CBOW Unbound", "SkipGram Unbound")
  # paths <- c("./data/word_deps_cbow_25d/", "./data/word_deps_sg_25d/")
  # 
  # for(i in 1:2) {
  #   # Lecture du fichier .NPY converti
  #   con <- file(paste0(paths[i],"words25.bin"), "rb")
  #   
  #   dim <- readBin(con, "integer", 2)
  #   words <- matrix(readBin(con, "numeric", prod(dim)), nrow=dim[1], ncol=dim[2])
  #   
  #   close(con)
  #   
  #   vocab <- readLines(paste0(paths[i],"words25.vocab"))
  #   
  #   # On filtre les caractères alphanumériques
  #   vocab <- gsub("[^a-z]+", "", vocab)
  #   words <- words[which(vocab != ""), ]
  #   vocab <- vocab[which(vocab != "")]
  #   
  #   words <- words[!duplicated(vocab), ]
  #   vocab <- vocab[!duplicated(vocab)]
  #   
  #   # We keep only the words in words_kept
  #   index <- which(vocab %in% words_kept)
  #   vocab <- vocab[index]
  #   words <- words[index,]
  #   
  #   
  #   models[[i]] <- list(vectors=words, vocab=vocab)
  # }
  
=======
>>>>>>> master
  ######################## INITIALIZATION ########################
  ### Load learned representations
  # Found at https://vsmlib.readthedocs.io/en/latest/tutorial/getting_vectors.html#pre-trained-vsms
  modelnames <- c("CBOW 25d", "SkipGram 25d", "CBOW 500d", "SkipGram 500d")
  paths <- c("./data/word_deps_cbow_25d/",
             "./data/word_deps_sg_25d/",
             "./data/word_deps_cbow_500d/",
             "./data/word_deps_sg_500d/")
  models <- vector("list", length(paths))
  
  for(i in 1:length(paths)) {
<<<<<<< HEAD
    
    words <- as.matrix(read.csv2(paste0(paths[i], "words.csv")))
    vocab <- as.character(read.csv2(paste0(paths[i], "vocab.csv"))[,1])
    
    models[[i]] <- list(vectors=words, vocab=vocab)
=======
    models[[i]] <- list(vectors = as.matrix(read.csv2(paste0(paths[i], "words.csv"))), 
                        vocab = as.character(read.csv2(paste0(paths[i], "vocab.csv"))[,1]))
>>>>>>> master
  }
  
  
  ######################## GENERAL FUNCTIONS ########################
  cosine_similarity <- function(a, b) {
    return(sum(a*b)/(sqrt(sum(a*a)) * sqrt( sum( b*b ) ) ))
  }
  
<<<<<<< HEAD
  closest_words <- function(a, model, n=5) {
    print("Find similarities")
    
    if(length(a) == 0) {
      return(rep("ERROR", n))
=======
  closest_words <- function(a, model, n=5, parallel=FALSE) {
    if(length(a) == 0) {
      return(rep("ERROR", n))
    }
    
    similarity <- numeric(length(model$vocab))
    
    if(parallel) {
      registerDoParallel(3)
      
      similarity <- foreach(i = 1:length(model$vocab), .combine = "c", .export = "cosine_similarity") %dopar%
        cosine_similarity(a, model$vectors[which(model$vocab == model$vocab[i]), ])
      
      stopImplicitCluster()
    }
    else {
      for(i in 1:length(model$vocab)) {
        v <- model$vectors[which(model$vocab == model$vocab[i]), ]
        
        similarity[i] <- cosine_similarity(a, v)
      }
>>>>>>> master
    }
    
    similarity <- numeric(length(model$vocab))
    
    registerDoParallel(3)
    
    similarity <- foreach(i = 1:length(model$vocab), .combine = "c", .export = "cosine_similarity") %dopar%
      cosine_similarity(a, model$vectors[which(model$vocab == model$vocab[i]), ])
    
    stopImplicitCluster()
    
    # for(i in 1:length(model$vocab)) {
    #   v <- model$vectors[which(model$vocab == model$vocab[i]), ]
    # 
    #   similarity[i] <- cosine_similarity(a, v)
    # 
    #   cat(".")
    # }
    
    ordered <- model$vocab[order(-similarity)]
    return(ordered[1:n])
  }
  
  # Stops the app when it is closed in navigator
  session$onSessionEnded(stopApp)
  
  ######################## MODEL ########################
<<<<<<< HEAD
  ### Autocomplétion
  # Predicts the next possibles words when the text input changes
  get_next_words <- eventReactive(input$auto_button, {
    sentence <- tolower(input$sentence)
    
    selected_models <- as.integer(input$auto_model_choice)
    
    df <- NULL
    
    if (nchar(sentence) == 0){
      # Empty df
      df <- data.frame(cbow = character(0L),
                       sg = numeric(0L))
    }
    else{
      sentence <- unlist(strsplit(sentence, "[ ]+"))
      
      print(sentence)
      
      for(mod in selected_models) {
        model <- models[[mod]]
        res <- rep(0, ncol(model$vectors))
        
        for(token in sentence) {
          print(paste(token, " and ", model$vocab[which(model$vocab ==  token)]))
          res <- res + model$vectors[which(model$vocab ==  token), ]
        }
        
        df <- cbind(df, closest_words(res, model, 3))
      }
    }
    print(df)
    colnames(df) <- modelnames[selected_models]
    print("name model")
    
    print(df)
    
    return(df)
  })
  
=======
>>>>>>> master
  ### Analogies
  # Find the result of arthimetic operation on representation vectors
  find_analogy <- eventReactive(input$ana_button, {
    
    selected_models <- as.integer(input$ana_model_choice)
    
    analogy1 <- tolower(input$analogy1)
    analogy2 <- tolower(input$analogy2)
    analogy3 <- tolower(input$analogy3)
    
    df <- NULL
    
<<<<<<< HEAD
    if (nchar(analogy1) == 0 || nchar(analogy2) == 0 || nchar(analogy3) == 0){
      # Empty df
      df <- data.frame(cbow = character(0L),
                       sg = numeric(0L))
    }else{
      for(mod in selected_models) {
        model <- models[[mod]]
        res <- rep(0, ncol(model$vectors))
        
        res <- res + model$vectors[which(model$vocab ==  analogy1), ]
        res <- res - model$vectors[which(model$vocab ==  analogy2), ]
        res <- res + model$vectors[which(model$vocab ==  analogy3), ]
        
        df <- cbind(df, closest_words(res, model, 3))
      }
    }
=======
    for(mod in selected_models) {
      model <- models[[mod]]
      res <- rep(0, ncol(model$vectors))
      
      res <- res + model$vectors[which(model$vocab ==  analogy1), ]
      res <- res - model$vectors[which(model$vocab ==  analogy2), ]
      res <- res + model$vectors[which(model$vocab ==  analogy3), ]
      
      df <- cbind(df, closest_words(res, model, 3))
    }
    
>>>>>>> master
    colnames(df) <- modelnames[selected_models]
    
    return(df)
  })
  
  ### Visualization
  # Makes a df containing all the data needed for the graph, updates when button visu_button is clicked
  get_coords <- eventReactive(input$visu_button, {

    # word tokenisation (same order as input)
    words <- strsplit(gsub("[^a-z]+", " ", tolower(input$visu_text)), " ")[[1]]

    # Index of the chosen models
    selected_models <- as.integer(input$visu_model_choice)
    
    coords <- data.frame(PC1 = numeric(0), PC2 = numeric(0), model = character(0), word = character(0))
    
    for(mod in selected_models){
      # Find words indexes (same order as in vocab)
      index <- which(models[[mod]]$vocab %in% words)
      
      # Reorder words (sorting by alphabetical order should be enough but that way there is no doubt)
      words <- models[[mod]]$vocab[index]
      
      # Getting the representations of the selected words
      repr <- models[[mod]]$vectors[index,]
      
      # Performs a PCA and get the vectors coordonates for the first 2 axis
      # df <- as.data.frame(prcomp(repr)$x[, 1:2]) %>%
      #   mutate(model = modelnames[mod],
      #          word = words)
      
      df <- as.data.frame(prcomp(models[[mod]]$vectors)$x[, 1:2]) %>%
        filter(row_number() %in% index) %>%
        mutate(model = modelnames[mod],
               word = words)
      
      # df <- data.frame(PC1 = repr[,2], PC2 = repr[,2]) %>%
      #   mutate(model = modelnames[mod],
      #          word = words)
      
      coords <- rbind(coords, df)
    }
  
    return(coords)
  })
  
  ### key word
  # find the nearest words from the sum of the input : "france" + "capital" = "Paris" (hopefully)
  get_key_word <- eventReactive(input$key_button, {
    
    # word tokenisation (same order as input)
<<<<<<< HEAD
    words <- strsplit(tolower(input$key_text), " ")[[1]]
=======
    words <- unlist(strsplit(tolower(input$key_text), " "))
>>>>>>> master
    
    # Index of the chosen models
    selected_models <- as.integer(input$key_model_choice)
    
    i <- 1
    for(mod in selected_models){
      # Find words indexes
      index <- which(models[[mod]]$vocab %in% words)

      # Getting the representations of the selected words
      repr <- models[[mod]]$vectors[index,]
      
      # Sum of all representations
      sum <- apply(repr, 2, sum)
      
      # nearest words from sum
      nearest <- closest_words(sum, models[[mod]], 3)
      
      if(i == 1){
        df <- data.frame(c1 = nearest)
      }else{
        df[,i] <- nearest
      }
      i <- i+1
    }
<<<<<<< HEAD
    names(df) <- modelnames[selected_models]
=======
    
    names(df) <- modelnames[selected_models]
    
>>>>>>> master
    return(df)
  })
  
  ### likelyhood
  # returns the mean of cosine similarity of every couple of input words
  # So it is a score, the higher it is, the similar are the words
  get_likelyhood <- function(words, selected_models){
    
    df <- data.frame(score = numeric(0), model = character(0))
    
    for(mod in selected_models){
      # Find words indexes
      index <- which(models[[mod]]$vocab %in% words)
      
      # Getting the representations of the selected words
      repr <- models[[mod]]$vectors[index,]
      
      # cosinus similarities of every couple of words
      similarities <- c()
      for(i in 1:(nrow(repr)-1)){
        for(j in (i+1):nrow(repr)){
          similarities <- c(similarities, cosine_similarity(repr[i,], repr[j,]))
        }
      }
      
      temp <- data.frame(score = mean(similarities), model = modelnames[mod])
      print(temp)
      df <- rbind(df, temp)
    }
    return(df)
  }
  likelyhood1 <- eventReactive(input$like_button, {
    
    # word tokenisation (same order as input)
    words <- strsplit(tolower(input$like_text1), " ")[[1]]
    
    # Index of the chosen models
    selected_models <- as.integer(input$like_model_choice)
    
    df <- get_likelyhood(words, selected_models)
    return(df)
  })
  likelyhood2 <- eventReactive(input$like_button, {
    
    # word tokenisation (same order as input)
    words <- strsplit(tolower(input$like_text2), " ")[[1]]
    
    # Index of the chosen models
    selected_models <- as.integer(input$like_model_choice)
    
    df <- get_likelyhood(words, selected_models)
    return(df)
  })
  
  ######################## CONTROLLER ########################
  ### TABLES
<<<<<<< HEAD
  output$next_word_table <- renderTable({
    get_next_words() 
  })
  
  output$analogies_word_table <- renderTable({
    find_analogy()
  })
  
  output$key_word_table <- renderTable({
    get_key_word()
  })
  
=======
  output$analogies_word_table <- renderTable({
    find_analogy()
  })
  
  output$key_word_table <- renderTable({
    get_key_word()
  })
  
>>>>>>> master
  output$like_table1 <- renderTable({
    likelyhood1() %>%
      rename("Score" = score,
             "Modèle" = model)
  })
  output$like_table2 <- renderTable({
    likelyhood2() %>%
      rename("Score" = score,
             "Modèle" = model)
  })
  
  ### GRAPHS
  # Updated whenever get_coords() is updated
  output$visu_graph <- renderPlot({
    
    coords <- get_coords()
    
    ### Graph settings
    # decalage graphique
    dec <- 0.1
    
    # limits
    xmargin <- (max(coords$PC1) - min(coords$PC1))*0.08
    ymargin <- (max(coords$PC2) - min(coords$PC2))*0.02
    xlim <- c(min(0, min(coords$PC1)-xmargin), max(0, max(coords$PC1)+xmargin))
    ylim <- c(min(0, min(coords$PC2)-ymargin), max(0, max(coords$PC2)+ymargin))
    nrow <- ceiling(length(unique(coords$model))/2)
    
    if(!input$type_graph){
      # bidouillage de la mort pour essayer de rendre les graph multiples plus lisibles
      # Ca ne change rien à l'interprétation des graph
      # Il est déconseillé d'essayer de le comprendre
      mods <- unique(coords$model)
      if (length(mods) > 1){
        ex <- sign(max(coords$PC1[coords$model == mods[1]]))
        for(i in 2:length(mods)){
          if(sign(max(coords$PC1[coords$model == mods[i]])) != ex){
            coords$PC1[coords$model == mods[i]] <- coords$PC1[coords$model == mods[i]] * (-1)
          }
        }
      }
      
      # Graph
      ggplot(coords) +
        geom_point(aes(x = 0, y = 0), shape = 3) + 
        geom_text(aes(x = PC1, y = PC2, label = word,
                      hjust = ifelse(PC1 >= 0, -1*dec, 1+dec), vjust = ifelse(PC2 >= 0, -1*dec, 1+dec))) +
        geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow()) +
        facet_wrap( ~ model, nrow = nrow) + theme(legend.position = "none") +
        # scale_color_manual(values = c("CBOW Unbound" = "#FF6437", "SkipGram Unbound" = "#183FCC")) + 
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        labs(title = "Représentation des vecteurs",
             x = "1er axe de l'ACP",
             y = "2ème axe de l'ACP",
             color = "Modèle") +
        theme(plot.title = element_text(hjust = 0.5))
    }else{
      ggplot(coords) +
        geom_point(aes(x = 0, y = 0), shape = 3) +
        geom_text(aes(x = PC1, y = PC2, label = word, color = model,
                      hjust = ifelse(PC1 >= 0, -1*dec, 1+dec), vjust = ifelse(PC2 >= 0, -1*dec, 1+dec))) +
        geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2, color = model), arrow = arrow()) +
        # scale_color_manual(values = c("CBOW Unbound" = "#FF6437", "SkipGram Unbound" = "#183FCC")) +
        scale_x_continuous(breaks = NULL, limits = xlim) +
        scale_y_continuous(breaks = NULL, limits = ylim) +
        labs(title = "Représentation des vecteurs",
             x = "1er axe de l'ACP",
             y = "2ème axe de l'ACP",
             color = "Modèle") +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
<<<<<<< HEAD
})
=======
})
>>>>>>> master
