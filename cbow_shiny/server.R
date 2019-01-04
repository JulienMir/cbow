library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(RcppCNPy)
library(doParallel)

shinyServer(server <- function(input, output, session) {
  
  # Load learned representations
  # Found at https://vsmlib.readthedocs.io/en/latest/tutorial/getting_vectors.html#pre-trained-vsms
  models <- vector("list", 2)
  modelnames <- c("CBOW unbound", "SG unbound")
  paths <- c("./data/word_deps_cbow_25d/", "./data/word_deps_sg_25d/")
  
  for(i in 1:2) {
    # Lecture du fichier NPY convertit
    con <- file(paste0(paths[i],"words25.bin"), "rb")
    
    dim <- readBin(con, "integer", 2)
    words <- matrix(readBin(con, "numeric", prod(dim)), nrow=dim[1], ncol=dim[2])
    
    close(con)
    
    vocab <- readLines(paste0(paths[i],"words25.vocab"))
    
    # On filtre les caractères alphanumériques
    vocab <- gsub("[^a-z]+", "", vocab)
    words <- words[which(vocab != ""), ]
    vocab <- vocab[which(vocab != "")]
    
    words <- words[!duplicated(vocab), ]
    vocab <- vocab[!duplicated(vocab)]
    
    models[[i]] <- list(vectors=words, vocab=vocab)
  }
  
  ######################## GENERAL FUNCTIONS ########################
  cosine_similarity <- function(a, b) {
    return(sum(a*b)/(sqrt(sum(a*a)) * sqrt( sum( b*b ) ) ))
  }
  
  closest_words <- function(a, model, n=5) {
    print("Find similarities")
    
    if(length(a) == 0) {
      return(rep("ERROR", n))
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
  ### Autocomplétion
  # Predicts the next possibles words when the text input changes
  get_next_words <- reactive({
    sentence <- tolower(input$sentence)
    
    df <- NULL
    
    if (nchar(sentence) == 0){
      # Empty df
      df <- data.frame(cbow = character(0L),
                       sg = numeric(0L))
    }
    else{
      sentence <- unlist(strsplit(sentence, "[ ]+"))
      
      print(sentence)
      
      for(model in models) {
        res <- rep(0, ncol(model$vectors))
        
        for(token in sentence) {
          print(paste(token, " and ", model$vocab[which(model$vocab ==  token)]))
          res <- res + model$vectors[which(model$vocab ==  token), ]
        }
        
        df <- cbind(df, closest_words(res, model, 3))
      }
    }
    print(df)
    colnames(df) <- modelnames
    print("name model")
    
    print(df)
    
    return(df)
  })
  
  ### Analogies
  # Find the result of arthimetic operation on representation vectors
  find_analogy <- reactive({
    analogy1 <- tolower(input$analogy1)
    analogy2 <- tolower(input$analogy2)
    analogy3 <- tolower(input$analogy3)
    
    df <- NULL
    
    if (nchar(analogy1) == 0 || nchar(analogy2) == 0 || nchar(analogy3) == 0){
      # Empty df
      df <- data.frame(cbow = character(0L),
                       sg = numeric(0L))
    }else{
      for(model in models) {
        res <- rep(0, ncol(model$vectors))
        
        res <- res + model$vectors[which(model$vocab ==  analogy1), ]
        res <- res - model$vectors[which(model$vocab ==  analogy2), ]
        res <- res + model$vectors[which(model$vocab ==  analogy3), ]
        
        df <- cbind(df, closest_words(res, model, 3))
      }
    }
    colnames(df) <- modelnames
    
    return(df)
  })
  
  ######################## CONTROLLER ########################
  
  output$next_word_table <- renderTable({
    get_next_words() 
  })
  
  output$analogies_word_table <- renderTable({
    find_analogy()
  })
})
