library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(RcppCNPy)

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
    words <- matrix(readBin(con, "numeric", prod(dim)), dim[1], dim[2])
    
    close(con)
    
    vocab <- readLines(paste0(paths[i],"words25.vocab"))
    
    models[[i]] <- list(vectors=words, vocab=vocab)
  }
  
  ######################## GENERAL FUNCTIONS ########################
  cosine_similarity <- function(a, b) {
    return(sum(a*b)/(sqrt(sum(a*a)) * sqrt( sum( b*b ) ) ))
  }
  
  closest_words <- function(a, model, n=5) {
    similarity <- numeric(length(model$vocab))
    for(i in 1:length(model$vocab)) {
      v1 <- a
      v2 <- model$vectors[which(model$vocab == model$vocab[i])]
      
      similarity[i] <- cosine_similarity(v1, v2)
    }
    
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
    
    if (nchar(sentence) == 0){
      # Empty df
      df <- data.frame(word = character(0L),
                       prob = numeric(0L))
    }
    else{
      sentence <- unlist(strsplit(sentence, "[ ]+"))
      
      print(sentence)
      
      for(model in models) {
        res <- numeric(0)
        
        for(token in sentence) {
          print(paste(token, " and ", vocab[which(model$vocab ==  token)]))
          res <- res + model$vectors[which(model$vocab ==  token)]
        }
        
        df <- cbind(df, data.frame(closest_words(res, model, 3)))
      }
    }
    
    colnames(df) <- modelnames
    
    print(df)
    
    return(df)
  })
  
  ### Analogies
  # Find the result of arthimetic operation on representation vectors
  find_analogy <- reactive({
    analogy1 <- input$analogy1
    analogy2 <- input$analogy2
    analogy3 <- input$analogy3
    
    if (nchar(analogy1) == 0 || nchar(analogy2) == 0 || nchar(analogy3) == 0){
      # Empty df
      df <- data.frame(word = character(0L),
                       prob = numeric(0L))
    }else{
      words <- c("red", "white", "black", "blue", "orange", "green", "yellow")
      val <- runif(length(words))
      df <- data.frame(word = words, prob = val/sum(val)) %>%
        arrange(desc(prob)) %>%
        filter(row_number() <= 3)
    }
    return(df)
  })
  
  ######################## CONTROLLER ########################
  
  output$next_word_table <- renderTable({
    get_next_words() 
  })
  
  output$analogies_word_table <- renderTable({
    find_analogy() %>%
      rename("Résultats" = word,
             "Probabilité" = prob)
  })
  
  output$keyword_table <- renderTable({
    find_analogy() %>%
      rename("Résultats" = word,
             "Probabilité" = prob)
  })
})
