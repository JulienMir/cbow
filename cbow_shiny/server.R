library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)

shinyServer(server <- function(input, output, session) {
  
  # Load learned representations
  # Found at https://vsmlib.readthedocs.io/en/latest/tutorial/getting_vectors.html#pre-trained-vsms
  representations <- vector("list", 3)
  # representations[[1]] <- list(vectors=numpyload("chemin du jeu"), vocab=numpyload("chemin du jeu"))
   
  ######################## GENERAL FUNCTIONS ########################
  find_word <- function(word) {
    return(which())
  }
    
  cosine_similarity <- function(a, b) {
    return(sum(a*b)/(sqrt(sum(a*a)) * sqrt( sum( b*b ) ) ))
  }
  
  closest_words <- function(a, n=5) {
    similarity <- numeric(length(words))
    for(i in 1:length(words)) {
      similarity[i] <- cosine_similarity(a, vectors[i,])
    }
    ordered <- words[order(-similarity)]
    return(ordered[1:n])
  }
  
  # Stops the app when it is closed in navigator
  session$onSessionEnded(stopApp)
  
  ######################## MODEL ########################
  ### Autocomplétion
  # Predicts the next possibles words when the text input changes
  get_next_words <- reactive({
    sentence <- input$sentence
    
    if (nchar(sentence) == 0){
      # Empty df
      df <- data.frame(word = character(0L),
                       prob = numeric(0L))
    }else{
      sentence <- strsplit(sentence, " ")
      
      # for(vectors in representations) {
      #   
      #   val <- 0
      #   for(token in sentence) {
      #     
      #   }
      # }
    }
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
    get_next_words() %>%
      rename("Mot suivant" = word,
             "Probabilité" = prob)
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
