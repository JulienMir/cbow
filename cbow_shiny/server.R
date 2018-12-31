library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)

shinyServer(function(input, output, session) {
  
  # Stops the app when it is closed in navigator
  session$onSessionEnded(stopApp)
  
  ######################## MODEL ########################
  
  # Predicts the next possibles words when the text input changes
  get_next_words <- reactive({
    sentence <- input$sentence
    
    if (nchar(sentence) == 0){
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
  
})
