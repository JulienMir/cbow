library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(ggplot2)

shinyServer(server <- function(input, output, session) {
  
  ######################## INITIALIZATION ########################
  
  # Load learned representations
  # Found at https://vsmlib.readthedocs.io/en/latest/tutorial/getting_vectors.html#pre-trained-vsms
  models <- vector("list", 2)
  modelnames <- c("CBOW Unbound", "SkipGram Unbound")
  paths <- c("./data/word_deps_cbow_25d/", "./data/word_deps_sg_25d/")
  
  for(i in 1:2) {
    # Lecture du fichier NPY converti
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
  # get_next_words <- reactive({
  #   sentence <- tolower(input$sentence)
  # 
  #   if (nchar(sentence) == 0){
  #     # Empty df
  #     df <- data.frame(word = character(0L),
  #                      prob = numeric(0L))
  #   }
  #   else{
  #     sentence <- unlist(strsplit(sentence, "[ ]+"))
  # 
  #     # print(sentence)
  # 
  #     for(model in models) {
  #       res <- numeric(0)
  # 
  #       for(token in sentence) {
  #         # print(paste(token, " and ", vocab[which(model$vocab ==  token)]))
  #         res <- res + model$vectors[which(model$vocab ==  token)]
  #       }
  # 
  #       df <- cbind(df, data.frame(closest_words(res, model, 3)))
  #     }
  #   }
  # 
  #   colnames(df) <- modelnames
  # 
  #   # print(df)
  # 
  #   return(df)
  # })
  
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
  
  ### Visualization
  # Makes a list containing all the data needed for the graph, updates when button visu_button is clicked
  get_coords <- eventReactive(input$visu_button, {

    # word tokenisation (same order as input)
    words <- strsplit(tolower(input$visu_text), " ")[[1]]

    # Index of the chosen model
    mod <- as.integer(input$visu_model_choice)

    # Find words indexes (same order as in vocab)
    index <- which(models[[mod]]$vocab %in% words)

    # Reorder words (sorting by alphabetical order should be enough but that way there is no doubt)
    words <- models[[mod]]$vocab[index]

    # Getting the representations of the selected words
    repr <- models[[mod]]$vectors[index,]

    # Performs a PCA and get the vectors coordonates for the first 3 axis
    coords <- as.data.frame(prcomp(repr)$x[, 1:3])

    return(list(modelname = modelnames[mod],
                words = words,
                coords = coords))
  })
  
  ######################## CONTROLLER ########################
  ### TABLES
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
  
  ### GRAPHS
  # Updated whenever get_coords() is updated
  output$visu_graph <- renderPlot({

    data <- get_coords()
    
    ### Graph settings
    # decalage graphique
    dec <- 0.1
    
    # limits
    xmargin <- (max(data$coords$PC1) - min(data$coords$PC1))*0.08
    ymargin <- (max(data$coords$PC2) - min(data$coords$PC2))*0.02
    xlim <- c(min(data$coords$PC1)-xmargin, max(data$coords$PC1)+xmargin)
    ylim <- c(min(data$coords$PC2)-ymargin, max(data$coords$PC2)+ymargin)
    
    
    ggplot() +
      # geom_vline(xintercept = 0) +
      # geom_hline(yintercept = 0) +
      geom_text(aes(x = data$coords$PC1, y = data$coords$PC2, label = data$words,
                    hjust = ifelse(data$coords$PC1 >= 0, -1*dec, 1+dec), vjust = ifelse(data$coords$PC2 >= 0, -1*dec, 1+dec))) +
      geom_segment(aes(x = 0, y = 0, xend = data$coords$PC1, yend = data$coords$PC2), arrow = arrow()) +
      # theme_bw() +
      scale_x_continuous(breaks = NULL, limits = xlim) +
      scale_y_continuous(breaks = NULL, limits = ylim) +
      labs(title = paste("Représentation des vecteurs de", data$modelname),
           x = "1er axe de l'ACP",
           y = "2ème axe de l'ACP") +
      theme(plot.title = element_text(hjust = 0.5))


  })

})
