library(shiny)
library(shinydashboard)

page <- dashboardPage(
  
  ################### HEADER ###################
  dashboardHeader(title = "C'est Beauw"),
  
  ################### SIDEBAR ###################
  dashboardSidebar(
    collapsed = T
  ),
  
  ################### BODY ###################
  dashboardBody(
    # Fenetre d'es analogies d'auto complétion
    box(title = "Autocomplétion",
        status = "primary",
        width = 12,
        solidHeader = T,
        collapsible = T,
        # box content :
        fluidRow(
          column(width = 7,
                 textInput(inputId = "sentence",
                           label = h3("Veuillez écrire une phrase"),
                           value = "skjdfjskuoouiuorizldjfkl sjkdfhmlkjfglkjsdg")),
          column(width = 5,
                 tableOutput("next_word_table"))
          )),
    # Fenetre des analogies
    box(title = "Analogies",
        status = "primary",
        width = 12,
        solidHeader = T,
        collapsible = T,
        # box content :
        fluidRow(
          column(width = 3,
                 textInput(inputId = "analogy1",
                           label = h3("Mot 1 - "),
                           value = "Kidlfsjkng")),
          column(width = 3,
                 textInput(inputId = "analogy2",
                           label = h3("Mot 2 + "),
                           value = "majkldsfjlskn")),
          column(width = 3,
                 textInput(inputId = "analogy3",
                           label = h3("Mot 3 = "),
                           value = "womsldkfman")),
          column(width = 3,
                 tableOutput("analogies_word_table"))
        ))
  )
)
