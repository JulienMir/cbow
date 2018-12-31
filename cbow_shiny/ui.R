library(shiny)
library(shinydashboard)

dashboardPage(
  
  ################### HEADER ###################
  dashboardHeader(title = "C'est Beauw"),
  
  ################### SIDEBAR ###################
  dashboardSidebar(
    collapsed = T
  ),
  
  ################### BODY ###################
  dashboardBody(
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
                           value = "My favourite color is")),
          column(width = 5,
                 tableOutput("next_word_table"))
          ))
  )
)