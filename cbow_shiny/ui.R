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
          column(width = 2,
                 checkboxGroupInput("auto_model_choice", label = "Choix des modèles", 
                                    choices = list("CBOW Unbound" = 1, "SkipGram Unbound" = 2),
                                    selected = 1)),
          column(width = 6,
                 textInput(inputId = "sentence",
                           label = "Veuillez écrire une phrase",
                           value = "The journalist reported that there was a terrible")),
          column(width = 2, style = "margin-top: 24px;",
                 actionButton("auto_button", "Valider")),
          column(width = 3,
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
          column(width = 2,
                 checkboxGroupInput("ana_model_choice", label = "Choix des modèles", 
                                    choices = list("CBOW Unbound" = 1, "SkipGram Unbound" = 2),
                                    selected = 1)),
          column(width = 2,
                 textInput(inputId = "analogy1",
                           label = "Mot 1 - ",
                           value = "mother")),
          column(width = 2,
                 textInput(inputId = "analogy2",
                           label = "Mot 2 + ",
                           value = "father")),
          column(width = 2,
                 textInput(inputId = "analogy3",
                           label = "Mot 3 = ",
                           value = "daugther")),
          column(width = 2, style = "margin-top: 24px;",
                 actionButton("ana_button", "Valider")),
          column(width = 2,
                 tableOutput("analogies_word_table"))
        )),
    # Fenêtre de la visualisation des projections
    box(title = "Visualisation des projections",
        status = "primary",
        width = 12,
        solidHeader = T,
        collapsible = T,
        # box content :
        fluidRow(
          column(width = 4,
                 column(width = 12,
                        textInput(inputId = "visu_text",
                                  label = "Saisissez les mots à visualiser",
                                  value = "car bike cat dog raspberry strawberry")),
                 column(width = 9,
                        # selectInput("visu_model_choice", label = "Choix du modèle", multiple = FALSE,
                        #             choices = list("CBOW Unbound" = 1, "SkipGram Unbound" = 2),
                        #             selected = 1),
                        checkboxGroupInput("visu_model_choice", label = "Choix des modèles", 
                                           choices = list("CBOW Unbound" = 1, "SkipGram Unbound" = 2),
                                           selected = 1)),
                 column(width = 3, style = "margin-top: 24px;",
                        actionButton("visu_button", "Valider"))),
          column(width = 8,
                 plotOutput("visu_graph"))
        ))
  )
)