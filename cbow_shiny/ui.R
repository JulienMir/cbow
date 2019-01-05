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
                                    choices = list("CBOW 25d" = 1, "SkipGram 25d" = 2, "CBOW 500d" = 3, "SkipGram 500d" = 4),
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
                           value = "daughter")),
          column(width = 1, style = "margin-top: 24px;",
                 actionButton("ana_button", "Valider")),
          column(width = 3,
                 tableOutput("analogies_word_table"))
        )),
    # Fenetre des key word
    box(title = "Complétion de requête",
        status = "primary",
        width = 12,
        solidHeader = T,
        collapsible = T,
        # box content :
        fluidRow(
          column(width = 2,
                 checkboxGroupInput("key_model_choice", label = "Choix des modèles", 
                                    choices = list("CBOW 25d" = 1, "SkipGram 25d" = 2, "CBOW 500d" = 3, "SkipGram 500d" = 4),
                                    selected = 1)),
          column(width = 6,
                 textInput(inputId = "key_text",
                           label = "Veuillez écrire une requête",
                           value = "capital france")),
          column(width = 1, style = "margin-top: 24px;",
                 actionButton("key_button", "Valider")),
          column(width = 3,
                 tableOutput("key_word_table"))
        )),
    # Fenêtre de la vraisemblance
    box(title = "Vraisemblance",
        status = "primary",
        width = 12,
        solidHeader = T,
        collapsible = T,
        # box content :
        fluidRow(
          column(width = 2,
                 checkboxGroupInput("like_model_choice", label = "Choix des modèles", 
                                    choices = list("CBOW 25d" = 1, "SkipGram 25d" = 2, "CBOW 500d" = 3, "SkipGram 500d" = 4),
                                    selected = 1:2)),
          column(width = 6,
                 textAreaInput(inputId = "like_text1",
                               label = "Veuillez écrire une phrase",
                               value = "I love eating at my best friend 's restaurant during my lunch break, the waiters are nice and the cook is tasty",
                               height = "55px")),
          column(width = 1, style = "margin-top: 24px;",
                 actionButton("like_button", "Valider")),
          column(width = 3,
                 tableOutput("like_table1"))
        ),
        fluidRow(
          column(width = 2),
          column(width = 6,
                 textAreaInput(inputId = "like_text2",
                               label = "Et une seconde si vous voulez comparer les scores de vraisemblance",
                               value = "My classification tooth and my belt 's philosophy dishwasher are late at the cemetery",
                               height = "55px")),
          column(width = 1),
          column(width = 3,
                 tableOutput("like_table2"))
        )),
    
    # Fenêtre de la visualisation des projections
    box(title = "Visualisation des projections",
        status = "primary",
        width = 12,
        solidHeader = T,
        collapsible = T,
        # box content :
        fluidRow(
          column(width = 3,
                 column(width = 12,
                        textAreaInput(inputId = "visu_text",
                                      label = "Saisissez les mots à visualiser",
                                      value = "mother father daughter son\ncapital france paris italy roma",
                                      height = "55px")),
                 column(width = 9,
                        checkboxGroupInput("visu_model_choice", label = "Choix des modèles", 
                                           choices = list("CBOW 25d" = 1, "SkipGram 25d" = 2, "CBOW 500d" = 3, "SkipGram 500d" = 4),
                                           selected = 1),
                        fluidRow(column(width = 12,
                                        checkboxInput("type_graph", label = "Regrouper les graphiques", value = F), style = "margin-top: 20px;"))
                        ),
                 column(width = 3, style = "margin-top: 93px;",
                        actionButton("visu_button", "Valider"))),
          column(width = 9,
                 plotOutput("visu_graph"))
        ))
  )
)