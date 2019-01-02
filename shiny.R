library(shiny)

source("cbow_shiny/ui.R")
source("cbow_shiny/server.R")

shinyApp(ui = page,
         server = server)
