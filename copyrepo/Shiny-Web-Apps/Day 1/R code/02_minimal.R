library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  textInput("name", 
            "Enter your name:",
            value = "Sam")
)

ui <- dashboardPage(
  dashboardHeader(),
  sidebar,
  dashboardBody()
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
