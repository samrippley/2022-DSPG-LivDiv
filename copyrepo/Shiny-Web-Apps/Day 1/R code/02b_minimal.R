library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  textInput("name", "Enter your name:", value = "Heike"),
 selectInput("state", "Select your state:", choices = c("VA", "CO")))

body <- dashboardBody(
  plotOutput("myplot")
)

ui <- dashboardPage(
  dashboardHeader(title= "My App"),
  sidebar = sidebar,
  dashboardBody()
)

server <- function(input, output, session) {
  output$myplot <- renderPLot((
    ggplot() +
      ggtiltle(sprint(""))
  ))
  
}

shinyApp(ui, server)

