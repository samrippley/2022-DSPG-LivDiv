#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Average Weekly Remittance"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("village", "Select village", choices = c(Villages)),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("rmt"),
    ),
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$rmt <- renderPlot({
    # generate bins based on input$bins from ui.R
  average_rmt_plot
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)