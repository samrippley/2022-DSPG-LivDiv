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
      checkboxGroupInput("village", "Select village", choices = c("Amrabati", "Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar",
                                                           "Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur"), 
                         selected = c("Amrabati", "Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar",
                                      "Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur")),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("rmt"),
      plotOutput("rmt_all")
    ),
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  filtered <- reactive({
    dplyr::filter(rmt_data_mean_weeks, Villages==input$village)
  })

  
  output$rmt <- renderPlot({
    ggplot(filtered(), aes(x = weeks_rep
                                    , y = mean_rmt_per_week, color = Villages)) + 
      geom_line() +
      theme_classic() +
      labs(x = "Date", y = "Average Remittance Income [Rupee]") +
      ggtitle("Average Remittance Income")#+ #(11/16/18 - 10/31/19)
      #scale_color_brewer(palette = "Spectral")#+
      #scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)
    #annotate(geom = "text", aes(x = unlist(months)))
    

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)