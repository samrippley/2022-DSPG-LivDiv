#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(knitr)
library(ggpubr)

load("~/Virginia Tech/Internship 2022/2022-DSPG-LivDiv-/data/livdivdata.RData")

baseline <- livdiv %>%
  slice(1:307,)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("myplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$myplot <- renderPlot({
      ggplot(baseline, aes(x = head_age)) +
        geom_histogram(fill = "cornflowerblue", 
                       color = "white", bins = 20
        ) + 
        labs(title="Age of Household Heads",
             y = "Number of Household Heads") +
        theme_classic() +
        scale_x_continuous(breaks = c(20, 30, 40, 50, 60, 70, 80), name="Age", limits=c(20, 80)) 
      
})
}

# Run the application 
shinyApp(ui = ui, server = server)
