
# Load Packages ---------------------------------------------------------------
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(stringr)
library(shinyjs)
library(mapdata)
library(htmlwidgets)
library(leafpop)
library(lattice)
library(ggplot2)
library(htmltools)
library(tigris)
library(leaflegend)
library(dplyr)
library(ggplotify)
library(grid)
library(gridExtra)
library(ggpubr)
library(lubridate)
library(shinyWidgets)


prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

<<<<<<< HEAD
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
        ),

    )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })

}

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
village_vector <- c("Amrabati", "Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar",
                    "Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur") 

################################################################################################################################################################
# user -------------------------------------------------------------
ui <- navbarPage(title = "DSPG-LivDiv 2022",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 
                 # main tab -----------------------------------------------------------
                 tabPanel("Project Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Availability of Services:"),
                                      h2(strong("Evolving Demographics, Housing, and Traffic in Rappahannock")),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Polytechnic Institute and State University"),
                                      #h4("[updat this]"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Project Introduction")),
                                          p(strong("Rappahannock,"), "a rural county situated in north of the Commonwealth of Virginia, 
                                            separated from Culpeper County by an Act of the General Assembly in 1833. 
                                            In the late 16th and the early 17th century,
                                            new settlers mainly of English descent moved to certain parts of the present-day Rappahannock County from northern ports and other regions of Virginia. 
                                            About 65 miles southwest of Washington DC and 120 miles northwest of Richmond, 
                                            Rappahannock is surrounded in the northwest by the Blue Ridge Mountains and in the southwest by Shenandoah National Park.
                                            Despite the geographical proximity to the state and national capital, an aging population in rural Rappahannock faces geographical isolation, income inequality,
                                            lack of local services, and rising housing prices amidst tensions around land conservation."),
                                          p("With the help of the Virginia Cooperative Extension personnel and other stakeholders in Rappahannock county, 
                                            our research team identified the pressing challenges faced by the current inhabitants in the county. 
                                            The priority challenges surrounding demographics, income inequality, availability of services, transportation, housing, etc. 
                                            were discussed in a meeting between the research team and the stakeholders at the inception of the project.
                                            In the initial meeting, there seemed to be a lack of compiled data that the county stakeholders could analyze and visualize,
                                            which has been inhibiting them from taking actionable policies to improve certain focus areas identified as challenges for Rappahannock residents."),
                                          
                                   ),
                                   column(4,
                                          h2(strong("Our Work")),
                                          
                                          p(),
                                          p("The research team compiled and utilized publicly available data from the American Community Survey (ACS) 
                                            and Virginia Department of Transportation’s Annual Average Daily Traffic (AADT) to explore the questions and concerns held by stakeholders.
                                            We implemented the data science approach to analyze and create four priority areas for Rappahannock county. In our temporal analysis (wherever applicable), 
                                            we use a 10 year time frame from 2010-2019."),
                                          p(),
                                          p("Our conclusions based on state-of-the-art techniques of data analysis and visualization allow the stakeholders to better understand the interplay of the county’s current profile with the present and recently evolved characterization of housing market, availability of services, and changing traffic patterns. The combined analysis helps correlate the several challenges in Rappahannock both spatially and how they evolved over the last decade and helps provide directions for policies that could help the county residents in the future. "),
                                          
                                          p(),
                                          p("This dashboard compiles our findings and allows users to explore the information interactively. Based on our analysis, we observe that Rappahannock population is aging with significantly higher proportion of seniors when compared to other counties in Virginia. Data on housing prices suggest that houses have turned out to be more expensive in the last decade. There is substantial heterogeneity across districts on composition of race, education, housing prices, availability of services, and the evolving traffic around major route segments within the county."),
                                          p("A decadal trend of traffic patterns in conjunction with locations of services suggest that average daily traffic increased in areas within the county where there are service clusters and in routes connecting nearby towns of neighboring counties that have essential amenities. Migration out of Rappahannock could potentially be correlated with lack of amenities in certain districts and in population hubs within the county. There are seemingly plausible policy implications based on this research that could focus on ensuring availability of essential services catering to the different needs of the economically poorer and aging population of Rappahannock. This could potentially distribute traffic from some of the high-volume traffic segments to other locations where centers providing essential amenities could be established.")
                                   ),
                                   
                                   column(4,
                                          h2(strong("Project Outcomes")),
                                          p(" We hope that the analysis presented here will be useful for VPI-SU Extension Professionals, Board of Supervisions, local government organizations, local field offices, and County Planning Commission in Rappahannock county.
                                              The three priority areas of analysis are: "),
                                          tags$li(strong("County Profile:"), "A characterization by the five Rappahannock districts and over time of age composition and prevalence of dependency, income distribution, 
                                                  household description and ownership of housing and vehicles, and access to computer and internet. Included in the County Profile, an overview of the distribution of housing prices in the five districts and how that has changed over time. This district-by-district analysis of the housing market displays where more expensive houses (over $500,000) were built over the last decade. It also shows the total number of houses in each housing price bracket, by district. "),
                                          tags$li(strong("Traffic:"), "Using last 10 years AADT data from Virginia Department of Transportation, we analyze and provide a visualization of volume and percentage change of traffic in major route segments within Rappahannock. The interactive map shows the routes where traffic volume has expanded as well as segments where traffic volume shrunk. Additionally, we also display the annual change in traffic volume for each year in the last decade for all the chosen segments.  ."),
                                          tags$li(strong("Services:"), " To explore the availability of services, we sourced publicly available online data from the ACS, The Guide by Rappahannock News, the Aging Together’s Regional Resource Guide, and various undigitized pamphlets gathered from the Rappahannock County Library. We georeferenced this list of services and overlayed them on a map of population distribution by district. We then create driving distance isochrones that show which services can reached within 15 or 30 minutes of the district population-centroid. Population centroid data are provided by the ACS. Since some services are only available outside of Rappahannock, we included clusters of services in Culpeper and Warrenton. Our interactive map displays the pattern of driving distance isochrones, population densities, and available services within and outside of Rappahannock.")
                                          
                                          
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021'))))
                 ),
                 
                 # county profile tab -----------------------------------------------------------
                 tabPanel("Data", value = "socio",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Sociodemographic Profile of Rappahannock County "), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(12,
                                          h4(strong("Who lives in Rappahannock County?")),
                                          p("With a little over 7,000 residents, around 80 percent of every district's population is White, followed by Black, Asian, and the remaining from other races, respectively. As compared to other counties in the Commonwealth of Virginia with 15 percent seniors (65 and above), Rappahannock's population is relatively elderly with about 26 percent over 65 years old. Age dependency, defined as the proportion of the sum of children (under 18 years) and seniors (65 years and over) to the working-age adult (18-64 years) is significantly higher in Rappahannock when compared to other counties in Virginia."),
                                          p("The median age of Rappahannock is 50 as compared to Virginia's median age of 38. The average age of Rappahannock has continuously risen in the last decade. Hampton district has the highest (around 32 percent) and Wakefield district has the lowest (around 18 percent) of proportion of senior population, respectively. In terms of age dependency, Hampton has around 95 percent age dependent population, the highest in the county, whereas Piedmont has about 50 percent, the lowest. "))
                          ),
                          tabsetPanel(
                            
                            tabPanel("Population",
                                     column(9,
                                            withSpinner(plotOutput("popplot",  height = "800px")),
                                            
                                     ),
                                     column(3,
                                            h4(strong("Population Demographic"), align = "center"),
                                            p("The graph presents linear trends of population over time (2010-2019) by districts. The relative thickness of each trend represents the relative size of population living in each district. Wakefield district had its population plummet from 2012 to 2014, but the population has increased ever since for the district to be the mostly populated in Rappahannock. The population in Hampton district has decreased considerably since 2010.")
                                            
                                     )
                                     
                            ),
                            
                            tabPanel("Race",
                                     
                                     column(9, 
                                            selectInput("racedrop", "Select Variable:", width = "100%", choices = c(
                                              "Non-White Population by District" = "race1",
                                              "Black Population by District" = "race2",
                                              "Asian Population by District"= "race3",
                                              "Other Population by District"= "race4")),
                                            withSpinner(plotOutput("raceplot", height = "800px")),
                                            #p(tags$small("Data Source: ACS Five Year Estimate Table ????"))
                                            
                                     ),
                                     column(3,
                                            h4(strong("Race Demographic"), align = "center"),
                                            p("Rappahannock has a predominantly White population that has consistently remained over 85 percent. The analysis presented here considers and graphically represents the evolution of races other than White in this county. "),
                                            h5(strong("Non-White Population by District")),
                                            p("The graph shows the composition of non-white races by district in Rappahannock over the last decade. The total length of the bars in each year in the graph shows the percentage of non-white population, which has remained consistently between 9 and 12 percent. Over the last decade, non-white population in Stonewall-Hawthorne has decreased substantially as compared to other districts, while that in Piedmont and Hampton have increased fractionally."),
                                            h5(strong("Black Population by District")),
                                            p("The Black community is the second most populous among other races in the county, which has slowly decreased over the last decade from little over 6 percent to less than five percent. Relatively, over the last ten years, the proportions of Black households have reduced considerably in Jackson and Stonewall-Hawthorne districts, while those in Hampton and Piedmont have increased substantially.  "), 
                                            h5(strong("Asian Population by District")),
                                            p("The Asian population has doubled in the last decade, but it’s contribution to the county’s population is still less than half a percent. The Asian population have been distributed around Hampton, Jackson, and Stonewall-Hawthorne districts."),
                                            h5(strong("Other Population by District")),
                                            p("Other races comprise of First Natioins, Mixed, and Oceania who add up to a little over 4 percent of Rappahannock population. They seem to be evenly distributed across the districts except for Stonewall-Hawthorne, which has the lowest population proportion of other races."), 
                                     )
                                     
                                     
                            ),
                            
                            
                            tabPanel("Age",
                                     
                                     column(9, 
                                            selectInput("agedrop", "Select Variable:", width = "100%", choices = c(
                                              "Age Composition" = "ageGroups",
                                              "Age Composition Over Time" = "ageTime",
                                              "Age Composition Over Time by District" ="ageTime2",
                                              "Median Age" = "medAge",
                                              "Age Dependency" = "ageDep")),
                                            withSpinner(plotOutput("ageplot", height = "800px")),
                                            
                                     ),
                                     column(3,
                                            h4(strong("Age Demographic"), align = "center"),
                                            h5(strong("Age Composition")),
                                            p("The pie charts show the age proportions for Rappahannock and Virginia in 2019. Rappahannock county has 11 percent greater proportion of senior population as compared to that of Virginia. Rappahannock has a noticeably smaller percent of adolescent and young adult populations than the rest of Virginia."),
                                            
                                            
                                            h5(strong("Age Composition Over Time")),
                                            p("The pie charts show the age proportions for Rappahannock and Virginia in 2019. Rappahannock county has 11 percent greater proportion of senior population as compared to that of Virginia. Rappahannock has a noticeably smaller percent of adolescent and young adult populations than the rest of Virginia."),
                                            
                                            h5(strong("Age Composition Over Time by District")),
                                            p("The districts of Hampton and Jackson have witnessed an increase in the percentage of senior population than the three other districts (Piedmont, Stonewall-Hawthorne, and Wakefield). There is also a significant reduction in the 30-65 year age category for Hampton, Jackson, and Stonewall-Hawthorne."),
                                            
                                            
                                            h5(strong("Median Age")),
                                            p("Rappahannock has a higher median age than its surrounding counties. The median age for the state of Virginia is 38 years whereas that of Rappahannock is 50 years. Rappahannock's districts all show the same trend of a high median age, except the district of Wakefield, which is almost the same as the state median age."),
                                            
                                            
                                            h5(strong("Age Dependency")),
                                            p("The age dependency ratio is an age-population ratio for dependents i.e., those who are not in the labor force. An overall age dependency ratio accounts for all ages of dependents, the child dependents as well as the old-age dependents. The child dependency ratio accounts for dependents under the age of 18, and the old-age dependency ratio accounts for dependents the age 65 and over. The dependency ratio is calculated by taking ratio of the number of people with ages below 18 and 65 and over to the number of people with ages between 18 and 64. The charts show that Rappahannock has the highest age dependency ratios for the overall age and old-age but has the lowest ratio for child when compared to Virginia and the surrounding counties.")
                                     ),
                                     
                            ),
                            
                            
                            tabPanel("Household Characteristics",
                                     column(8,
                                            selectInput("hcdrop", "Select Variable:", width = "100%", choices = c(
                                              "Household Size" = "houseSize",
                                              "Housing Units Occupied by Owners and Renters" = "rentOwn",
                                              "Vehicles per Household" = "vehicles")
                                            ),
                                            withSpinner(plotOutput("hcplot", height ="800px")),
                                            
                                            
                                     ),
                                     column(4,
                                            h4(strong("Houshold Characteristics"), align = "center"),
                                            h5(strong("Household Size")),
                                            p("The pie chart shows the distribution of household sizes in Rappahannock for 2019. 68.2 percent of Rappahannock families had a household size of two or less. About 18 percent of the households had three members, with around 14 percent households having more than four members, respectively."),
                                            
                                            h5(strong("Housing Units Occcupied by Owners and Renters")),
                                            p("The graphs plot the housing units (owner- or renter- occupied) on the vertical axis and time on the horizontal axis. Note that the scales in the two plots are different to capture the temporal changes visually. In 2010, more than 70 percent of the housing units were owner occupied and a little less than 30 percent were renter occupied. In 2014, more than 80 percent of the housing units were owner occupied, the highest proportion in the last decade. However, in 2019, less than 2200 units are owner occupied and 750 units are renter occupied, which are 67 percent and 23 percent, respectively, of the total housing units."),
                                            
                                            h5(strong("Vehicles per Household")),
                                            p("To visualize the vehicles owned per household, we present a pie-chart of the number of vehicles (categories: none to three), and the distribution for Rappahannock inhabitants as compared to neighboring districts. About 40 percent of Rappahannock households have three vehicles, and less than 3 percent have no vehicles. The distribution of vehicle ownership (by number of vehicles) in Rappahannock seems quite comparable to neighboring districts.")
                                            
                                     )  
                                     
                            ),
                            
                            tabPanel("Education",
                                     column(8,
                                            withSpinner(plotOutput("eduplot", height ="800px"))
                                            
                                            
                                     ),
                                     column(4,
                                            h4(strong("Education"), align = "center"),
                                            h5(strong("Educational Attainment")),
                                            p("The bars in the graph show the composition of educational qualification across districts with the heights of the bars adjusted by the respective population proportions. Hampton, Piedmont, and Stonewall-Hawthorne districts have the highest proportion of adults with bachelor’s degree and/or above. While almost an eight of Piedmont and Wakefield’s population have education less than a high school degree, Hampton, Stonewall-Hawthorne, and Jackson have relatively lower (less than 10 percent) of their population who have less than high-school education.  ")
                                            
                                            
                                     )  
                                     
                            ),
                            
                            tabPanel("Income",
                                     column(8,
                                            withSpinner(plotOutput("incomePlot", height = "1000px"))
                                            
                                     ),
                                     column(4,
                                            h4(strong("Income Description"), align = "center"),
                                            h5(strong("Income")),
                                            p("The 10-panel graph presents the annual household income distribution of Rappahannock districts over the period 2010 to 2019. We classified median annual household incomes into four bins: under $25,000, $25,000 to $50,000, $50,000 to $100,000, and above $100,000. The length of each bar captures the relative proportion of the district’s population in Rappahannock. All the five bars add up to a 100 percent for every year.  "),
                                            p("In 2019, Jackson and Wakefield have the highest proportion of households with annual incomes over $100,000. The proportion of households with less than an annual income of $25,000 has decreased over time in Stonewall-Hawthorne district while that in Wakefield has increased in the last decade.  ")
                                     )
                                     
                                     
                            ),
                            tabPanel("Broadband",
                                     
                                     column(8,
                                            selectInput("bbdrop", "Select Variable:", width = "100%", choices = c(
                                              "Internet Subscription by Income in Rappahannock" = "intIncome",
                                              "Internet Subscription and Computer Ownership by District" = "compDist")
                                            ),
                                            withSpinner(plotOutput("bbplot", height ="800px")),
                                            
                                            
                                     ),
                                     column(4,
                                            h4(strong("Broadband"), align = "center"),
                                            h5(strong("Internet Subscription by Income")),
                                            p("The graph presents the distribution of internet subscription based on income distribution. We use the three ACS income categories for classification of internet subscription. Residents with a higher income are more likely to have an internet subscription. About two-thirds households with less than annual income of $20,000, one-fourth households with annual income between $20,000 and $75,000, respectively, do not have internet subscription. For households with annual income greater than $75,000, only 8 percent do not have internet subscription."),
                                            h5(strong("Internet Subscription and Computer Ownership by District")),
                                            p("The bar graph shows internet subscriptions and computer ownership by Rappahannock's districts. For both internet subscriptions and computer ownership, Hampton and Jackson districts have the
                                            higher percentages of residents with internet and computers, while Stonewall-Hawthorne has the lowest percentage of internet subscriptions and computer ownership. Over 80 percent of households in Jackson and Hampton have internet and more than 90 percent own at least one computer, respectively.")
                                            
                                     )
                                     
                            ),
                            tabPanel("Housing Market",
                                     
                                     column(8,
                                            selectInput("hmdrop", "Select Variable:", width = "100%", choices = c(
                                              "Housing Prices" = "housing1",
                                              "Housing Prices by District" = "housing2")
                                            ),
                                            withSpinner(plotOutput("hmplot", height ="800px")),
                                            
                                            
                                     ),
                                     column(4,
                                            h4(strong("Housing Market Description"), align = "center"),
                                            h5(strong("Housing Prices")),
                                            p("The graph presents the distribution of homes by housing prices in the last decade. The bins are classified in five housing-price categories: less than $100,000, between $100,000 and $300,000, between $300,000 and half million, between half- and one- million, and over one- million. Less than 5 percent homes are below $100,000 in Rappahannock. Houses in the price range of $100,000 to $300,000 comprise of more than 30 percent of all available houses. The proportion of households in the price range from half- to a million dollars has decreased from more than 30 percent in 2010 to little below 25 percent in 2019. On the other hand, proportion of houses with prices in the range of $300,000 to $500,000 has increased in the last decade. "),
                                            h5(strong("Housing Prices (in US dollars) from 2010 to 2019")),
                                            p("In this graph, we present a similar distribution of housing prices as in the previous tab, but by districts. This graph also presents the relative composition of houses in terms of their prices in each of the Rappahannock districts, and how that has changed in the last 10 years.  "),
                                            p("During the last decade, all Rappahannock districts had less than 10 percent houses whose prices were less than $100,000. The number of houses between $100,000 and $300,000 has increased to almost 60 percent of the total houses in Wakefield. Wakefield has the highest proportion of houses in this price range (100-300k) followed by Piedmont. Jackson is the district with highest proportion (about 50 percent) of houses between the price range of $300,000 to $500,000.  ")
                                     )
                                     
                            )
                            
                          )
                          
                          
                 ))
                 
>>>>>>> 8926853bf6041710db63ffd89ee8720dd79ee7fe

# Run the application 
shinyApp(ui = ui, server = server)
