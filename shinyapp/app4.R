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


prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

# data -----------------------------------------------------------


# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }

           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }

            var mytype = getUrlParam('type','Empty');

            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");

                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }

           var x = document.getElementsByClassName('navbar-brand');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/node/451\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic');
           }
           "

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
                                   h1(strong("LivDiv"),
                                      h2(strong("Livelihood Diversification Using High-Frequency Data")),
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
                                          p(strong("Sundarbans,"), "Largest delta in the world in India & Bangladesh"),
                                          p("Population of over 4 million"),
                                         
                                   ),
                                   column(4,
                                          h2(strong("Our Work")),
                                    
                                          p(),
                                          p("Provide insights to help stakeholders design effective and targeted poverty-reducing strategies to aid those affected by natural disasters and climate change​"),
                                          p(),
                                          p("Our conclusions "),
                                          
                                          p(),
                                          p("Aims")
                                   ),
                                   
                                   column(4,
                                          h2(strong("Project Outcomes")),
                                          p(" We hope that the analysis presented here will be useful  "),
                                          tags$li(strong("Woot"))
                                          
                              
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021'))))
                 ),
                 
                 ## Tab Date Intro--------------------------------------------
                 tabPanel("Data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("We have BL and FD"), align = "center"),
                                   p("", style = "padding-top:10px;")
                                   
                          ) 
                 ), 
                 ## Tab Demographics --------------------------------------------
                 tabPanel("Demographics",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Demographic Characteristics"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4, 
                                          h4(strong("Households")),
                                          p("We examined 306 hh."),
                                          p("October 2019"), 
                                          p("Our interactive plots visualize village level sociodemographic characteristics of households. 
                                       This format allows for easy digestion and comparison of factors to help us best understand who the households on a statistical level.")
                                   ) ,
                                   column(8,
                                          h4(strong("Resident Socioeconomic Characteristics by Census Tract or Block Group")),
                                          selectInput("char1", "Select Variable:", width = "100%", choices = c(
                                            "Median Household Income" = "income",
                                            "Household Head Average Age" = "age" ,
                                            "Occupation" = "unemploy",
                                            "Education" = "high",
                                          )
                                          ), 
                                          withSpinner(leafletOutput("demo1")) , 
                                          p(tags$small("Data Source: BL October 2019")),
                                   ) 
                                   
                          ) 
                 ), 
                 
                 
                # FD data tab-----------------------------------------------------------
                tabPanel("High Frequency Data", value = "",
                         fluidRow(style = "margin: 6px;",
                                  h1(strong("FD"), align = "center"),
                                  p("", style = "padding-top:10px;"),
                                  column(12,h4(strong("Overview")),
                                         p("Over time"),
                                         br("")
                                          
                                          
                                          ) ) ),      
                ## Shocks Tab --------------------------------------------
                tabPanel("Shocks",
                         fluidRow(style = "margin: 6px;",
                                  h1(strong("Shocking"), align = "center"),
                                  p("", style = "padding-top:10px;")
                                  
                         ) 
                ), 
                
                ## FGD tab------------------------------------------
                tabPanel("Focus Group Discussion",
                         fluidRow(style = "margin: 6px;",
                                  h1(strong("FGD"), align = "center"),
                                  p("", style = "padding-top:10px;")
                                  
                         ) 
                ), 
                 # team tab -----------------------------------------------------------
                 tabPanel("Meet the Team", value = "contact",
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                            h1(strong("Contact"), align = "center"),
                            br(),
                            h4(strong("Virginia Tech Data Science for the Public Good")),
                            p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                              "is a summer immersive program held at the", a(href = 'https://aaec.vt.edu/s', 'Virginia Tech Department of Agricultural and Applied Economics.'),
                              "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around
                              critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences
                              to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program
                              highlights, how to apply, and our annual symposium, please visit", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'the official VT DSPG website.', target = "_blank")),
                            p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                            column(6, align = "center",
                            h4(strong("DSPG Team Members")),
                            img(src = "team-tim.jpeg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                            p(tags$small(a(href = 'https://www.linkedin.com/in/timothyspierce', 'Timothy Pierce', target = '_blank'), "(Virginia Tech, Agricultural and Applied Economics)"),),
                            img(src = "team-mousa.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                            p(tags$small(a(href = 'https://www.linkedin.com/in/reginald-mousa-toure-32b550106/', 'Mousa Toure', target = '_blank'), "(Virginia State University, Computer Science)"),),
                            
                            img(src = "team-christina.jpeg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                            p(tags$small(a(href = 'https://www.linkedin.com/in/christina-prisbe-60966b218/', 'Christina Prisbe', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics)."),),
                            
                            #p(a(href = 'www.linkedin.com/in/timothyspierce', 'Timothy Pierce', target = '_blank'), "(Virginia Tech, Agricultural and Applied Economics);",
                            #  a(href = 'https://www.linkedin.com/in/reginald-mousa-toure-32b550106/', 'Mousa Toure', target = '_blank'), "(Virginia State University, Computer Science);",
                            #  a(href = 'https://www.linkedin.com/in/christina-prisbe-60966b218/', 'Christina Prisbe', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics)."),
                            p("", style = "padding-top:10px;")
                            ),
                            column(6, align = "center",
                            h4(strong("Faculty and Associate Team Members")),
                            img(src = "faculty-gupta.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                            p(tags$small(a(href = "https://aaec.vt.edu/people/faculty/gupta-anubhab.html", 'Dr. Anubhab Gupta', target = '_blank'), "(Faculty Lead, Virginia Tech)"),),
                            
                            img(src = "faculty-mulu.jpeg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                            p(tags$small(a(href = "https://www.vsu.edu/cet/departments/technology/faculty-staff/kahsai-mulugeta.php", 'Dr. Mulugeta Kahsai', target = '_blank'), "(Faculty Affiliate, Virginia State University)"),),
                            
                            img(src = "team-leo.jpeg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                            p(tags$small(a(href = 'https://aaec.vt.edu/people/graduatestudents/index/quaye-leonard-allen.html', 'Leonard-Allen Quaye', target = '_blank'), "(Research Associate, Virginia Tech)"),),
                            
                            #p(a(href = "https://aaec.vt.edu/people/faculty/gupta-anubhab.html", 'Dr. Anubhab Gupta', target = '_blank'), "(Faculty Lead, Virginia Tech);",
                            #  a(href = "https://www.vsu.edu/cet/departments/technology/faculty-staff/kahsai-mulugeta.php", 'Dr. Mulugeta Kahsai', target = '_blank'), "(Faculty Affiliate, Virginia State University);",
                            #  a(href = 'https://aaec.vt.edu/people/graduatestudents/index/quaye-leonard-allen.html', 'Leonard-Allen Quaye', target = '_blank'), "(Research Associate, Virginia Tech)."),
                            p("", style = "padding-top:10px;")
                            )
                            ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                            h4(strong("Project Stakeholders")),
                            p("VPI-SU Extension Professionals, Board of Supervisions, local government organizations, local field offices, and County Planning Commission in Rappahannock county"),
                            p("", style = "padding-top:10px;"),
                            h4(strong("Acknowledgments")),
                            p("We would like to thank Kenner Love, Unit Coordinator Extension Agent, Agricultural and Natural Resources Crop & Soil Sciences from the Virginia Cooperative Extension for his support on this project.")
                          )
                 ),
                 inverse = T)



# server -----------------------------------------------------------

server <- function(input, output) {
  runjs(jscode)
  
  
  char1 <- reactive({
    input$char1
  })
  output$demo1 <- renderLeaflet({
    if(char1() == "home") {
      pal <- colorNumeric(palette = "viridis", 
                          domain = home$estimate)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              home$NAME,
              "<br />",
              "<strong>Home Value: </strong>",
              formatC(home$estimate, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      home %>%
        leaflet(width = "100%") %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.7,
                    label = labels,
                    color = ~ pal(estimate)) %>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = ~ estimate,
                  title = "Median Home Value",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = .7)
      
     } else if (char1() == "age") {
        pal <- colorNumeric(palette = "viridis", 
                            domain = age$estimate)
        
        labels <- lapply(
          paste("<strong>Area: </strong>",
                age$NAME,
                "<br />",
                "<strong>Median Age: </strong>",
                formatC(age$estimate, format = "f", digits = 0)),
          htmltools::HTML
        )
        
        
        age %>%
          leaflet(width = "100%") %>%
          addProviderTiles(provider = "CartoDB.Positron") %>%
          addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                      stroke = FALSE,
                      smoothFactor = 0,
                      fillOpacity = 0.7,
                      label = labels, 
                      color = ~ pal(estimate)) %>%
          addLegend("bottomright", 
                    pal = pal, 
                    values = ~ estimate,
                    title = "Median Age",
                    labFormat = labelFormat(),
                    opacity = 1)
        
        
      
    }else if(char1() == "poverty") {
      pal <- colorNumeric(palette = "viridis", 
                          domain = poverty$estimate)
      labels <- lapply(
        paste("<strong>Area: </strong>",
              poverty$NAME,
              "<br />",
              "<strong>Population Estimate: </strong>",
              formatC(poverty$estimate, format = "f", digits = 0)),
        htmltools::HTML
      )
      poverty %>%
        leaflet(width = "100%") %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.7,
                    label = labels,
                    color = ~ pal(estimate)) %>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = ~ estimate,
                  title = "Population Income below Poverty",
                  labFormat = labelFormat(suffix = ""),
                  opacity = .7)
    }
    
  }) 
  
  
  runjs(jscode)

}

shinyApp(ui = ui, server = server)

