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
library(viridis)


prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
village_vector = c("Amrabati","Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar","Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur")
# data -----------------------------------------------------------

fd <- livdiv %>%
  select(-(4:967))
# remittance data
rmt <- fd %>% 
  select(village, hhid, name, week, date, 
         rmt_total, rmt_method_bank, rmt_method_person, 
         rmt_method_mobile, rmt_method_moneyorder, rmt_method_oth, rmt_purpose_food, rmt_purpose_food,
         rmt_purpose_tuition, rmt_purpose_asset, rmt_purpose_med, rmt_purpose_oth, rmt_purpose_none)
rmt
rmt_nonZero <- rmt %>% 
  filter(rmt_total != 0)
rmt_nonZero


fd2 <- livdiv %>%
  select(-(4:967))
rmt2 <- fd %>%
  select(c(hhid, date, rmt_total, village))
rmt2$date <- as_date(rmt2$date)
# rmt plot data:
village <- c("Amrabati","Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar","Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur") 
Villages <- rep(village, 49)
weeks <- c(rep(1,10), rep(2, 10), rep(3,10), rep(4,10), rep(5,10), rep(6,10), rep(7,10), rep(8,10), rep(9,10), rep(10,10), 
               rep(11,10), rep(12,10), rep(13, 10), rep(14,10), rep(15,10), rep(16, 10), rep(17, 10), rep(18,10), rep(19,10), rep(20,10),
               rep(21, 10), rep(22,10), rep(23,10), rep(24,10), rep(25,10), rep(26, 10), rep(27,10), rep(28,10), rep(29,10), rep(30,10),
               rep(31,10), rep(32,10), rep(33,10), rep(34,10), rep(35,10), rep(36,10), rep(37,10), rep(38,10), rep(39,10), rep(40,10),
               rep(41,10), rep(42,10), rep(43,10), rep(44,10), rep(45,10), rep(46,10), rep(47,10), rep(48,10), rep(49,10)) 
week1_rmt_means <- c(0, 5166.667, 11100, 0, 5250, 950, 5000, 0, 0, 0)
week2_rmt_means <- c(2000, 6000, 4071.429, 1600, 1300, 2700, 0, 2383.333, 3000, 2625)
week3_rmt_means <- c(2000, 3000, 2033.333, 2500, 3250, 3000, 2000, 1250, 0, 2000)
week4_rmt_means <- c(0, 6333.333, 2250, 1650, 2333.333, 5000, 750, 2833.333, 4500, 10000)
week5_rmt_means <- c(0, 2125, 3950, 2850, 4000, 0, 0, 875, 3000, 0)
week6_rmt_means <- c(0, 6280, 2800, 1733.333, 0, 0, 1500, 700, 7000, 1000)
week7_rmt_means <- c(0, 2000, 3100, 1775, 3000, 900, 1666.667, 2833.333, 0, 4200)
week8_rmt_means <- c(0, 3700, 2357.143, 1450, 0, 0, 0, 4025, 0, 500)
week9_rmt_means <- c(0, 7000, 4916.667, 2220, 0, 1000, 2000, 1825, 0, 8250)
week10_rmt_means <- c(0, 5000, 3500, 3680, 0, 3833.333, 0, 1575, 0, 7000)
week11_rmt_means <- c(0, 1000, 1542.857, 2500, 1166.667, 750, 0, 1400, 2000, 900)
week12_rmt_means <- c(0, 3500, 3333.333, 1800, 7000, 0, 0, 1600, 0, 600)
week13_rmt_means <- c(0, 9333.333, 3416.667, 1600, 6333.333, 500, 0, 1625, 0, 1750)
week14_rmt_means <- c(0, 2666.667, 3959.091, 2600, 0, 0, 2000, 1250, 5000, 850)
week15_rmt_means <- c(0, 3000, 2500, 2700, 500, 1500, 0, 925, 2750, 8000)
week16_rmt_means <- c(0, 4125, 5000, 6800, 500, 0, 3200, 800, 500, 8750)
week17_rmt_means <- c(0, 3500, 4000, 1700, 500, 3000, 0, 1850, 0, 10000)
week18_rmt_means <- c(0, 30200, 3250, 6750, 20250, 2000, 3000, 4300, 0, 5750)
week19_rmt_means <- c(0, 6000, 3250, 3000, 700, 3192.5, 500, 2600, 0, 13000)
week20_rmt_means <- c(0, 1000, 2500, 0, 500, 3590, 0, 200, 0, 1500)
week21_rmt_means <- c(0, 5333.333, 3625, 3000, 300, 3500, 1000, 1730, 0, 0)
week22_rmt_means <- c(8000, 4501.250, 4250, 2633.333, 25000, 900, 1250, 3166.667, 0, 8000)
week23_rmt_means <- c(0, 2333.333, 4250, 3850, 0, 3166.667, 0, 2333.333, 0, 0)
week24_rmt_means <- c(0, 6333.333, 2285.714, 6500, 0, 7500, 1050, 1300, 0, 6500)
week25_rmt_means <- c(0, 6416.667, 3333.333, 500, 0, 5000, 0, 10000, 0, 0)
week26_rmt_means <- c(0, 6500, 2800, 2100, 0, 3000, 8000, 500, 0, 30000)
week27_rmt_means <- c(0, 3000, 5460, 4000, 20000, 0, 0, 1400, 0, 0)
week28_rmt_means <- c(0, 0, 2300, 0, 0, 2750, 0, 3000, 2000, 7500)
week29_rmt_means <- c(0, 5000, 5062.5, 15000, 10000, 2562.5, 0, 2875, 5000, 3500)
week30_rmt_means <- c(0, 5375, 2928.571, 3500, 3675, 0, 0, 1033.333, 0, 3025)
week31_rmt_means <- c(0, 1500, 3366.667, 0, 500, 5666.667, 0, 750, 7000, 0)
week32_rmt_means <- c(0, 6000, 1833.333, 6500, 300, 2000, 0, 900, 0, 9900)
week33_rmt_means <- c(0, 6000, 2742.857, 0, 450, 7000, 0, 2500, 0, 5000)
week34_rmt_means <- c(0, 2000, 2214.286, 3000, 1450, 0, 0, 500, 2000, 0)
week35_rmt_means <- c(0, 3500, 4136.364, 4433.333, 2733.333, 3400, 0, 0, 0, 32000)
week36_rmt_means <- c(0, 1000, 3000, 2000, 475, 0, 0, 1000, 0, 10000)
week37_rmt_means <- c(0, 2500, 4927.273, 5000, 2637.5, 0, 0, 500, 0, 7666.667)
week38_rmt_means <- c(0, 7400, 2657.143, 4035, 3103.333, 1500, 0, 1500, 0, 0)
week39_rmt_means <- c(0, 1000, 3500, 3000, 1875, 0, 0, 3250, 0, 0)
week40_rmt_means <- c(500, 3533.333, 2600, 5035, 1943.333, 3100, 2500, 500, 0, 900)
week41_rmt_means <- c(0, 4500, 3242.857, 0, 1220, 1000, 0, 1500, 0, 5000)
week42_rmt_means <- c(5000, 6250, 2583.333, 2500, 1000, 2500, 5000, 2750, 0, 15000)
week43_rmt_means <- c(5000, 2500, 1750, 1945, 5000, 3000, 0, 3333.333, 0, 4000)
week44_rmt_means <- c(0, 3250, 2300, 800, 400, 0, 0, 1750, 0, 20500)
week45_rmt_means <- c(0, 5333.333, 3200, 7000, 2153.333, 0, 0, 2900, 0, 0)
week46_rmt_means <- c(0, 7333.333, 5400, 3335, 1250, 2500, 0, 1866.667, 1800, 15000)
week47_rmt_means <- c(0, 4000, 3582.500, 3833.333, 5000, 0, 0, 1100, 0, 0)
week48_rmt_means <- c(0, 3900, 6875, 0, 1210, 8500, 0, 1200, 0, 0)
week49_rmt_means <- c(9700, 6500, 2228.571, 0, 500, 8000, 0, 1675, 0, 400)
mean_rmt_per_week <- c(week1_rmt_means, week2_rmt_means, week3_rmt_means, week4_rmt_means, week5_rmt_means,
                       week6_rmt_means, week7_rmt_means, week8_rmt_means, week9_rmt_means, week10_rmt_means,
                       week11_rmt_means, week12_rmt_means, week13_rmt_means, week14_rmt_means, week15_rmt_means,
                       week16_rmt_means, week17_rmt_means, week18_rmt_means, week19_rmt_means, week20_rmt_means,
                       week21_rmt_means, week22_rmt_means, week23_rmt_means, week24_rmt_means, week25_rmt_means,
                       week26_rmt_means, week27_rmt_means, week28_rmt_means, week29_rmt_means, week30_rmt_means,
                       week31_rmt_means, week32_rmt_means, week33_rmt_means, week34_rmt_means, week35_rmt_means,
                       week36_rmt_means, week37_rmt_means, week38_rmt_means, week39_rmt_means, week40_rmt_means,
                       week41_rmt_means, week42_rmt_means, week43_rmt_means, week44_rmt_means, week45_rmt_means,
                       week46_rmt_means, week47_rmt_means, week48_rmt_means, week49_rmt_means)
avgRmt <- rmt2 %>%
  select(date, rmt_total, village) %>% 
  group_by(date, village) %>%
  summarize_at(c("rmt_total"), mean, na.rm= T)

dates <- avgRmt$date
months <- c("January 2019", "April 2019", "July 2019", "October 2019")
rmt_data_mean_weeks <- data.frame(Villages, weeks, mean_rmt_per_week)
average_rmt_plot <- ggplot(rmt_data_mean_weeks, aes(x = weeks
                                                    , y = mean_rmt_per_week, color = Villages)) + 
  geom_line() +
  theme_classic() +
  labs(x = "Date", y = "Average Remittance Income [Rupee]") +
  ggtitle("Average Remittance Income Per Village")+ #(11/16/18 - 10/31/19)
  scale_color_brewer(palette = "Spectral")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)
annotate(geom = "text", aes(x = unlist(months)))
#--------------------------------------------------------------------
# rmt method plot:
method_counts <- c(397, 472, 1, 1, 13)
Method <- c("Bank", "In person", "Mobile", "Money Order", "Other")
method_dat <- data.frame(Method, method_counts, stringsAsFactors = T)
method_values <- c("       397", "       472", "   1", "   1", "    13")

rmt_method_plot <- ggplot(method_dat, aes( x= Method, y = method_counts, fill = Method)) +
  geom_col(fill = plasma(5, alpha = 1, begin = 0, end = 1, direction = 1)) +
  labs(x = "", y = "Total") +
  theme_classic() +
  coord_flip()+
  ggtitle("Method of Receiving Remittance")+
  geom_text(aes(label = method_values), size = 3)
#--------------------------------------------------------------------
# rmt purpose plot:
Purpose <-  c("Food/Utility Purchases", "Tuition", "Assets/Durable Purchases", "Medical Expenses", "Other", "No Reason")
purpose_count <- c(594, 37, 27, 93, 128, 43)
purpose_dat <- data.frame(Purpose, purpose_count, stringsAsFactors = T)
purpose_values <- c("      594", "      37", "     27", "     93", "      128", "      43")

rmt_purpose_plot <- ggplot(purpose_dat, aes(x = Purpose, y = purpose_count, fill = Purpose)) + 
  geom_col(fill = plasma(6, alpha = 1, begin = 0, end = 1, direction = 1)) +
  labs(x = "", y = "Total") +
  theme_classic() +
  ggtitle("Purpose for Receiving Remittance")+
  #rotate_x_text(angle = 22, size = rel(0.8))
  coord_flip()+
  geom_text(aes(label = purpose_values), size = 2.4)
#-----------------------------------------------------------------


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
                                            "Education" = "high"
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
                                          
                                          
                                          ) ),
                         # Sidebar with a select input for village
                         sidebarLayout(
                           sidebarPanel(
                             #tags$h2("Select/Deselect all"),
                             pickerInput("village", "Select Village:", choices = village_vector, 
                                         selected = village_vector,
                                         multiple = T, options = list(`actions-box` = T)),
                             

                           ),
                           
                           # Show a plot of the generated plot
                           mainPanel(
                             tabsetPanel(
                             tabPanel("Plot",plotOutput("rmt")),
                             tabPanel("Table",DT:: DTOutput("rmt_table")),
                             tabPanel("Method", plotOutput("rmt_method")),
                             tabPanel("Purpose", plotOutput("rmt_purpose"))
                             )
                           ),
                           
                         )
                        ),
               
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
  # rmt plot output
  filtered <- reactive({
    rmt_data_mean_weeks %>%
      #filter(Villages==input$village)
      filter(Villages %in% input$village)
  })
  
  output$rmt <- renderPlot({
    ggplot(filtered(), aes(x = weeks
                           , y = mean_rmt_per_week, color = Villages)) + 
      geom_line() +
      theme_classic() +
      labs(x = "Date", y = "Average Remittance Income [Rupee]") +
      ggtitle("Average Weekly Remittance Income")+ #(11/16/18 - 10/31/19)
      #scale_color_brewer(palette = "Spectral")+
    scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)
    #annotate(geom = "text", aes(x = unlist(months)))
    
    
  })
  output$rmt_table <- DT::renderDT({
    filtered()
  })
  output$rmt_method <- renderPlot({
    rmt_method_plot
  })
  output$rmt_purpose <- renderPlot({
    rmt_purpose_plot
  })
  
  
  runjs(jscode)

}

shinyApp(ui = ui, server = server)

