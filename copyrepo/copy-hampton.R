#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(leaflet.minicharts)
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
library(gridExtra)
library(stringr)
library(shinyjs)
library(plotly)
library(ggrepel)
library(shinydashboard)
library(mapdata)
library(plotrix)
library(scatterpie)
library(leafpop)

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

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
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
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
ui <- navbarPage(title = "Hampton Roads",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 
                 # Project Overview -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "VTDSPG Logo.png", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Tracking indicators of the economic and social mobility of the Black community in Hampton Roads"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("The Setting")),
                                          p(a(href = "https://en.wikipedia.org/wiki/Hampton_Roads", "Hampton Roads", target = "_blank"), "area consists of ten cities and six counties in the Southeastern region of Virginia. It is ranked as the 33rd largest MSA in the United States, the 8th largest metro area in the Southeast region, and the 2nd largest between Atlanta and Washington, DC."),
                                          
                                          p("The jurisdictions of Hampton Roads are the cities of Chesapeake, Franklin, Hampton, Newport News, Norfolk, Poquoson, Portsmouth, Suffolk, Virginia Beach, and Williamsburg, and the counties of Gloucester, Isle of Wright, James City, Mathews, Southampton, and York."),  
                                          p("The population of the Hampton Roads MSA has been growing over the last decade with an estimated population of approximately 1.7 million in 2020, a 3% increase from 2010. This region accounts for a large percentage – about 20% - of Virginia’s state population.  "),
                                          p("Each jurisdiction in Hampton Roads has a separate municipal government, unlike some other metro areas. While there are consultations on regional issues, there are more than 20 elected independent municipal governing bodies. As such, it is imperative for our project to examine not only differences with the Virginia population but also within the localities of Hampton Roads."),
                                          p("Given its location, Hampton Roads has one of the world’s greatest natural harbors, with the ability to accommodate the largest cargo ships on the planet.  It is also known for its large military presence, shipping piers, and miles of waterfront property and beaches. Moreover, due to the diversity of the localities, Hampton Roads have a diverse set of natural resources assets ranging from 26 miles of Atlantic Ocean and Chesapeake Bay beaches, numerous state parks, wildlife refuges, Lake Drummond, and various rivers and waterways.")
                                   ),
                                   column(4,
                                          h2(strong("Project Background")),
                                          p("The Black Business Research Analytics Networking and Development (Black BRAND) is a non-profit organization operating in the Hampton Roads region. One goal of Black Brand, which was established in 2016, is to improve the values of the Black families and community within the region."),
                                          p("Despite the overall economic growth in the United States, there is a significant Black-white wealth gap. In 2019, the median white household held $188,200 in wealth",  em("– 7.8 times"), "that of the typical Black household ($24,100). As such, Black BRAND wants to investigate the economic well-being of the Black community in Hampton Roads.  Moreover, given the recent increase in support for black businesses, it is important to determine whether this has resulted in any economic improvement for the Black community."),
                                          p("Guided by our meetings with the Black BRAND stakeholders and Claud Anderson's PowerNomics model, there are five main pillars to measure the overall economic and social progress of the Black community in Hampton Roads."),
                                          img(src = "Flowchart.png", style = "display:inline;margin-left: auto; margin-right: auto;", width="80%"),
                                          p(), 
                                          p("The focus of our project is on the first two pillars: ", strong("Education and Economics."))
                                   ),
                                   column(4,
                                          h2(strong("Project Goals")),
                                          p("Our team aims to create a dashboard that shows the state of the Black community in the Hampton Roads region.  This will enable stakeholders to understand the myriad of past and current factors that affect the economic and social progress of Black households in Hampton. As such, data-driven recommendations can be made to improve the well-being of residents of the region."),
                                          p("We identified, acquired, and used publicly available data to provide our stakeholders, Black BRAND, with a dashboard that shows a combined view of multiple indicators for the two pillars: Education and Economics."), 
                                          p("For the Education pillar, we utilized indicators across three main areas: "),
                                          tags$ul(
                                            tags$li("Educators"),
                                            tags$li("Educational Attainment"),
                                            tags$li("Suspension")
                                          ),
                                          p(),
                                          p("Under the Economics pillar, we collected indicators for four distinct issues:"),
                                          tags$ul(
                                            tags$li("Income/Wealth"),
                                            tags$li("Labor Market"),
                                            tags$li("Homeownership"),
                                            tags$li("Health"), 
                                            tags$li("Household Wellbeing")
                                          ),
                                          p("We conducted a cross-comparison analysis with our indicators across the counties and cities in Hampton Roads.  Our project also compares the Black population against general population in Hampton Roads to determine whether racial differences exist within each locality. Moreover, we also present information for the general population in Virginia."),
                                          p("The dashboard compiles our findings and allows our stakeholders, and other interested users to explore the information dynamically.")
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021'))))
                 ),
                 
                 
                 #Hampton Roads Overview & Demographics--------------------------------------------------
                 navbarMenu(title="Hampton Roads",
                            tabPanel("Hampton Roads Localities", 
                                     fluidRow(
                                       column(3,
                                              p("", style = "padding-top:30px;"),
                                              h4(strong("Counties and Cities of Hampton Roads")),
                                              tags$ul(
                                                tags$li("City of Chesapeake"),
                                                tags$li("City of Franklin"),
                                                tags$li("City of Hampton"),
                                                tags$li("City of Newport News"), 
                                                tags$li("City of Norfolk"),
                                                tags$li("City of Poquoson"),
                                                tags$li("City of Portsmouth"),
                                                tags$li("City of Suffolk"),
                                                tags$li("City of Virginia Beach"),
                                                tags$li("City of Williamsburg"),
                                                tags$li("Gloucester County"),
                                                tags$li("Isle of Wight County"),
                                                tags$li("James City County"),
                                                tags$li("Mathews County"),
                                                tags$li("Southampton County"),
                                                tags$li("York County"),
                                              ),
                                       ),
                                       column(7,
                                              h3(strong("Localities of Hampton Roads"), align = "center"),
                                              withSpinner((plotOutput("hampton_counties_map", width ="100%", height = "600px")))
                                       ),
                                     )),
                            
                            tabPanel("Sociodemographics",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Sociodemographic Characteristics" ), align = "center"),
                                              p("", style = "padding-top:20px;"),
                                              column(4,
                                                     h4(strong("Who Makes Up Hampton Roads?")),
                                                     p("We used the American Community Census data (ACS) to better understand the population in Hampton Roads and Virginia.  The ACS is a yearly survey conducted by the U.S. Census Bureau provides detailed demographic information about American household. We collected the 5-year estimates over the period 2010-2019 to compute the percent of Hampton Roads residents in each locality by race and age.  This information is also presented for the state of Virginia."), 
                                                     p("The black population accounts for about 30% of the total population in Hampton Roads. This is significantly greater than Virginia’s which is about 19%."),
                                                     p("The age composition for the Hampton Roads region appears to be representative of Virginia's population. However, there are variations by localities. For example, there exists a large population of young adults (ages 18 to 29) in Southampton County, whereas Portsmouth has a larger population of seniors - individuals 65 years and older. This data suggests that such ages difference may play a role in the differences in economic or education indicators across localities."), 
                                              ),
                                              column(8, 
                                                     tabsetPanel(
                                                       # Tab race
                                                       tabPanel("Race",
                                                                fluidRow(
                                                                  h1(strong("Racial Demographics"), align = "center"),
                                                                  column(6,
                                                                         h4("Hampton Roads"),
                                                                         
                                                                         selectInput("hampRaceYearDrop", "Select Year:", width = "100%", choices = c(
                                                                           "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                                         )),
                                                                         withSpinner(plotOutput("hamp_pie")),
                                                                         p(tags$small("Data Source: ACS 5 Year Estimate Table B02001")),
                                                                  ),
                                                                  
                                                                  
                                                                  column(6,
                                                                         
                                                                         h4("Virginia"),
                                                                         
                                                                         selectInput("VaRaceYearDrop", "Select Year:", width = "100%", choices = c(
                                                                           "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                                         )),
                                                                         withSpinner(plotOutput("va_pie")),
                                                                         p(tags$small("Data Source: ACS 5 Year Estimate Table B02001"))
                                                                  )
                                                                  
                                                                )
                                                       ),
                                                       #Tab age
                                                       tabPanel("Age",
                                                                fluidRow(
                                                                  h1(strong("Age Composition of Hampton Roads"), align = "center"),
                                                                  column(6,
                                                                         
                                                                         h4("Hampton Roads"),
                                                                         selectInput("HampAgeYearDrop", "Select Year:", width = "100%", choices = c(
                                                                           "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                                         )),
                                                                         
                                                                         withSpinner(plotOutput("hamp_graph")),
                                                                         p(tags$small("Data Source: ACS 5 Year Estimate Table B01001"))
                                                                  ),
                                                                  
                                                                  column(6,
                                                                         
                                                                         h4("Virginia"),
                                                                         p(""),
                                                                         selectInput("VaAgeYearDrop", "Select Year:", width = "100%", choices = c(
                                                                           "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                                         )),
                                                                         
                                                                         withSpinner(plotOutput("va_graph")),
                                                                         p(tags$small("Data Source: ACS 5 Year Estimate Table B01001"))
                                                                  ),
                                                                  
                                                                  column(12,
                                                                         
                                                                         h4("Hampton Roads Age Group by Localities"),
                                                                         selectInput("HampCountAgeYearDrop", "Select Year:", width = "100%", choices = c(
                                                                           "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                                         )),
                                                                         withSpinner(plotOutput("age_map", height="600px")),
                                                                         p(tags$small("Data Source: ACS 5 Year Estimate Table B01001"))
                                                                  )
                                                                  
                                                                )
                                                                
                                                       )
                                                     ) 
                                              )    
                                     )) 
                            
                            
                 ),
                 
                 
                 #Data and Methodology ----------------------------------------------------
                 
                 
                 tabPanel("Data and Methodology", #value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data and Methodology"), align = "center"),
                                   p("", style = "padding-top:10px;"), 
                                   column(4,
                                          img(src = 'acs.png', style = "display: inline; float: left;", width = "200px"),
                                          p("The ",strong("American Community Survey (ACS)")," is an ongoing annual survey conducted by the U.S. Census Bureau. 
                                            The ACS samples households to compile a 1-year and 5-year datasets providing information on households sociodemographic 
                                            and socioeconomic characteristics. We used almost a decade (2010-2019) 5- year estimates to obtain county and city level 
                                            characteristics to explore the population of Hampton Roads."),
                                          br(), 
                                          img(src = 'doe.jpg', style = "display: inline; float: left;", width = "150px"),
                                          p("The ", strong("Virginia Department of Education (VDOE)"), "collects a variety of information from all school divisions in Virginia including the total number of educators and graduation rates. 
                                            This allowed us to report demographic information for educators across the Hampton Roads region along with on-time graduation rate "), 
                                          br(), 
                                          img(src = 'kidscount.png', style = "display: inline; float: left;", width = "150px"),
                                          p(strong("Kids Count"), "is a project of the", strong("Annie E. Casey Foundation"), "produces an annual report that assesses child well-being in the United States. 
                                            We use this data source to present information on short-term suspension by race for the counties and cities in the Hampton Roads region. ")) ,
                                   
                                   
                                   column(4,  
                                          h5(strong("Methodology")),
                                          p("We explore two main pillars to examine the economic and social well-being of the Black community in Hampton Roads."),
                                          p(strong("Education:"), "Educational attainment is a major contributor to improving an individual’s economic well-being and standard of living. As such, we examine the disparities in educational attainment to determine the economic progress of the Black community.  
                                          Specifically, we collected data on on-time high school graduation rates and bachelor’s degree attainment. We also include the racial/ethnic representation of educators. Research indicates some distinct, albeit small, advantages when black students are paired with a Black teacher [1]. Suspension rate was also included in our analysis as some students are more likely to be disciplined than others even for the same transgression (KIDS Count). 
                                          Moreover, a higher suspension rate will limit educational attainment, given the negative effect on achievement [2].
                                            "),
                                          img(src = 'education.png',align="center", height = "300px", width="400px"),
                                          p(tags$small("[1] Anna J. Egalitea, A.J. , Kisida B., & Winters, M.A. (2015), Economics of Education Review, 45, 44-52.")),
                                          p(tags$small("[2] Noltemeyer, A. L., Ward, R. M., & Mcloughlin, C. (2015). Relationship between school suspension and student outcomes: A meta-analysis. School Psychology Review, 44(2), 224-240.")),
                                   ),
                                   
                                   
                                   column(4,  
                                          p(),
                                          p(),
                                          p(),
                                          p(strong("Economics:"), "A major driver of economic growth and mobility is growth in labor, income, health, and wealth accumulation (homeownership). Thus, we examine median income, poverty rates, industry employment and unemployment rates, health insurance status, and homeownership rate for the Black and general population across the counties and cities in Hampton Roads [1]."),
                                          p("To further assess economic progress, we included several characteristics to measure Black household’s well-being.  We included welfare measures (Food Stamps or SNAP recipients); family stability (female-headed household, marital rates, grandparents’ guardianship, and migration); transportation access (private and public transportation); and technology access (computer and internet access)."),
                                          p(),
                                          img(src = 'economic.png', align="center", height = "300px", width="400px"),
                                          p(),
                                          p(),
                                          p("Lastly, we rank counties/cities based on several of our indicators. The idea is to determine whether the Black community was consistently underperforming in some localities in the Hampton Road region."),
                                          p(),
                                          p(tags$small("[1] Butler, S. M., Beach, W. W., & Winfree, P. L. (2008). Pathways to economic mobility: Key indicators. Economic mobility project.")),
                                          
                                   )
                                   
                          )
                          
                 ),
                 
                 
                 # Education Indicators ----------------------------------------------------
                 
                 
                 navbarMenu(title="Education",
                            #Educator Demographics
                            tabPanel("Educators",
                                     fluidRow(
                                       p("", style = "padding-top:20px;"),
                                       column(4,
                                              h4(strong("Who are the Educators in Hampton Roads?")),
                                              p("", style = "padding-top:10px;"),  
                                              p("The Black population overwhelmingly attends public schools, yet in most parts of the U.S., teachers are overwhelmingly White. The National Center for Education 
						Statistics reports that about 79 percent of all U.S. public school teachers were non-Hispanic White [1]. This trend is also reflected in the State of Virginia.   
            For the 2020-2021 academic year, the Virginia Department of Education released racial and ethnic breakdown for educators-administrators, teachers, and other pupil personnel- by school division.
						These data enable examination of racial and ethnic representation to be matched within local school divisions. 
           In 2020/21, 13 percent of public-school educators identified as Black, compared to a state rate of 19.9% Black population in 2019, a difference of almost seven percentage points [2]. "), 
                                              p("In 2019, 31% percent of the Hampton Roads population identified as Black.  Seven of the 16 localities (44%) reported rates of public school Black educator representation at or above the regional average, while another 7 reported rates less than 12%.  
						Black educator representation ranged from high rates of 62.3% in Portsmouth and 55.8% in Franklin City to very low rates of 3.5% in Poquoson.  These percentages are reversed for White educators, with highs of 92.6% representation in Poquoson and lows of 34.6% and 40% respectively in Portsmouth and Franklin City."),   
                                              p("Hampton Roads localities vary considerably in their racial and ethnic profiles, making it difficult to ascertain from these data alone how well educator representation matches specific communities. A future project might be to map local demographic profiles to school division educator representation to determine 
						how well Black educators are matched to local Black population representation."),
                                              p("", style = "padding-top:20px;"),
                                              p(tags$small("[1] Taie,S. and Golding, R. (2020). Characterisitcs of Public and Private Elementary and Secondary School Teachers
						in the United States: Results from the 2017-18 National Teacher and Principal Survery. First Look. NCES 2020-142. National Center for Education Statistics")),
                                              p(tags$small("[2].  Virginia Department of Education, Virginia Educator Ethnicity and Race Data, https://doe.virginia.gov/teaching/workforce_data/index.shtml; U.S Census QuickFacts, https://www.census.gov/quickfacts/fact/table/VA,US/PST045219.")),
                                       ),
                                       column(8,
                                              h1(strong("Educator by Racial and Ethnic Groups"), align = "center"),
                                              p("", style = "padding-top:20px;"),
                                              selectInput("teacherRaceBreakdown", "Select Race:", width = "100%", choices = c(
                                                "Black", "White", "American Indian", "Asian", "Hawaiian", "Two or More Races", "Hispanic"
                                              )),
                                              withSpinner(plotlyOutput("teacherRacePlots")),
                                              p(tags$small("*Note: Missing bar reflects 0 teachers for that locality's racial/ethnic group. Williamsburg and James City educator data were combined from the source dataset. ")),
                                              p(tags$small("Source: Virginia DOE, Virginia 2020-2021 Teacher Race Report"))
                                       ))
                                     
                            ),
                            tabPanel("Educational Attainment",
                                     fluidRow(
                                       p("", style = "padding-top:40px;"),
                                       column(4,
                                              h4(strong("Graduating High School On-Time")),
                                              p("Virginia Department of Education defines on-time graduation as the percentage of students who earned a Board of Education-approved diploma within four years of entering high school for the first time. On-time graduation rates have been steadily climbing in Virginia from 2009 thru the 2020 school year moving from 85.5% in 2010 to 92.5% in 2020. Black students have made even greater gains across this period, from 80.4% in 2011 to 91.4% graduating within four years in 2020."),
                                              p("Across Hampton Roads, the same upward state trend is evident across the decade. The regional average on-time graduation rate increased steadily from 82% in 2010 to 92.6% in 2020. Black students mirrored the trend, from a regional average of 79.7% in 2010 to 92.4% in 2020, effectively matching the total regional and state population average."),
                                              p("Due to the large variation between localities, we examine local differences.  For Black students, gains ranged from 7.8 to approximately 24 percentage points, with an average improvement of 17 percentage points.  Portsmouth and Southampton made the most dramatic gains, both showing upward trends from lows in 2010 of 66.1% and 72.9% respectively, to highs of 89.6% and 95% in 2020, a similar pattern to Hampton, which also showed very impressive steady gains (from 78% in 2010 to 96% in 2020)."),
                                              p("While every locality showed improvement over time, two locality rates remained notably lower to the state average. Norfolk and Suffolk remained more than 6.8 and 7.2 percentage points lower than the state average in 2020. Future research could help better understand whether on-time graduation rates reflect students leaving school, obtaining alternate certification, or moving out of state without transfer notice (Suffolk in particular borders North Carolina)."),
                                              
                                              h4(strong("College Degree")),
                                              p("A college degree is not only a 'requirement for most jobs', but leads to higher wages, financial security, and improvement in standard of living. In the U.S., there is an increasing trend of college attainment over the last decade. The ACS reports a similar trend for Virginia residents. 
                                                       In 2010, 33.8% of Virginia residents aged 25 years or older had earned a bachelor's degree or higher. This rate improved to 38.8% by 2019."),
                                              p("There are substantial differences in attainment rate for Black residents across the localities in Hampton Roads.  In 2019, four-year college degree rates ranged from a low of 9.8% (Poquoson) to a high of 34.8% (York).  
                                                       Additionally, the differential rates between the general population and Black residents vary widely across localities by year and across years. 
                                                       Some of the largest gaps are found in 2019 in Williamsburg (39.6 percentage point difference) and Poquoson (33 percentage points). The shortest gap at the end of the decade was in Hampton at 1.2 percentage points.")                                                 
                                       ),
                                       column(8,
                                              h1(strong("Educational Attainment"), align = "center"),
                                              tabsetPanel(
                                                #On-time Graduation Rate
                                                tabPanel("High School Graduation",
                                                         
                                                         column(12,
                                                                fluidRow(
                                                                  p("", style = "padding-top:10px;"),
                                                                  h4(strong("High School On-Time Graduation Rates in Hampton Roads"), align = "center"),
                                                                  withSpinner(leafletOutput("dropout_map", width = "850px")),
                                                                  p(tags$small("Data Source: Virginia Department of Education")),
                                                                  
                                                                ))
                                                ),
                                                
                                                tabPanel("Bachelor's Degree", p(""),
                                                         fluidRow(
                                                           p("", style = "padding-top:40x;"),
                                                           h4(strong("Percentage of Hampton Roads Population 25 years and older with Bachelor's degree or higher"), align = "center"),
                                                           selectInput("genEdAttainmentYearDrop", "Select Year:", width = "100%", choices = c(
                                                             "2019", "2018", "2017", "2016", "2015", "2014", "2013",
                                                             "2012", "2011", "2010"
                                                           )
                                                           ),
                                                           withSpinner(plotlyOutput("genEdAttainmentPlots")),
                                                           p(tags$small("*Note: Data missing for some years. ")),
                                                           p(tags$small("Data Source: ACS 5 Year Estimate Tables C15002B (For Black Population), S1501 (For Total Population)"))
                                                           
                                                         ))
                                                
                                              )
                                       )
                                     )
                            ),
                            
                            tabPanel("Suspension",
                                     
                                     fluidRow(
                                       p("", style = "padding-top:60px;"),
                                       column(4, h4(strong("Who is Being Punished?")), 
                                              p("The Virginia Department of Education refers to students being barred from attending school for at most 10 days.  We collected short-term suspension rate by race from Kids COUNT, Annie E. Casey which is defined as the total number of offenders of a certain race divided by the total number of students of that race for each locality. Rates include students from grades K - 12."), 
                                              p("Across Virginia, from academic years 2014-15 to 2018-19, the Black student suspension rate was consistently more than eight percentage points higher than White and Hispanic student rates. Black students were suspended between a relative low of 12.0 % in 2018-19 and a high of 13.6 % in 2016-17. In contrast, the highest suspension rate for White and Hispanic students was 3.8 % and 3.9 % respectively, both in 2016-17."), 
                                              p("Black student suspension rates in Hampton Roads were decidedly higher than for White students across all years examined. This is consistent even for areas with low suspension rates - averaging rate across years, the lowest rate for Black students was 4.9% for Williamsburg-James City County which is in contrast to White students which was 2.5%. On average, the White student suspension rate was 4.8%, almost a 9-point difference."), 
                                              p("To illustrate gaps between Black and White student suspension rates over time, we plot the percentage point difference between Black and White rates for each Hampton Roads locality that reported more than two years of data for students of both races.  Most localities showed a Black-White student suspension gap rate of between 5 and 14 percentage points over this time period. "), 
                                              p("Norfolk consistently showed the greatest racial suspension gap between students, ranging from a relative low of 12.8% in 2016-17 to a high gap of 14.1% in 2018-19.  York showed the smallest gap for three of the five years, followed by Gloucester for two years.")), 
                                       column(8,
                                              h1(strong("Short Term Suspension"), align = "center"),
                                              tabsetPanel(
                                                tabPanel("Suspension Rate", p(""),
                                                         p("", style = "padding-top:10px;"),
                                                         h4(strong("Percent of Students Suspended Short-Term in Virginia"), align = "center"),
                                                         withSpinner(plotlyOutput("suspension_line_graph")),
                                                         p(tags$small("Data Source: KIDS COUNT, Annie E. Casey Foundation"))
                                                ),
                                                tabPanel("Suspension Rate by Race", p(""),
                                                         p("", style = "padding-top:10px;"),
                                                         h4(strong("Percent of Students Suspended Short-Term in Hampton Roads by Race"), align = "center"),
                                                         selectInput("BWsuspensionYearDrop", "Select Year:", width = "100%", choices = c(
                                                           "2019", "2018", "2017", "2016", "2015")),
                                                         withSpinner(plotlyOutput("BW_map", height = "700px")),
                                                         p(tags$small("Data Source: KIDS COUNT, Annie E. Casey Foundation")),
                                                         p(tags$small("*Note: Black student data supressed for Mathews and Poquoson. Missing data for some years for Franklin for White student rates"))
                                                ),
                                                tabPanel("Suspension Gap", p(""),
                                                         p("", style = "padding-top:10px;"),
                                                         h4(strong("Difference Between Black and White Student Short-Term Suspension"), align = "center"),
                                                         withSpinner(plotlyOutput("suspensionGap", height = "700px")),
                                                         p(tags$small("Data Source: KIDS COUNT, Annie E. Casey Foundation"))
                                                )
                                                
                                              )
                                              
                                       )
                                       
                                     )
                                     
                                     
                            )
                 ),
                 
                 
                 navbarMenu(title="Economics",
                            #Median Income
                            tabPanel("Income",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:30px;"),
                                              #change this later to formatting that is desired
                                              column(4, h4(strong("Household's Economic Status in Hampton Roads")), 
                                                     p("In 2019, the Black community had a median annual income of $52,596, which is less than the median income of $67,387 of Hampton Road’s general population. This gap is consistent with the state trend, although it is larger at the state level, where the Black and general households earn $51,654 and $74,222, respectively."), 
                                                     p("While there are racial differences in median income, the gap between the Black community and the general population has been closing over the last decade. In 2010, the racial median income gap in Hampton Roads was $17,121, whereas it was $14,791 in 2019. This suggests that despite the lower median income, the Black community's economic status has slightly improved over the last decade.")),
                                              h1(strong("Median Income in Virginia and Hampton Roads"), align = "center"),
                                              column(8,
                                                     withSpinner(plotOutput("medianTimeGraph")),
                                                     p(tags$small("Data Source: ACS 5 Year Estimates Table S1903")),
                                                     
                                                     selectInput("MedianIncomeYearDrop", "Select Year:", width = "100%", choices = c(
                                                       "2019","2018", "2017", "2016", "2015","2014",
                                                       "2013","2012", "2011", "2010")),
                                                     withSpinner(plotOutput("income_plot")),
                                                     p(tags$small("Data Source: ACS 5 Year Estimates Table S1903"))
                                              )
                                     )),
                            
                            #),
                            #Homeownership Map
                            tabPanel("Homeownership", 
                                     fluidRow(
                                       p("", style = "padding-top:20px;"),
                                       column(4, h4(strong("Home Owners")),
                                              p("We report homeownership rates in Hampton Roads. The interactive map includes both the percentage of homeowners defined as total homeowners/ total population and the percentage of black homeowners calculated as the total black homeowners/total Black population. 
                                         The radio buttons at the top right of the map allow the user to view the percentage of Black homeowners across the localities in Hampton Roads."),
                                              p("Homeownership improves individuals' long-term wealth. It also improves civic and political engagement, children's educational outcomes, and health behaviors. 
                                          As such, we examine the rate of homeownership for the Black community in Hampton Roads."),
                                              p("Regardless of locality, the Black population has a lower rate of homeownership than the general population. The disparity
                                         is persistent for most counties/cities. In James City, there is a 23 percentage points difference between the general population and 
                                         the Black population, with a 76% and 53.1% homeownership rate, respectively. For the other localities, differences range from 5 to 28 percentage points.
                                         Poquoson was the only locality where the Black population had a greater homeownership rate than the general population.")
                                       ),
                                       column(8,
                                              h1(strong("Home Ownership in Hampton Roads"), align="center"),
                                              withSpinner(leafletOutput("homeownership_map")),
                                              p(tags$small("Data Source: ACS 5 Year Estimates Table S2505"))
                                       ),
                                     )),
                            
                            
                            #Unemployment Rate
                            tabPanel("Labor Market", 
                                     fluidRow(
                                       p("", style = "padding-top:40px;"),
                                       column(4, h4(strong("Labor Sector & Unemployment")),
                                              p("We examine the economic health of the labor market for the Black communities in Hampton Roads. First, we look at the top two industries that explain the highest number of workers in the region. This allows us to understand the employment landscape of the region, which may provide insights into vulnerable areas.  
                                         We then analyze unemployment to understand the economic hardship that households in the Hampton Region face."),
                                              p("", style = "padding-top:10px;"),
                                              h5(strong ("Industries that employ the largest number of workers")),
                                              p("Majority of the residents in the Hampton region were employed in the Educational Services, health care, and social services sector. Among each locality in Hampton Roads, educational services, health care, and social services was one of the top two industries that employ the highest number of workers. 
                                         However, for the other top employable industry, there are some slight variations across localities and time. 
                                         For example, in 2012, eight of the 16 localities top two sector was in retail trade. However, only two areas had retail trade in the top two industries indicating more economic diversity among the localities over time."),
                                              h5(strong ("Are there differences in the unemployment rate across localities?")),
                                              p("The unemployment rate for the Black population in Hampton Roads tends to be greater than the average rate in Virginia, regardless of the time period. For example, in 2019, Franklin-which had the highest unemployment 
                                         rate in the region- the unemployment rate for the black community was 14.9 % compared with Virginia's average of 4.6%.  Moreover, regardless of localities, the Black community is more likely to be unemployed than the other residents.  In 2019, the Black population had a higher unemployment rate than their counterparts in every county and city in the Hampton Roads region, even in areas that were below the state's average (Gloucester)."),
                                              p("While for some counties there has been a decline in unemployment rates across the decade (2010-2019) for Black households, there still exists some significant disparity for most counties/cities. One such example is Williamsburg County that in 2010 had a similar unemployment rate for both Blacks and the region residents, 6.0% and 6.1%, respectively. But by 2019, that gap widen significantly by 4.6 percentage points (the Black unemployment rate was 10.4% and the total population 5.8%)"),
                                              
                                              
                                       ),
                                       column(8,
                                              h1(strong("Labor Market Characteristics"), align = "center"),
                                              tabsetPanel(
                                                #Sector Employment
                                                tabPanel("Industry Employment", 
                                                         p("", style = "padding-top:10px;"),
                                                         h4(strong("Top Two Industry Sectors"), align = "center"),
                                                         fluidRow(p(""),
                                                                  selectInput("SectorEmploymentYearDrop", "Select Year:", width = "100%", choices = c(
                                                                    "2019","2018", "2017", "2016", "2015","2014",
                                                                    "2013","2012", "2011", "2010")),
                                                                  withSpinner(plotlyOutput("sector_plot")),
                                                                  p(tags$small("*Note: Data missing for some years. ")),
                                                                  p(tags$small("Source: ACS 5 Year Estimate Table DP03"))
                                                                  
                                                         )
                                                ),
                                                
                                                tabPanel("Unemployment Rate",
                                                         p("", style = "padding-top:10px;"),
                                                         h4(strong("Unemployment Rate in Hampton Roads"), align = "center"),
                                                         withSpinner(plotlyOutput("unemployment_plot")),
                                                         p(tags$small("*Note: Red dotted line represents Virgina's unemployment rate. Missing data for the Black population in Mathews and Poquoson")),
                                                         p(tags$small("Data Source: ACS 5 Year Estimates Table S2301")),
                                                         sliderInput("UnemploymentRateSlider", "Select Year", value = 2019, min = 2010, max = 2019, sep = "", width = "100%",
                                                                     animate=animationOptions(interval = 1400)),
                                                ),
                                                tabPanel("Unemployment Rate Trends",
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(width=12, height=550,
                                                                  img(src="updated_unemployment_plot.gif", height = "800", width="1100")
                                                                  
                                                         )
                                                ))))
                            ),  
                            
                            
                            #Poverty Rate
                            tabPanel("Poverty",
                                     fluidRow(
                                       p("", style = "padding-top:50px;"),
                                       column(4, h4(strong ("How does the poverty rate in Hampton Roads compare to all of Virginia?")), 
                                              p("", style = "padding-top:10px;"),
                                              p("It is clear that regardless of location the Black population has borne a higher rate of poverty than other races by about 7-8 percentage points between 2012-2019. Still, Hampton Roads has endured somewhat higher rates over the years. In 2019, 11.3% of people in Hampton Roads were in poverty, 0.7 percentage points higher than in Virginia overall. And among the Black population, the gap (1.2 percentage points) was even wider. In 2012, the gaps were smaller, but since then has gotten wider, indicating a deterioration in economic circumstances, especially for the Black population."),
                                              p("", style = "padding-top:10px;"),
                                              h5(strong ("Is poverty more prevalent in some counties or cities in Hampton Roads than others?")),
                                              p("It appears that, even at the county or city level, the Black population is always more likely to be in poverty than other residents. The disparity has been pronounced and persistent for most counties/cities. Most notably, Mathews has experienced a poverty gap that has grown from about 6 percentage points in 2012 to 30 percentage points in 2019. On the other hand, some counties/cities have enjoyed a significant decline in poverty rates and gaps over time, notably Franklin City. The city had a poverty rate of roughly 36% among Blacks compared to 23% in its entire population in 2012. But by 2019, the rate decreased markedly for both groups to 22% and 15% respectively, cutting the poverty gap by half over the 8-year period."),
                                              
                                       ),
                                       column(8,
                                              h1(strong("Poverty Rates in Hampton Roads"), align = "center"),
                                              tabsetPanel(
                                                tabPanel("Poverty Rates",
                                                         p("", style = "padding-top:10px;"),
                                                         h4(strong("Poverty Rates across Hampton Roads and Virginia"), align = "center"),
                                                         selectInput("PovertyYearDrop", "Select Year:", width = "100%", choices = c(
                                                           "2019","2018", "2017", "2016", "2015","2014",
                                                           "2013","2012")),
                                                         withSpinner(plotOutput("pov_plot")),
                                                         p(tags$small("Data Source: ACS 5 Year Estimates Table S1701"))
                                                ),
                                                tabPanel("Poverty Rates across Localities",
                                                         p("", style = "padding-top:10px;"),
                                                         h4(strong("Poverty Rates across Hampton Roads' Cities and Counties"), align = "center"),
                                                         selectInput("PovertyCountYearDrop", "Select Year:", width = "100%", choices = c(
                                                           "2019","2018", "2017", "2016", "2015","2014",
                                                           "2013","2012")),
                                                         withSpinner(plotlyOutput("counties_pov")),
                                                         p(tags$small("Data Source: ACS 5 Year Estimates Table S1701"))
                                                ), 
                                                
                                                tabPanel("Poverty Trends", 
                                                         p("", style = "padding-top:10px;"),
                                                         fluidRow(width=12, height=550,
                                                                  img(src="poverty.gif", height = "800", width="1200")
                                                                  
                                                         )
                                                )
                                                
                                                
                                              )
                                              
                                       )       
                                     )    
                                     
                            ), 
                            
                            #Uninsured Rates
                            tabPanel("Health", 
                                     fluidRow(
                                       p("", style = "padding-top:50px;"),
                                       column(4, h4(strong("Insurance Status In Hampton Roads")),
                                              p("Health insurance is critical for preventative and emergency care and is considered a key component of financial security.  Since the Patient Protection and Affordable Care Act (ACA) passed in 2010, uninsured rates have fallen across the country. Similarly, the percentage rate for Virginias dropped from 12.3% in 2010 to 8.6% in 2019. 
                                              The rates for Black Virginians were higher - in 2012, 15.4% were uninsured, whereas, in 2019, it was 10.1%. "),
                                              p("The Black population of Hampton Roads showed similar trajectories to the national and state trends, with most localities showing a fairly linear falling trend.  The most dramatic change occurred in the later years, particularly in 2018 and 2019, when several localities almost or more than halved their Black uninsured rate.  
                                              Most dramatically, in Franklin City, more than a quarter of the Black population (25.8%) was uninsured in 2012. By 2019, 7.3% were uninsured, a difference of 18.5 percentage points. "),
                                              p("While almost all localities showed declines, many rates remained higher than the state average. 
                                              Norfolk consistently remained above the state average (20.7% uninsured in 2012 to 13.8% in 2019) despite a decreasing trend. On the other hand, Chesapeake and Virginia Beach consistently had lower rates than state averages. York had by far the lowest rates over time for an area with proportional Black representation."),
                                              p("Several exceptions to the general pattern also emerged. Mathews showed an increasing trend of Black residents uninsured, ranging from a relative low of 18.7% in 2013 to the highest level in 2019 at 32.4%. In stark contrast, all Black residents of Poquoson were consistently insured across the decade. Such differences may be due to the areas' small Black population. "),
                                              p("Several important policy changes, as well as economic improvement following the Great Recession, may explain some of the patterns.  Full implementation of the ACA started in 2014, and in 2018 Virginia expanded Medicaid under the ACA.  Thus, there may be further improvements in future data.")
                                              
                                       ),
                                       column(8,
                                              fluidPage(
                                                h1(strong("Health Uninsured Rates"), align = "center"),
                                                withSpinner(plotlyOutput("uninsured_plot")),
                                                sliderInput("UninsuredPctSlider", "Select Year", value = 2019, min = 2012, max = 2019, sep = "", width = "100%",
                                                            animate=animationOptions(interval = 1400)),
                                                p(tags$small("*Note: Data missing for Black population in Poquoson.")),
                                                p(tags$small("Data Source: ACS 5 Year Estimates Table S2701")))
                                       )
                                       
                                     )
                                     
                            ),
                            
                            
                            #Veteran Status
                            
                            tabPanel("Veterans",
                                     fluidRow(
                                       p("", style = "padding-top:50px;"),
                                       column(4, h4(strong("Who has served in Hampton Roads?")),
                                              p("Hampton Roads is rich in military bases and establishments. All branches of the U.S. military have bases or substantial presence, with Naval Station Norfolk being the largest naval station in the United States, if not the world [1].  Starting in the 1960s, Black Americans have been overrepresented in the United States Armed Services relative to their proportion of the population [2]. 
                                       Among other benefits, many view the military as an economically cost-effective means to gain training and educational advancement.   "),
                                              p("We report rates of Black veteran status as a percentage of the total Black population by locality. Percentages were calculated by dividing the Black veteran population by the total Black population for each locality.  
                                       The radio buttons at the top right of the map allow the user to view military bases across Hampton Roads. "),
                                              p("We report rates of Black veteran status as a percentage of the total Black population by locality. Percentages were calculated by dividing the Black veteran population by the total Black population for each locality.  The radio buttons at the top right of the map allow the user to view military bases across Hampton Roads."),
                                              p("", style = "padding-top:40px;"),
                                              p(tags$small("[1]  Hampton Roads Chamber of Commerce, 2021. Retrieved from:", a(href = "https://www.hrchamber.com/page/our-military/", "https://www.hrchamber.com/page/our-military/", target = "_blank"))),
                                              p(tags$small("[2]. Ferguson, P. (2021).  U.S. Army. Retrieved from:", a(href = "https://www.army.mil/article/243604/african_american_service_and_racial_integration_in_the_u_s_military", "army.mil/article/243604/african_american_service_and_racial_integration_in_the_u_s_military", target = "_blank"))
                                              ),
                                              
                                       ),
                                       column(8,
                                              fluidPage(
                                                h1(strong("Veteran Status in Hampton Roads"), align = "center"),
                                                withSpinner(leafletOutput("veteran_map")),
                                                p(tags$small("Data Source: ACS 5 Year Estimates Table S2101")),
                                                sliderInput("VeteranSlider", "Select Year:", value = 2019, min = 2010, max = 2019, sep = "",  width = "100%",
                                                            animate=animationOptions(interval = 1200))),
                                       )
                                     )
                            ),
                            
                            tabPanel("Household Wellbeing",
                                     fluidRow(
                                       p("", style = "padding-top:20px;"),
                                       column(4, 
                                              h4(strong("Household Well-being Characteristics")),
                                              p("", style = "padding-top:10px;"),
                                              p("We retrived several indicators using the 2019 5-year ACS estimates to examine the well-being of Black households in Hampton Roads. These characteristics
                                               are presented using interactive maps. Clicking on the tabs and selecting a variable populates the map. Hovering over the map displays the percentage of Black residents living
                                                in a specific city/county with the selected characteristics. "),
                                              p("", style = "padding-top:20px;"),
                                              tags$ul(
                                                tags$li(("The interactive map shows:")),
                                                p("", style = "padding-top:20px;"),
                                                withSpinner(textOutput("description_text")))
                                       ),
                                       column(8,
                                              fluidPage(
                                                h1(strong("Household Characteristics"), align = "center"),
                                                selectInput("select_wellbeing", "Select Indicator:", width = "100%", choices = c("Percent of Black Households Receiving Foodstamps/SNAP Benefits", "Percent of Black Children under 18 in Female Head of Household", "Percent of Married Black Population 15 years and over", "Percent of Black Grandparents who are Guardians",
                                                                                                                                 "Percent of Black County Migration","Percent of Black Population that uses car/truck/van to get to work", "Percent of Black Population that uses public transportation to get to work",
                                                                                                                                 "Percent of Black Households with a computer with broadband internet", "Percent of Black Households without a computer")),
                                                withSpinner(leafletOutput("wellbeing_maps")),
                                                p(tags$small("Data Source: ACS 5 Year Estimates Tables: S0901, S2201, S0701, S1002, S1201, S0802, S2802")),
                                                
                                              )
                                       )
                                     )
                            )
                            
                 ),
                 
                 
                 tabPanel("Future Steps",
                          fluidRow(
                            p("", style = "padding-top:20px;"),
                            column(4, 
                                   p("", style = "padding-top:10px;"),
                                   h4(strong("Future Steps")),
                                   p("", style = "padding-top:5px;"),
                                   p("We provided graphs that rank selected education and economic indicators for each locality in Hampton Roads. We use the 2019 ACS 5-year estimates data to create these rankings. Users can select different indicators that present a bar graph that ranks each locality according to the specific indicator.  
                                     We provided this information to determine whether there were some localities in the Hampton Roads region that the Black community was consistently underperforming.  Caution should be noted for the rankings for Poquoson and Mathews County due to the small area size and Black population for both localities. 
                                     This has resulted in some outliers and unavailability of data for these two areas for majority of the indicators."), 
                                   p("The rankings show that there are considerable variations across each county and city in the Hampton region. For instance, Black households seem to be less vulnerable in areas in technology access, welfare assistance, poverty, access to health insurance, and suspension rates in York County. However, they are extremely vulnerable in educational attainment and income/wealth accumulation (that is median income and homeownership).  
                                     This trend is similar for all localities. In Chesapeake, there seems to be less vulnerability in areas of health insurance, transportation, technology access, unemployment, and poverty rate. On the other hand, there is a high vulnerability for income/wealth accumulation, welfare assistance, and educational attainment. Given that each jurisdiction in Hampton Roads has a separate municipality, it may be more insightful to analyze the rankings of the indicators within localities rather than making comparisons across localities to inform policymaking.  "),
                                   p("The indicators that we have presented in our dashboard, along with the rankings, provide evidence that there are some areas that the Black community needs assistance with to improve economic and social well-being. Data-driven assistance programs in employment, obtaining health insurance, education, homeownership, and improving household stability can foster economic mobility in the Hampton Regions."),
                                   p("For this project, we have provided a dashboard that shows a combined view of multiple indicators in two major areas: Education and Economics that collectively addresses the challenges facing the Black communities in the Hampton Region. However, given the substantial variations across localities, a future project would be to consider using the indicators provided to develop an economic and social vulnerability 
                                     index specific for the Black community in the Hampton region. This future project could also include the other three pillars: Media, Art, and Entertainment, People and Values, and Politics and Justice to provide a robust and comprehensive index and dashboard for Black BRAND.")
                                   
                            ),
                            column(8,
                                   fluidPage(
                                     h1(strong("Hampton Roads Rankings"), align = "center"),
                                     selectInput("select_rank", "Select Indicator:", width = "100%", choices = c("Percentage of Black Students 25 yrs and over that have Bachelor's Degree or Higher",
                                                                                                                 "Percentage of Black Students Suspended","Black Median Income", "Percentage of Black HomeOwners", 
                                                                                                                 "Black Unemployment rate","Black Poverty rate", "Percent of Uninsured (Health) Black population", 
                                                                                                                 "Percentage of Black Children under 18 in Female Headed Household", 
                                                                                                                 "Percentage of Black Population that uses car/truck/van for work","Percent of Black Households without a computer")),
                                     withSpinner(plotlyOutput("ranked_chart")),
                                     p(tags$small("*Note: Data missing for some localities ")),
                                     p(tags$small("Data Source: 2019 ACS 5 Year Estimates Tables: C15002B, KIDS Count, S1903, S2505, S2301, S1701, S2701, S0901, S0802, S2802")),
                                     
                                   )
                            ))),
                 
                 tabPanel("Hampton Team", value = "team",
                          fluidRow(column(3),
                                   column(6,
                                          h4(strong("Virginia Tech Data Science for the Public Good") , align = "center"),
                                          p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"), 
                                            "is a summer immersive program held at the", a(href = 'https://aaec.vt.edu/s', 'Virginia Tech Department of Agricultural and Applied Economics.'), 
                                            "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply, and our annual symposium, please visit", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'the official VT DSPG website.', target = "_blank")),
                                          #p("", style = "padding-top:10px;")
                                   )
                                   
                          ),
                          fluidRow(
                            column(2),
                            column(2, align = "center",
                                   h4(strong("Graduate Fellow")), tags$br(),
                                   tags$br(), img(src = "fellow-seth.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width="45%", height="45%"),
                                   tags$br(), p(a(href = 'https://www.linkedin.com/in/aviseth/', 'Avi Seth', target = '_blank'), "(Virginia Tech, Computer Science)")
                            ),
                            column(2, align = "center",
                                   h4(strong("Undergraduate Interns")), tags$br(),
                                   img(src = "BurkholderHeadshot.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                   p(a(href = 'https://www.linkedin.com/in/matthew-burkholder-297b9119a/', 'Matthew Burkholder', target = '_blank'), "(Virginia Tech, Philosophy, Politics, & Economics)"), tags$br(),
                                   img(src = "Mukora.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"), tags$br(),
                                   p(a(href = "https://www.linkedin.com/in/victormukora/", 'Victor Mukora', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics)"), tags$br(),
                                   img(src = "Christina_Prisbe_Headshot.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"), tags$br(),
                                   p(a(href = "https://www.linkedin.com/in/christina-prisbe-60966b218/?midToken=AQGZJw6kSGscnQ&midSig=0iRgRUj5PNWpQ1&trk=eml-email_m2m_invite_single_01-hero-4-prof%7Ecta&trkEmail=eml-email_m2m_invite_single_01-hero-4-prof%7Ecta-null-ec0b8k%7Ekrp4eqqv%7E3v-null-neptune%2Fprofile%7Evanity%2Eview", 'Christina Prisbe', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics)"), tags$br(),
                                   img(src = "kwabe.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"), tags$br(),
                                   p(a(href = "https://www.linkedin.com/in/kb1999/", 'Kwabe Boateng', target = '_blank'), "(Virginia State University, College of Engineering and Technology)"),
                                   
                            ),
                            column(2, align = "center",
                                   h4(strong("Faculty Advisors")), tags$br(),
                                   img(src = "Dr_Holmes.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", height = "150px", width = "150px"),  tags$br(),
                                   p(a(href = 'https://aaec.vt.edu/people/faculty/holmes-chanita.html', 'Dr. Chanita Holmes', target = '_blank'), "(Virginia Tech, Department of Agricultural and Applied Economics)"),  tags$br(),
                                   img(src = "Dr_Bradburn.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", height = "150px", width = "150px"),  tags$br(),
                                   p(a(href = 'https://liberalarts.vt.edu/departments-and-schools/department-of-human-development-and-family-science/faculty/isabel-bradburn.html', 'Dr. Isabel Bradburn', target = '_blank'), "(Virginia Tech, Department of Human Development and Family Science)"),  tags$br()
                            ),
                            
                            column(2, align = "center",
                                   h4(strong("Stakeholders")), tags$br(),
                                   img(src = "MalloryTuttle.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                   p(a(href = 'https://www.linkedin.com/in/mallory-taylor-tuttle/', 'Mallory Tuttle', target = '_blank'), "(Associate Director Virginia Tech Hampton Roads Centers)"), tags$br()
                            )
                            
                          )
                 ),
                 inverse = T)

# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  #hampton roads map of counties -------------------------------------------
  output$hampton_counties_map <- renderPlot({
    coord_data <- read_rds("data/age/coordinates.rds")
    coord_data <- st_transform(coord_data)
    coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
    coordinates2 <- coordinates1[,6]
    city <- c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews", 
              "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
              "Williamsburg", "York")
    coordinates2 <- mutate(coordinates2, Loc = city)
    coordinates2$Loc[coordinates2$Loc == "Franklin"] <- "Franklin City"
    #Graph
    hampton_counties_map <- ggplot(coordinates2) +
      geom_sf() +
      geom_sf_label(aes(label=Loc,geometry = geometry), label.padding = unit(.5, "mm"), size =4) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=13),
            panel.background = element_blank()) 
    #plot
    hampton_counties_map
    
  })
  
  # hampton race plots -----------------------------------------------------
  var_hampRace <- reactive({
    input$hampRaceYearDrop
  })
  
  output$hamp_pie <- renderPlot({
    #pal
    vir_pal <- c("#33638DFF", "#1F968BFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")
    
    if(var_hampRace() == 2019){
      hamp_data <- read.csv("data/race/hamp_race2019.csv")
    }
    if(var_hampRace() == 2018){
      hamp_data <- read.csv("data/race/hamp_race2018.csv")
    }
    if(var_hampRace() == 2017){
      hamp_data <- read.csv("data/race/hamp_race2017.csv")
    }
    if(var_hampRace() == 2016){
      hamp_data <- read.csv("data/race/hamp_race2016.csv")
    }
    if(var_hampRace() == 2015){
      hamp_data <- read.csv("data/race/hamp_race2015.csv")
    }
    if(var_hampRace() == 2014){
      hamp_data <- read.csv("data/race/hamp_race2014.csv")
    }
    if(var_hampRace() == 2013){
      hamp_data <- read.csv("data/race/hamp_race2013.csv")
    }
    if(var_hampRace() == 2012){
      hamp_data <- read.csv("data/race/hamp_race2012.csv")
    }
    if(var_hampRace() == 2011){
      hamp_data <- read.csv("data/race/hamp_race2011.csv")
    }
    if(var_hampRace() == 2010){
      hamp_data <- read.csv("data/race/hamp_race2010.csv")
    }
    hamp_data <- hamp_data[,2:6]
    #combining data from counties
    variable <- sample(c("B02001_001", "B02001_002","B02001_003", "B02001_004",
                         "B02001_005", "B02001_006", "B02001_007", "B02001_008",
                         "B02001_009", "B02001_010"),160, replace = TRUE)
    hamp_data <- hamp_data %>% group_by(variable) %>% summarize(sum(estimate))
    #select the column and rows we want
    hamp_races <- hamp_data[c(2:8),]
    #Now graphing with other = hawaiin/pi, america/alask naive. other
    hamp_races3 <- data.frame(t(hamp_data[c(2:8),2]))
    hamp_races3 <- mutate(hamp_races3, X8 = X3+X5+X6)
    hamp_races4 <- data.frame(t(hamp_races3[,c(1,2,4,7,8)]))
    colnames(hamp_races4) <- "estimate"
    total_pop = sum(hamp_races4$estimate)
    hamp_races4 <- mutate(hamp_races4, total = total_pop)
    hamp_races4 <- mutate(hamp_races4, pct = estimate/total*100)
    hamp_races4 <- mutate(hamp_races4, race = c("White", "Black", "Asian", "Two or more", "Other"))
    colnames(hamp_races4) <- c("estimate", "Total", "Percent of Population", "race")
    
    hamp_races5 <- hamp_races4 %>% 
      mutate(
        cs = rev(cumsum(rev(`Percent of Population`))), 
        pos = `Percent of Population`/2 + lead(cs, 1),
        pos = if_else(is.na(pos),`Percent of Population`/2, pos))
    
    hamp_pie <- ggplot(hamp_races5, aes(x = "" , y = `Percent of Population`, fill = fct_inorder(race))) +
      geom_col(width = 1) +
      coord_polar(theta = "y", start = 0 ) +
      geom_label_repel(aes(y = pos, label = paste0(round(`Percent of Population`, digits=2), "%")),
                       data = hamp_races5, size=4, show.legend = F, nudge_x = 1) +
      guides(fill = guide_legend(title = "Key")) +
      scale_fill_manual(values =vir_pal)+
      theme_void() +
      theme(legend.title = element_blank())
    #plot
    hamp_pie
  })
  
  
  # VA race plots -----------------------------------------------------
  var_VaRace <- reactive({
    input$VaRaceYearDrop
  })
  
  output$va_pie <- renderPlot({
    #pal
    vir_pal <- c("#33638DFF", "#1F968BFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")
    if(var_VaRace() == 2019){
      races <- read.csv("data/race/va_race2019.csv")
    }
    if(var_VaRace() == 2018){
      races <- read.csv("data/race/va_race2018.csv")
    }
    if(var_VaRace() == 2017){
      races <- read.csv("data/race/va_race2017.csv")
    }
    if(var_VaRace() == 2016){
      races <- read.csv("data/race/va_race2016.csv")
    }
    if(var_VaRace() == 2015){
      races <- read.csv("data/race/va_race2015.csv")
    }
    if(var_VaRace() == 2014){
      races <- read.csv("data/race/va_race2014.csv")
    }
    if(var_VaRace() == 2013){
      races <- read.csv("data/race/va_race2013.csv")
    }
    if(var_VaRace() == 2012){
      races <- read.csv("data/race/va_race2012.csv")
    }
    if(var_VaRace() == 2011){
      races <- read.csv("data/race/va_race2011.csv")
    }
    if(var_VaRace() == 2010){
      races <- read.csv("data/race/va_race2010.csv")
    }
    races <- races[,2:6]
    total <- races[1,4]
    #Now graphing with other = hawaiin/pi, america/alask naive. other
    va_races <-  data.frame(t((races[c(2:8), 4])))
    va_races <- mutate(va_races, X8 = X3+X5+X6)
    va_races2 <- data.frame(t(va_races[,c(1,2,4,7,8)]))
    colnames(va_races2) <- "estimate"
    va_races2 <- mutate(va_races2, totl = total)
    va_races2 <- mutate(va_races2, pct = estimate/totl*100)
    va_races2 <- mutate(va_races2, race = c("White", "Black", "Asian", "Two or more", "Other"))
    va_races3 <- va_races2 %>% 
      mutate(
        cs = rev(cumsum(rev(pct))), 
        pos = pct/2 + lead(cs, 1),
        pos = if_else(is.na(pos), pct/2, pos))
    
    va_pie <- ggplot(va_races3, aes(x = "" , y = pct, fill = fct_inorder(race))) +
      geom_col(width = 1) +
      coord_polar(theta = "y", start = 0 ) +
      geom_label_repel(aes(y = pos, label = paste0(round(pct, digits=2), "%")),
                       data = va_races3, size=4, show.legend = F, nudge_x = 1) +
      guides(fill = guide_legend(title = "Key")) +
      scale_fill_manual(values=vir_pal)+
      theme_void()  +
      theme(legend.title = element_blank()) 
    #plot 
    va_pie
    
  })
  
  # Hampton age plot-------------------------------------------------
  var_hampAge <- reactive({
    input$HampAgeYearDrop
  })
  
  output$hamp_graph <- renderPlot({
    #pal
    vir_pal <- c("#33638DFF", "#1F968BFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")
    if(var_hampAge() == 2019){
      hamp_ages  <- read.csv("data/age/hamp_age2019.csv")
    }
    if(var_hampAge() == 2018){
      hamp_ages  <- read.csv("data/age/hamp_age2018.csv")
    }
    if(var_hampAge() == 2017){
      hamp_ages  <- read.csv("data/age/hamp_age2017.csv")
    }
    if(var_hampAge() == 2016){
      hamp_ages  <- read.csv("data/age/hamp_age2016.csv")
    }
    if(var_hampAge() == 2015){
      hamp_ages  <- read.csv("data/age/hamp_age2015.csv")
    }
    if(var_hampAge() == 2014){
      hamp_ages  <- read.csv("data/age/hamp_age2014.csv")
    }
    if(var_hampAge() == 2013){
      hamp_ages  <- read.csv("data/age/hamp_age2013.csv")
    }
    if(var_hampAge() == 2012){
      hamp_ages  <- read.csv("data/age/hamp_age2012.csv")
    }
    if(var_hampAge() == 2011){
      hamp_ages  <- read.csv("data/age/hamp_age2011.csv")
    }
    if(var_hampAge() == 2010){
      hamp_ages  <- read.csv("data/age/hamp_age2010.csv")
    }
    hamp_ages <- hamp_ages[,2:6]
    #total population in hampton Roads (1713267)
    hamp_pop_tbl <- hamp_ages %>%
      group_by(NAME) %>%
      slice(1)
    hamp_pop <- colSums(hamp_pop_tbl[,4])
    #Getting male estimates for each age group (summing every county for that specific male age group)
    hamp_male <- hamp_ages %>%
      group_by(NAME) %>%
      slice(3:25)
    hamp_male2 <- hamp_male %>% group_by(variable) %>% summarize(sum(estimate, na.rm = TRUE))
    #Getting female estimates for each age group (summing every county for that specific female age group)
    hamp_female <- hamp_ages %>%
      group_by(NAME) %>%
      slice(27:49)
    hamp_female2 <- hamp_female %>% group_by(variable) %>% summarize(sum(estimate, na.rm = TRUE))
    hamp_gender <- cbind(hamp_male2, hamp_female2)
    hamp_gender <- hamp_gender[,c(2,4)]
    #hamp_gender2 <- data.frame(estimate = c(hamp_gender[,1], hamp_gender[,2]))
    colnames(hamp_gender) <- c("male", "female")
    hamp_gender <- mutate(hamp_gender, total = male+female)
    #transposing just the estimates
    hamp_ages2 <- data.frame(t(hamp_gender[,3]))
    #sorting into the age groups
    hamp_ages2 <- mutate(hamp_ages2, Under18 = X1 + X2 + X3 + X4)
    hamp_ages2 <- mutate(hamp_ages2, YoungAdult = X5 + X6 + X7 + X8 + X9)
    hamp_ages2 <- mutate(hamp_ages2, Adult = X10 + X11 + X12)
    hamp_ages2 <- mutate(hamp_ages2, MiddleAge = X13 + X14 + X15 + X16 + X17)
    hamp_ages2 <- mutate(hamp_ages2, Senior = X18 + X19 + X20 + X21 + X22 + X23)
    #using just the 5 age group data that was just sorted
    hamp_ages3 <- hamp_ages2[,24:28]
    row.names(hamp_ages3) <- "General Estimate"
    hamp_ages3 <- data.frame(t(hamp_ages3))
    #Getting the percentage
    hamp_ages3 <- mutate(hamp_ages3,TotalPopulation = hamp_pop)
    hamp_ages3 <- mutate(hamp_ages3, PctPop = General.Estimate/TotalPopulation*100)
    hamp_ages3 <- mutate(hamp_ages3, Labels =c("Under 18", "18 to 29", "30 to 44",
                                               "45 to 64","65 and Older"))
    #ordering the age grpups
    hamp_ages3$Labels <- factor(hamp_ages3$Labels, levels=c("Under 18", "18 to 29", "30 to 44",
                                                            "45 to 64","65 and Older"))
    #Graph
    
    
    hamp_graph <- ggplot(hamp_ages3 , aes(x="", y=PctPop, fill=Labels)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + 
      theme_void() +
      theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 13)) +
      geom_text(aes(label = paste0(round(PctPop), "%")), position = position_stack(vjust=0.5), size=5, color = "white")  +
      scale_fill_manual(values =vir_pal)
    #plot
    hamp_graph
    
  })
  
  # Va age plot----------------------------------------------------
  var_VaAge <- reactive({
    input$VaAgeYearDrop
  })
  
  output$va_graph <- renderPlot({
    #pal
    vir_pal <- c("#33638DFF", "#1F968BFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")
    if(var_VaAge() == 2019){
      age1  <- read.csv("data/age/va_age2019.csv")
    }
    if(var_VaAge() == 2018){
      age1  <- read.csv("data/age/va_age2018.csv")
    }
    if(var_VaAge() == 2017){
      age1  <- read.csv("data/age/va_age2017.csv")
    }
    if(var_VaAge() == 2016){
      age1  <- read.csv("data/age/va_age2016.csv")
    }
    if(var_VaAge() == 2015){
      age1  <- read.csv("data/age/va_age2015.csv")
    }
    if(var_VaAge() == 2014){
      age1  <- read.csv("data/age/va_age2014.csv")
    }
    if(var_VaAge() == 2013){
      age1  <- read.csv("data/age/va_age2013.csv")
    }
    if(var_VaAge() == 2012){
      age1  <- read.csv("data/age/va_age2012.csv")
    }
    if(var_VaAge() == 2011){
      age1  <- read.csv("data/age/va_age2011.csv")
    }
    if(var_VaAge() == 2010){
      age1  <- read.csv("data/age/va_age2010.csv")
    }
    age1 <- age1[,2:6]
    va_total_pop<- age1[1,4]
    #Adds the female and male data together to get the population for each age group
    va_male_age <- age1[3:25,]
    va_female_age <- age1[27:49,]
    va_male_age <- tibble::rowid_to_column(va_male_age, "ID")
    va_female_age <- tibble::rowid_to_column(va_female_age, "ID")
    #adding the male and female estimates to get the total
    ages <- merge(va_female_age, va_male_age, by = "ID")
    ages <- mutate(ages, total = estimate.x + estimate.y)
    #Getting just the estimates for each age group and transposing it to combining rows easily
    va_ages1 <- data.frame(t(ages[,12]))
    #Groups: Under 18, 18-30, 30-45, 45-65, 65+
    va_ages1 <- mutate(va_ages1, Under18 = X1 + X2 + X3 + X4)
    va_ages1 <- mutate(va_ages1, YoungAdult = X5 + X6 + X7 + X8 + X9)
    va_ages1 <- mutate(va_ages1, Adult = X10 + X11 + X12)
    va_ages1 <- mutate(va_ages1, MiddleAge = X13 + X14 + X15 + X16 + X17)
    va_ages1 <- mutate(va_ages1, Senior = X18 + X19 + X20 + X21 + X22 + X23)
    #using the 5 age group data
    va_ages2 <- va_ages1[,24:28]
    row.names(va_ages2) <- "Estimate"
    va_ages2 <- data.frame(t(va_ages2))
    va_ages2 <- mutate(va_ages2,TotalPopulation = va_total_pop)
    #Make Percentage
    va_ages2 <- mutate(va_ages2, PctPop = Estimate/TotalPopulation*100)
    #labeling
    va_ages2 <- mutate(va_ages2, labels = c("Under 18", "18 to 29", "30 to 44",
                                            "45 to 64","65 and Older"))
    colnames(va_ages2) <- c("Estimate", "Total Population", "Percent of Population", "Labels")
    va_ages2[,4] <- factor(va_ages2[,4], levels = c("Under 18", "18 to 29", "30 to 44",
                                                    "45 to 64","65 and Older"))
    #Graph
    va_graph <- ggplot(va_ages2 , aes(x="", y=`Percent of Population`, fill=Labels)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + 
      theme_void() +
      theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 13)) +
      geom_text(aes(label = paste0(round(`Percent of Population`), "%")), position = position_stack(vjust=0.5), size=5, color = "white") +
      scale_fill_manual(values =vir_pal)
    #plot
    va_graph
    
  })
  
  
  # Hampton Counties Map ------------------------------------------
  var_hampCountiesAge <- reactive({
    input$HampCountAgeYearDrop
  })
  
  output$age_map <- renderPlot({
    if(var_hampCountiesAge() == 2019){
      hamp_ages  <- read.csv("data/age/hamp_age2019.csv")
    }
    if(var_hampCountiesAge() == 2018){
      hamp_ages  <- read.csv("data/age/hamp_age2018.csv")
    }
    if(var_hampCountiesAge() == 2017){
      hamp_ages  <- read.csv("data/age/hamp_age2017.csv")
    }
    if(var_hampCountiesAge() == 2016){
      hamp_ages  <- read.csv("data/age/hamp_age2016.csv")
    }
    if(var_hampCountiesAge() == 2015){
      hamp_ages  <- read.csv("data/age/hamp_age2015.csv")
    }
    if(var_hampCountiesAge() == 2014){
      hamp_ages  <- read.csv("data/age/hamp_age2014.csv")
    }
    if(var_hampCountiesAge() == 2013){
      hamp_ages  <- read.csv("data/age/hamp_age2013.csv")
    }
    if(var_hampCountiesAge() == 2012){
      hamp_ages  <- read.csv("data/age/hamp_age2012.csv")
    }
    if(var_hampCountiesAge() == 2011){
      hamp_ages  <- read.csv("data/age/hamp_age2011.csv")
    }
    if(var_hampCountiesAge() == 2010){
      hamp_ages  <- read.csv("data/age/hamp_age2010.csv")
    }
    hamp_ages <- hamp_ages[,2:6]
    county_pop <- hamp_ages %>% group_by(NAME) %>%
      slice(1)
    county_pop <- county_pop[,4]
    #Getting male estimates for each age group 
    county_male <- hamp_ages %>%
      group_by(NAME) %>%
      slice(3:25)
    #Getting female estimates for each age group (summing every county for that specific female age group)
    county_female <- hamp_ages %>%
      group_by(NAME) %>%
      slice(27:49)
    #assigning ID to merge female and male estimates to get overall estimates
    county_male <- tibble::rowid_to_column(county_male, "ID")
    county_female <- tibble::rowid_to_column(county_female, "ID")
    county_ages <- merge(county_female, county_male, by = "ID")
    county_ages <- mutate(county_ages, total = estimate.x + estimate.y)
    #get the estimates put in the age groups(map data)
    #under 18
    county_under <- county_ages %>%
      group_by(NAME.y) %>%
      slice(1:4)
    county_under2 <- county_under %>%
      group_by(NAME.y) %>%
      summarise(x=sum(total))
    county_under2<- county_under2[,2]
    #young adult
    county_ya <- county_ages %>%
      group_by(NAME.y) %>%
      slice(5:9)
    county_ya2 <- county_ya %>%
      group_by(NAME.y) %>%
      summarise(x=sum(total))
    county_ya2 <- county_ya2[,2]
    #adult
    county_adult <- county_ages %>%
      group_by(NAME.y) %>%
      slice(10:12)
    county_adult2 <- county_adult %>%
      group_by(NAME.y) %>%
      summarise(x=sum(total))
    county_adult2 <- county_adult2[,2]
    #middle age
    county_ma <- county_ages %>%
      group_by(NAME.y) %>%
      slice(13:17)
    county_ma2 <- county_ma %>%
      group_by(NAME.y) %>%
      summarise(x=sum(total))
    county_ma2 <- county_ma2[,2]
    #senior
    county_senior <- county_ages %>%
      group_by(NAME.y) %>%
      slice(18:23)
    county_senior2 <- county_senior %>%
      group_by(NAME.y) %>%
      summarise(x=sum(total))
    county_senior2 <- county_senior2[,2]
    counties_label <- c("Chesapeake", "Franklin", "Hampton", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Suffolk", "Virginia Beach", "Williamsburg", "Gloucester", "Isle of Wight", "James City", "Mathews", "Southampton", "York")
    #getting coordinates
    lat <- c(36.690473, 36.683540, 37.046933, 37.123232, 36.903378, 37.130348, 36.878493, 36.714941, 36.792042,      
             37.267284, 37.405450, 36.901637, 37.311197, 37.470724, 36.720152, 37.242246)
    lon <- c(-76.297654, -76.940148, -76.390236, -76.523771, -76.248186, -76.357799, -76.380289, -76.626346,
             -76.053855, -76.708205, -76.519133, -76.708161, -76.804677, -76.375820, -77.114512, -76.566393)
    #format
    general_county_alt <-cbind(county_under2, county_ya2, county_adult2, county_ma2, county_senior2, county_pop)
    colnames(general_county_alt) <- c("a", "b", "c", "d", "e", "total")
    general_county_alt <-  mutate(general_county_alt, under = a/total*100)
    general_county_alt <-  mutate(general_county_alt, ya = b/total*100)
    general_county_alt <-  mutate(general_county_alt, adult = c/total*100)
    general_county_alt <-  mutate(general_county_alt, ma = d/total*100)
    general_county_alt <-  mutate(general_county_alt, senior = e/total*100)
    general_county_alt2 <- general_county_alt[,7:11]
    general_county_alt2 <-  mutate(general_county_alt2, county = counties_label)
    general_county_alt2 <- cbind(general_county_alt2, lon, lat)
    colnames(general_county_alt2) <- c("A", "B","C","D", "E", "county", "lon", "lat")
    #Getting map data for counties in Hampton roads
    coord_data <- read_rds("data/age/coordinates.rds")
    coord_data <- st_transform(coord_data)
    coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
    coordinates2 <- coordinates1[,6]
    city <- c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews", 
              "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
              "Williamsburg", "York")
    coordinates2 <- mutate(coordinates2, Loc = city)
    coordinates2$Loc[coordinates2$Loc == "Franklin"] <- "Franklin City"
    #Graph
    age_map <- ggplot(coordinates2) +
      geom_sf() +
      geom_scatterpie(aes(x=lon, y=lat, group=county, r =0.05), data=general_county_alt2,
                      cols=LETTERS[1:5]) +
      geom_sf_label(aes(label=Loc,geometry = geometry), label.padding = unit(.5, "mm"), size =4, nudge_x=0.05, nudge_y = 0.05) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=13)) +
      #geom_scatterpie(aes(x=lon, y=lat, group=county, r =0.05), data=general_county_alt2,
      #             cols=LETTERS[1:5]) + 
      scale_fill_viridis_d(labels = c("Under 18", "18 to 29", "30 to 44",
                                      "45 to 64","65 and Older")) 
    #plot
    age_map
    
  })
  
  # Total Population Educational Attainment ---------------------------------
  
  var_genEducationalAttainment <- reactive({
    input$genEdAttainmentYearDrop
  })
  
  output$genEdAttainmentPlots <- renderPlotly({
    if(var_genEducationalAttainment() == "2019") {
      generalEducationalAttainment <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2019.csv")
      generalBlackEducationalAttainment <-  read.csv("data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2019.csv")
      colnames(generalEducationalAttainment) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <- c( "Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
      generalEducationalAttainment$Variable  <- rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <- rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <- cbind(generalBlackEducationalAttainment$Name, generalBlackEducationalAttainment$Variable, generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      modifiedGeneralEducationalAttainment <- cbind(generalEducationalAttainment$Name, generalEducationalAttainment$Variable, generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      generalTotal <- rbind(modifiedGeneralEducationalAttainment, modifiedGeneralBlackEducationalAttainment)
      colnames(generalTotal)  <- c("Name", "Demographic","Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal) 
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-  as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
      va_tot_education_bar <- generalTotal %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        arrange(desc(Name)) %>% 
        ggplot(aes(fill = Demographic, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent (%)",
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if(var_genEducationalAttainment() == "2018") {
      generalEducationalAttainment <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2018.csv")
      generalBlackEducationalAttainment <-  read.csv("data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2018.csv")
      colnames(generalEducationalAttainment) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <- c( "Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
      generalEducationalAttainment$Variable  <- rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <- rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <- cbind(generalBlackEducationalAttainment$Name, generalBlackEducationalAttainment$Variable, generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      modifiedGeneralEducationalAttainment <- cbind(generalEducationalAttainment$Name, generalEducationalAttainment$Variable, generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      generalTotal <- rbind(modifiedGeneralEducationalAttainment, modifiedGeneralBlackEducationalAttainment)
      colnames(generalTotal)  <- c("Name", "Demographic","Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal) 
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-  as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
      va_tot_education_bar <- generalTotal %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        arrange(desc(Name)) %>% 
        ggplot(aes(fill = Demographic, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent (%)",
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if(var_genEducationalAttainment() == "2017") {
      generalEducationalAttainment <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2017.csv")
      generalBlackEducationalAttainment <-  read.csv("data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2017.csv")
      colnames(generalEducationalAttainment) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <- c( "Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
      generalEducationalAttainment$Variable  <- rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <- rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <- cbind(generalBlackEducationalAttainment$Name, generalBlackEducationalAttainment$Variable, generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      modifiedGeneralEducationalAttainment <- cbind(generalEducationalAttainment$Name, generalEducationalAttainment$Variable, generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      generalTotal <- rbind(modifiedGeneralEducationalAttainment, modifiedGeneralBlackEducationalAttainment)
      colnames(generalTotal)  <- c("Name", "Demographic","Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal) 
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-  as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
      va_tot_education_bar <- generalTotal %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        arrange(desc(Name)) %>% 
        ggplot(aes(fill = Demographic, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent (%)",
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if(var_genEducationalAttainment() == "2016") {
      generalEducationalAttainment <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2016.csv")
      generalBlackEducationalAttainment <-  read.csv("data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2016.csv")
      colnames(generalEducationalAttainment) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <- c( "Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
      generalEducationalAttainment$Variable  <- rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <- rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <- cbind(generalBlackEducationalAttainment$Name, generalBlackEducationalAttainment$Variable, generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      modifiedGeneralEducationalAttainment <- cbind(generalEducationalAttainment$Name, generalEducationalAttainment$Variable, generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      generalTotal <- rbind(modifiedGeneralEducationalAttainment, modifiedGeneralBlackEducationalAttainment)
      colnames(generalTotal)  <- c("Name", "Demographic","Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal) 
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-  as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
      va_tot_education_bar <- generalTotal %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        arrange(desc(Name)) %>% 
        ggplot(aes(fill = Demographic, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent (%)",
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if(var_genEducationalAttainment() == "2015") {
      generalEducationalAttainment <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2015.csv")
      generalBlackEducationalAttainment <-  read.csv("data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2015.csv")
      colnames(generalEducationalAttainment) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <- c( "Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
      generalEducationalAttainment$Variable  <- rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <- rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <- cbind(generalBlackEducationalAttainment$Name, generalBlackEducationalAttainment$Variable, generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      modifiedGeneralEducationalAttainment <- cbind(generalEducationalAttainment$Name, generalEducationalAttainment$Variable, generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      generalTotal <- rbind(modifiedGeneralEducationalAttainment, modifiedGeneralBlackEducationalAttainment)
      colnames(generalTotal)  <- c("Name", "Demographic","Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal) 
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-  as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
      va_tot_education_bar <- generalTotal %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        arrange(desc(Name)) %>% 
        ggplot(aes(fill = Demographic, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent (%)",
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if(var_genEducationalAttainment() == "2014") {
      generalEducationalAttainment <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2014.csv")
      generalBlackEducationalAttainment <-  read.csv("data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2014.csv")
      colnames(generalEducationalAttainment) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <- c( "Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
      generalEducationalAttainment$Variable  <- rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <- rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <- cbind(generalBlackEducationalAttainment$Name, generalBlackEducationalAttainment$Variable, generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      modifiedGeneralEducationalAttainment <- cbind(generalEducationalAttainment$Name, generalEducationalAttainment$Variable, generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      generalTotal <- rbind(modifiedGeneralEducationalAttainment, modifiedGeneralBlackEducationalAttainment)
      colnames(generalTotal)  <- c("Name", "Demographic","Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal) 
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-  as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
      va_tot_education_bar <- generalTotal %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        arrange(desc(Name)) %>% 
        ggplot(aes(fill = Demographic, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent (%)",
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if(var_genEducationalAttainment() == "2013") {
      generalEducationalAttainment <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2013.csv")
      generalBlackEducationalAttainment <-  read.csv("data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2013.csv")
      colnames(generalEducationalAttainment) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <- c( "Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
      generalEducationalAttainment$Variable  <- rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <- rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <- cbind(generalBlackEducationalAttainment$Name, generalBlackEducationalAttainment$Variable, generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      modifiedGeneralEducationalAttainment <- cbind(generalEducationalAttainment$Name, generalEducationalAttainment$Variable, generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      generalTotal <- rbind(modifiedGeneralEducationalAttainment, modifiedGeneralBlackEducationalAttainment)
      colnames(generalTotal)  <- c("Name", "Demographic","Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal) 
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-  as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
      va_tot_education_bar <- generalTotal %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        arrange(desc(Name)) %>% 
        ggplot(aes(fill = Demographic, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent (%)",
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if(var_genEducationalAttainment() == "2012") {
      generalEducationalAttainment <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2012.csv")
      generalBlackEducationalAttainment <-  read.csv("data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2012.csv")
      colnames(generalEducationalAttainment) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <- c( "Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
      generalEducationalAttainment$Variable  <- rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <- rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <- cbind(generalBlackEducationalAttainment$Name, generalBlackEducationalAttainment$Variable, generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      modifiedGeneralEducationalAttainment <- cbind(generalEducationalAttainment$Name, generalEducationalAttainment$Variable, generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      generalTotal <- rbind(modifiedGeneralEducationalAttainment, modifiedGeneralBlackEducationalAttainment)
      colnames(generalTotal)  <- c("Name", "Demographic","Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal) 
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-  as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
      va_tot_education_bar <- generalTotal %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        arrange(desc(Name)) %>% 
        ggplot(aes(fill = Demographic, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent (%)",
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    else if(var_genEducationalAttainment() == "2011") {
      generalEducationalAttainment <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2011.csv")
      generalBlackEducationalAttainment <-  read.csv("data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2011.csv")
      colnames(generalEducationalAttainment) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <- c( "Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
      generalEducationalAttainment$Variable  <- rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <- rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <- cbind(generalBlackEducationalAttainment$Name, generalBlackEducationalAttainment$Variable, generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      modifiedGeneralEducationalAttainment <- cbind(generalEducationalAttainment$Name, generalEducationalAttainment$Variable, generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      generalTotal <- rbind(modifiedGeneralEducationalAttainment, modifiedGeneralBlackEducationalAttainment)
      colnames(generalTotal)  <- c("Name", "Demographic","Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal) 
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-  as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
      va_tot_education_bar <- generalTotal %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        arrange(desc(Name)) %>% 
        ggplot(aes(fill = Demographic, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent (%)",
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
    
    else if(var_genEducationalAttainment() == "2010") {
      generalEducationalAttainment <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2010.csv")
      generalBlackEducationalAttainment <-  read.csv("data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2010.csv")
      colnames(generalEducationalAttainment) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      colnames(generalBlackEducationalAttainment) <- c("Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
      generalEducationalAttainment$Variable  <- rep(c("Total Population"), 16)
      generalBlackEducationalAttainment$Variable  <- rep(c("Black Population"), 16)
      modifiedGeneralBlackEducationalAttainment <- cbind(generalBlackEducationalAttainment$Name, generalBlackEducationalAttainment$Variable, generalBlackEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      modifiedGeneralEducationalAttainment <- cbind(generalEducationalAttainment$Name, generalEducationalAttainment$Variable, generalEducationalAttainment$`Bachelor or Higher as Highest Attainment %`)
      generalTotal <- rbind(modifiedGeneralEducationalAttainment, modifiedGeneralBlackEducationalAttainment)
      colnames(generalTotal)  <- c("Name", "Demographic","Bachelor or Higher as Highest Attainment %")
      generalTotal <- as.data.frame.matrix(generalTotal) 
      generalTotal$`Bachelor or Higher as Highest Attainment %` <-  as.numeric(generalTotal$`Bachelor or Higher as Highest Attainment %`)
      
      va_tot_education_bar <- generalTotal %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>%
        arrange(desc(Name)) %>% 
        ggplot(aes(fill = Demographic, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent (%)",
             x = "") + theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      ggplotly(va_tot_education_bar)
    }
    
  })
  
  # Teacher Demographics------------------------------------------------------
  
  var_teacherRaces <- reactive({
    input$teacherRaceBreakdown
  })
  
  
  output$teacherRacePlots <- renderPlotly({
    if(var_teacherRaces() == "Black") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Black Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40))  + 
        scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) 
    }
    
    else if (var_teacherRaces() == "Asian") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Asian Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40)) + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y")))
    }
    
    else if (var_teacherRaces() == "White") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of White Teachers`, fill = Name)) + geom_col() +
        labs(title = "White Teacher Breakdown", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40))   + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) 
    }
    
    else if (var_teacherRaces() == "Hispanic") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Hispanic Teachers`, fill = Name)) + geom_col() +
        labs(title = "Hispanic Teacher Breakdown", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40))   + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) 
    }
    
    else if (var_teacherRaces() == "American Indian") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of American Indian Teachers`, fill = Name)) + geom_col() +
        labs(title = "American Indian Teacher Breakdown", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40))   + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) 
    }
    
    else if (var_teacherRaces() == "Two or More Races") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Two Or More Races Teachers`, fill = Name)) + geom_col() +
        labs(title = "Two or More Races Teacher Breakdown", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40)) + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) 
      
    }
    
    else if (var_teacherRaces() == "Hawaiian") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Hawaiian Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "") + theme(axis.text.x = element_text(angle = 40))   + scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y")))
    }
    
  }
  )
  
  
  
  # suspension line graph-------------------------------------------------------
  
  output$suspension_line_graph <- renderPlotly({
    year <- "2018-2019"
    suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race19 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
    va_suspension_race19$Data <- as.numeric(va_suspension_race19$Data)
    va_suspension_race19 <- mutate(va_suspension_race19, Data = Data*100)
    va_suspension_race19 <- mutate(va_suspension_race19, race = c("Black", "Hispanic", "White"))
    va_suspension_race19 <- mutate(va_suspension_race19, year = "2019")
    ##
    year <- "AY 2017-2018"
    suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race18 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
    va_suspension_race18$Data <- as.numeric(va_suspension_race18$Data)
    va_suspension_race18 <- mutate(va_suspension_race18, Data = Data*100)
    va_suspension_race18 <- mutate(va_suspension_race18, race = c("Black", "Hispanic", "White"))
    va_suspension_race18 <- mutate(va_suspension_race18, year = "2018")
    ##
    year <- "AY 2016-2017"
    suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race17 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
    va_suspension_race17$Data <- as.numeric(va_suspension_race17$Data)
    va_suspension_race17 <- mutate(va_suspension_race17, Data = Data*100)
    va_suspension_race17 <- mutate(va_suspension_race17, race = c("Black", "Hispanic", "White"))
    va_suspension_race17 <- mutate(va_suspension_race17, year = "2017")
    ##
    year <- "AY 2015-2016"
    suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(black, hispanic, white)
    va_suspension_race16 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
    va_suspension_race16$Data <- as.numeric(va_suspension_race16$Data)
    va_suspension_race16 <- mutate(va_suspension_race16, Data = Data*100)
    va_suspension_race16 <- mutate(va_suspension_race16, race = c("Black", "Hispanic", "White"))
    va_suspension_race16 <- mutate(va_suspension_race16, year = "2016")
    ##
    year <- "AY 2014-2015"
    suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race15 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
    va_suspension_race15$Data <- as.numeric(va_suspension_race15$Data)
    va_suspension_race15 <- mutate(va_suspension_race15, Data = Data*100)
    va_suspension_race15 <- mutate(va_suspension_race15, race = c("Black", "Hispanic", "White"))
    va_suspension_race15 <- mutate(va_suspension_race15, year = "2015")
    #combining data
    suspension_line <- rbind(va_suspension_race19, va_suspension_race18, va_suspension_race17, va_suspension_race16, va_suspension_race15)
    colnames(suspension_line) <- c("Percent Suspended", "Race", "Year")
    suspension_line_graph <- ggplot(suspension_line, aes(x=Year, y=`Percent Suspended`, group = Race, color = Race)) + 
      geom_line(position = "identity", size =1.5) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        axis.text = element_text(size=13)) +
      labs(y ="Percent (%)") +
      scale_color_viridis_d() +
      scale_y_continuous(limits = c(2,14), breaks = seq(0, 14, by =2))
    #plot
    ggplotly(suspension_line_graph, tooltip = c("x", "y", "group")) %>%
      layout(legend = list(y=0.5))
    
  })
  
  #suspension gap line graph ------------------------------------------------
  output$suspensionGap <- renderPlotly({
    gap_data <- read.csv("data/suspension/suspensionGap.csv")
    gap_data$year[gap_data$year == "2018-2019"] <- "2019"
    gap_data$year[gap_data$year == "2017-2018"] <- "2018"
    gap_data$year[gap_data$year == "2016-2017"] <- "2017"
    gap_data$year[gap_data$year == "2015-2016"] <- "2016"
    gap_data$year[gap_data$year == "2014-2015"] <- "2015"
    colnames(gap_data) <- c("x", "Location", "Percent Difference", "Year")
    susGapPlot <- ggplot(gap_data, aes(x=Year, y=`Percent Difference`, group = Location, color =Location)) + 
      geom_line(position = "identity", size =1.5) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        axis.text = element_text(siz=13)) +
      labs(y ="Percent Difference (%)") +
      scale_color_viridis_d() +
      ylim(0,16)
    
    suspensionGap <- ggplotly(susGapPlot, tooltip = c("x", "y", "group")) %>%
      layout(legend = list(y=0.5))
    
  })
  
  
  # Suspension for black and white (counties) ---------------------------------
  var_BWsuspension <- reactive({
    input$BWsuspensionYearDrop
  })
  
  output$BW_map <- renderPlotly({
    if(var_BWsuspension() == "2019"){
      year <- "2018-2019"
      suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
      city <- c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews",
                "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
                "Williamsburg", "York")
      suspension_counties <-filter(suspension_data, Location %in% city)
      
      pct_white<- suspension_counties %>% filter(Race=="White") %>%
        filter(DataFormat=="Percent")
      pct_white2 <- pct_white %>% filter(TimeFrame==year)
      pct_white2$Location[pct_white2$Location == "Williamsburg"] <- "Williamsburg-James City"
      pct_white2 <- pct_white2[c(1:5,7:16),]
      #putting NAs and Ss in a table
      pct_white2$Data[is.na(pct_white2$Data)] <- "NA"
      display_tbl_white <- pct_white2 %>% filter(Data %in% c("NA", "S", "<", "*"))
      display_tbl_white2<- display_tbl_white[,c(2,3,6)]
      pct_white2$Data[pct_white2$Data=="NA"] <- 0
      pct_white2$Data[pct_white2$Data=="S"] <- 0
      pct_white2$Data[pct_white2$Data=="<"] <- 0
      pct_white2$Data[pct_white2$Data=="*"] <- 0
      #adding estimates by 100 (need to convert to numeric first)
      pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
      pct_white2 <- mutate(pct_white2, pct = Data *100)
      pct_white2$pct <- na_if(pct_white2$pct,0.00000)
      as.numeric(pct_white2$pct, na.rm = TRUE)
      #labeling
      pct_white3 <- pct_white2[,c(2,7)]
      colnames(pct_white3) <- c("Location", "Percent (%)")
      #black data
      suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
        filter(DataFormat=="Percent")
      suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
      suspension_pct2$Location[suspension_pct2$Location == "Williamsburg"] <- "Williamsburg-James City"
      suspension_pct2 <- suspension_pct2[c(1:5,7:16),]
      #make a table w/ NA a S
      suspension_pct2$Data[is.na(suspension_pct2$Data)] <- "NA"
      display_tbl_black <- suspension_pct2 %>% filter(Data %in% c("NA", "S","<", "*"))
      display_tbl_black2 <- display_tbl_black[,c(2,3,6)]
      suspension_pct2$Data[suspension_pct2$Data=="NA"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="S"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="<"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="*"] <- 0
      #convert data column to numeric so we can multiply by 100
      suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
      suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
      suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
      as.numeric(suspension_pct2$pct, na.rm = TRUE)
      pct_blck <-suspension_pct2[,c(2,7)]
      colnames(pct_blck) <- c("Location", "Percent (%)")
      sus <- rbind(pct_blck,pct_white3)
      num <- nrow(sus)/2
      sus <- mutate(sus, Race = c(rep("Black",num), rep("White", num)))
      #bar graph
      suspension_counties_plot <-
        ggplot(sus , aes(Location, y=`Percent (%)`, fill=Race)) +
        geom_bar(stat="identity", position=position_dodge())+
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
              legend.key.height = unit(0.3, 'cm'),
              legend.key.width = unit(0.3, 'cm'),
              legend.title = element_blank(),
              legend.text = element_text(size=14),
              axis.text=element_text(size=12),
              axis.title.y = element_text(size=13),
              axis.title=element_text(size=17),
              axis.title.x=element_blank()) +
        theme(axis.text.x = element_text(angle = 40, vjust = 0.95, hjust=1))+
        scale_fill_manual(values=c("#D55E00","#0072B2")) +
        labs(x = "Location")
      
      #plot
      BW_map <- suspension_counties_plot
      ggplotly(BW_map)
      
    }
    else if (var_BWsuspension() %in% c("2018", "2017", "2016", "2015")){
      if (var_BWsuspension() == "2018") {
        year <- "AY 2017-2018"
      }
      else if (var_BWsuspension() == "2017") {
        year <- "AY 2016-2017"
      }
      else if (var_BWsuspension() == "2016") {
        year <- "AY 2015-2016"
      }
      else if (var_BWsuspension() == "2015") {
        year <- "AY 2014-2015"
      }
      suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
      city <- c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews",
                "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
                "Williamsburg", "York")
      suspension_counties <-filter(suspension_data, Location %in% city)
      
      pct_white<- suspension_counties %>% filter(Race=="White") %>%
        filter(DataFormat=="Percent")
      pct_white2 <- pct_white %>% filter(TimeFrame==year)
      pct_white2$Location[pct_white2$Location == "James City"] <- "Williamsburg-James City"
      pct_white2$Location[pct_white2$Location == "Williamsburg"] <- "Williamsburg-James City"
      #putting NAs and Ss in a table
      pct_white2$Data[is.na(pct_white2$Data)] <- "NA"
      display_tbl_white <- pct_white2 %>% filter(Data %in% c("NA", "S", "<", "*"))
      display_tbl_white2<- display_tbl_white[,c(2,3,6)]
      pct_white2$Data[pct_white2$Data=="NA"] <- 0
      pct_white2$Data[pct_white2$Data=="S"] <- 0
      pct_white2$Data[pct_white2$Data=="<"] <- 0
      pct_white2$Data[pct_white2$Data=="*"] <- 0
      #adding estimates by 100 (need to convert to numeric first)
      pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
      pct_white2 <- mutate(pct_white2, pct = Data *100)
      pct_white2$pct <- na_if(pct_white2$pct,0.00000)
      as.numeric(pct_white2$pct, na.rm = TRUE)
      #labeling
      pct_white3 <- pct_white2[,c(2,7)]
      colnames(pct_white3) <- c("Location", "Percentage of Students (%)")
      #black data
      suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
        filter(DataFormat=="Percent")
      suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
      suspension_pct2$Location[suspension_pct2$Location == "James City"] <- "Williamsburg-James City"
      suspension_pct2$Location[suspension_pct2$Location == "Williamsburg"] <- "Williamsburg-James City"
      #make a table w/ NA a S
      suspension_pct2$Data[is.na(suspension_pct2$Data)] <- "NA"
      display_tbl_black <- suspension_pct2 %>% filter(Data %in% c("NA", "S","<", "*"))
      display_tbl_black2 <- display_tbl_black[,c(2,3,6)]
      suspension_pct2$Data[suspension_pct2$Data=="NA"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="S"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="<"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="*"] <- 0
      #convert data column to numeric so we can multiply by 100
      suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
      suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
      suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
      as.numeric(suspension_pct2$pct, na.rm = TRUE)
      pct_blck <-suspension_pct2[,c(2,7)]
      colnames(pct_blck) <- c("Location", "Percentage of Students (%)")
      sus <- rbind(pct_blck,pct_white3)
      num <- nrow(sus)/2
      sus <- mutate(sus, Race = c(rep("Black",num), rep("White", num)))
      #bar graph
      suspension_counties_plot <-
        ggplot(sus , aes(Location, y=`Percentage of Students (%)`, fill=Race)) +
        geom_bar(stat="identity", position=position_dodge())+
        theme_minimal() +
        ylab("Percent (%)")+
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
              legend.key.height = unit(0.3, 'cm'),
              legend.key.width = unit(0.3, 'cm'),
              legend.title = element_blank(),
              legend.text = element_text(size=14),
              axis.title.y = element_text(size=13),
              axis.text=element_text(size=12),
              axis.title=element_text(size=17),
              axis.title.x=element_blank()) +
        theme(axis.text.x = element_text(angle = 40, vjust = 0.95, hjust =1)) +
        scale_fill_manual(values=c("#D55E00","#0072B2")) +
        labs(x = "Location")
      #plot
      BW_map <- suspension_counties_plot
      ggplotly(BW_map)
    }
  })
  
  
  # On Time Graduation Rates -----------------------------------------------------------
  # 
  rates <- read_csv("data/on_time_graduation.csv")
  rates <- rates %>% 
    na.omit(rates)
  
  basemap <- leaflet(width = "100%", height = "400px" ) %>% 
    addProviderTiles("CartoDB.Positron")
  
  colors <- c("#D55E00", "#0072B2")
  
  output$dropout_map <- renderLeaflet({ 
    grad_map <- basemap %>% 
      addMinicharts(
        rates$lon, rates$lat,
        chartdata = rates[, c("Black Students", "All Students")],
        time = rates$`Cohort Year`,
        colorPalette = colors,
      )
  })
  # var_dropoutrate <- reactive({
  #   input$DropoutDropdown
  # })
  # 
  # output$dropout_map <- renderLeaflet({
  #   if(var_dropoutrate() == "2020") {
  #     dropout_20 <- read.csv("data/dropout2020.csv")
  #     mapping <- read.csv("data/dropoutmapdata.csv")
  #     colors <- c("#0072B2", "#D55E00")
  #     dropout_20_map <- leaflet() %>% 
  #       addProviderTiles("CartoDB.Voyager") %>% 
  #       addMinicharts(
  #         mapping$lon, mapping$lat,
  #         chartdata = dropout_20,
  #         colorPalette = colors,
  #         width = 45, height = 45
  #       )
  #   }
  # })
  
  # Median Income plots: Working on it --------------------------------
  var_medianIncome <- reactive({
    input$MedianIncomeYearDrop
  })
  
  output$income_plot <- renderPlot({
    if(var_medianIncome() %in% c("2019", "2018", "2017")){
      if(var_medianIncome() == "2019") { 
        va_yr <- read.csv("data/income/va_income2019.csv")
        hamp_yr <- read.csv("data/income/hampton_income2019.csv")
      }
      else if(var_medianIncome() == "2018") { 
        va_yr <- read.csv("data/income/va_income2018.csv")
        hamp_yr <- read.csv("data/income/hampton_income2018.csv")
      }
      else if(var_medianIncome() == "2017") { 
        va_yr <- read.csv("data/income/va_income2017.csv")
        hamp_yr <- read.csv("data/income/hampton_income2017.csv")
      }
      va_yr <- va_yr[2:6]
      race_names <- c("Total", "Black")
      #VA income
      va_race_income_median <- data.frame(va_yr[c(81,83), 4])
      va_race_income <- data.frame(cbind(race_names, va_race_income_median))
      colnames(va_race_income) <- c("Race", "Median Income")
      #Hampton Income
      hamp_yr <- hamp_yr[2:6]
      #getting the name, variable and estimate
      hamp_income2 <- hamp_yr[,2:4]
      hamp_income3 <- hamp_income2 %>%
        group_by(NAME) %>%
        slice(c(81,83))
      variable <- sample(c("S1903_C03_001","S1903_C03_003"),32, replace = TRUE)
      hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
      #Putting them together
      median_income <- cbind(va_race_income, hamp_race_income_median)
      median_income <- median_income[, c(2,4)]
      #having all the estimates in the same column
      median_income2 <- data.frame(median = c(median_income[,1], median_income[,2]))
      #labeling
      median_income2 <- mutate(median_income2, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
      median_income2 <- mutate(median_income2, demo = rep(c("Total Population", "Black Population"),2))
      colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
      #making them all numeric
      median_income2 <- transform(median_income2, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
      colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
      median_income2$Location <- factor(median_income2$Location, levels= c("Hampton Roads","Virginia"))
      #Graph
      income_plot <- ggplot(median_income2, aes(x=Location, y=`Median Income (US Dollars)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Median Income (US Dollars)`))), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=5)+
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
              legend.title = element_blank(),
              legend.key.height = unit(1, 'cm'), 
              legend.key.width = unit(1, 'cm'),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              axis.title=element_text(size=17),
              axis.title.x=element_blank(),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      #plot
      income_plot
    }
    else if (var_medianIncome() %in% c("2016", "2015", "2014", "2013", "2012", "2011", "2010")){
      
      if(var_medianIncome() == "2016") { 
        va_yr <- read.csv("data/income/va_income2016.csv")
        hamp_yr <- read.csv("data/income/hampton_income2016.csv")
      }
      else if(var_medianIncome() == "2015") { 
        va_yr <- read.csv("data/income/va_income2015.csv")
        hamp_yr <- read.csv("data/income/hampton_income2015.csv")
      }
      else if(var_medianIncome() == "2014") { 
        va_yr <- read.csv("data/income/va_income2014.csv")
        hamp_yr <- read.csv("data/income/hampton_income2014.csv")
      }
      else if(var_medianIncome() == "2013") { 
        va_yr <- read.csv("data/income/va_income2013.csv")
        hamp_yr <- read.csv("data/income/hampton_income2013.csv")
      }
      else if(var_medianIncome() == "2012") { 
        va_yr <- read.csv("data/income/va_income2012.csv")
        hamp_yr <- read.csv("data/income/hampton_income2012.csv")
      }
      else if(var_medianIncome() == "2011") { 
        va_yr <- read.csv("data/income/va_income2011.csv")
        hamp_yr <- read.csv("data/income/hampton_income2011.csv")
      }
      else if(var_medianIncome() == "2010") { 
        va_yr <- read.csv("data/income/va_income2010.csv")
        hamp_yr <- read.csv("data/income/hampton_income2010.csv")
      }
      
      va_yr <- read.csv("data/income/va_income2016.csv")
      va_yr <- va_yr[,2:6]
      #income by Race
      race_names <- c("Total", "Black")
      #median income
      va_race_income_median <- data.frame(va_yr[c(31,33), 4])
      va_race_income <- data.frame(cbind(race_names, va_race_income_median))
      colnames(va_race_income) <- c("Race", "Median Income")
      #Hampton Income
      #getting the name, variable and estimate
      hamp_income2 <- hamp_yr[,2:6]
      hamp_income3 <- hamp_income2 %>%
        group_by(NAME) %>%
        slice(c(31,33))
      variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
      hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
      #Putting them in the same dataset
      median_income <- cbind(va_race_income, hamp_race_income_median)
      median_income <- median_income[, c(2,4)]
      #having all the estimates in the same column
      median_income2 <- data.frame(median = c(median_income[,1], median_income[,2]))
      median_income2 <- mutate(median_income2, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
      median_income2 <- mutate(median_income2, demo = rep(c("Total Population", "Black Population"),2))
      colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
      #making them all numeric
      median_income2 <- transform(median_income2, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
      colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
      median_income2$Location <- factor(median_income2$Location, levels= c("Hampton Roads","Virginia"))
      #Hampton and VA graph
      income_plot <- ggplot(median_income2, aes(x=Location, y=`Median Income (US Dollars)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Median Income (US Dollars)`))), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=5)+
        theme_minimal()+ 
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
              legend.title = element_blank(),
              legend.key.height = unit(1, 'cm'), 
              legend.key.width = unit(1, 'cm'),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              axis.title=element_text(size=17),
              axis.title.x=element_blank(),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      #plot
      income_plot
    }
    
    
  })
  
  # Median Income line plots -------------------------------------------------
  
  output$medianTimeGraph <- renderPlot ({
    va_yr <- read.csv("data/income/va_income2019.csv")
    va_yr <- va_yr[2:6]
    race_names <- c("Total", "Black")
    #median income
    va_race_income_median <- data.frame(va_yr[c(81,83), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2019.csv")
    hamp_yr <- hamp_yr[2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(81,83))
    #This give us overall hampton overall and black median income
    variable <- sample(c("S1903_C03_001","S1903_C03_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income19 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income19 <- mutate(median_income19, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income19 <- mutate(median_income19, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income19) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income19 <- transform(median_income19, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income19) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income19 <- mutate(median_income19, Year = "2019")
    ############################################################################2018
    va_yr <- read.csv("data/income/va_income2018.csv")
    va_yr <- va_yr[2:6]
    race_names <- c("Total", "Black")
    #median income
    va_race_income_median <- data.frame(va_yr[c(81,83), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2018.csv")
    hamp_yr <- hamp_yr[2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(81,83))
    #This give us overall hampton overall and black median income
    variable <- sample(c("S1903_C03_001","S1903_C03_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income18 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income18 <- mutate(median_income18, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income18 <- mutate(median_income18, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income18) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income18 <- transform(median_income18, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income18) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income18 <- mutate(median_income18, Year = "2018")
    ############################################################################2017
    va_yr <- read.csv("data/income/va_income2017.csv")
    va_yr <- va_yr[2:6]
    race_names <- c("Total", "Black")
    #median income
    va_race_income_median <- data.frame(va_yr[c(81,83), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2017.csv")
    hamp_yr <- hamp_yr[2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(81,83))
    #This give us overall hampton overall and black median income
    variable <- sample(c("S1903_C03_001","S1903_C03_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income17 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income17 <- mutate(median_income17, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income17 <- mutate(median_income17, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income17) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income17 <- transform(median_income17, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income17) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income17 <- mutate(median_income17, Year = "2017")
    ###########################################################################2016
    va_yr <- read.csv("data/income/va_income2016.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2016.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income16 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income16 <- mutate(median_income16, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income16 <- mutate(median_income16, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income16) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income16 <- transform(median_income16, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income16) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income16 <- mutate(median_income16, Year = "2016")
    ###########################################################################2016
    va_yr <- read.csv("data/income/va_income2015.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2015.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income15 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income15 <- mutate(median_income15, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income15 <- mutate(median_income15, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income15) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income15 <- transform(median_income15, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income15) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income15 <- mutate(median_income15, Year = "2015")
    ###########################################################################2014
    va_yr <- read.csv("data/income/va_income2014.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2014.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income14 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income14 <- mutate(median_income14, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income14 <- mutate(median_income14, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income14) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income14 <- transform(median_income14, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income14) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income14 <- mutate(median_income14, Year = "2014")
    ###########################################################################2013
    va_yr <- read.csv("data/income/va_income2013.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2013.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income13 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income13 <- mutate(median_income13, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income13 <- mutate(median_income13, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income13) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income13 <- transform(median_income13, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income13) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income13 <- mutate(median_income13, Year = "2013")
    ############################################################################2012
    va_yr <- read.csv("data/income/va_income2012.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2012.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income12 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income12 <- mutate(median_income12, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income12 <- mutate(median_income12, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income12) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income12 <- transform(median_income12, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income12) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income12 <- mutate(median_income12, Year = "2012")
    ###########################################################################2011
    va_yr <- read.csv("data/income/va_income2011.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2011.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income11 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income11 <- mutate(median_income11, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income11 <- mutate(median_income11, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income11) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income11 <- transform(median_income11, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income11) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income11 <- mutate(median_income11, Year = "2011")
    ###########################################################################2010
    va_yr <- read.csv("data/income/va_income2010.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/income/hampton_income2010.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income10 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income10 <- mutate(median_income10, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income10 <- mutate(median_income10, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income10) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income10 <- transform(median_income10, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income10) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income10 <- mutate(median_income10, Year = "2010")
    ###############################################################################Combined
    income_years <- rbind(median_income19, median_income18, median_income17, median_income16,
                          median_income15, median_income14, median_income13, median_income12,
                          median_income11, median_income10)
    #VA line graph
    va_years <- income_years %>% filter(Location=="Virginia")
    #graph
    va_line <- ggplot(va_years, aes(x=Year, y=`Median Income (US Dollars)`, group = Demographic, color = Demographic)) + 
      geom_line(position = "identity", size =1.3) +
      theme_minimal() +
      ggtitle("Virginia") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank()) +
      theme(legend.title = element_blank(),
            axis.title.y = element_text(size=12),
            legend.text = element_text(size=12),
            axis.text = element_text(size=12)) +
      labs(y ="Median Income (US Dollars)") +
      scale_color_manual(values = c("#D55E00", "#0072B2")) +
      ylim(35000, 75000)
    
    #hamp line graph
    hamp_years <- income_years %>% filter(Location == "Hampton Roads")
    #graph
    hamp_line <- ggplot(hamp_years, aes(x=Year, y=`Median Income (US Dollars)`, group = Demographic, color = Demographic)) + 
      geom_line(position = "identity", size =1.3 ) +
      scale_color_manual(values = c("#D55E00", "#0072B2")) +
      theme_minimal() +
      ggtitle("Hampton Roads")+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            axis.title.y = element_text(size=12),
            legend.title = element_blank(),
            legend.text = element_text(size =12),
            axis.text = element_text(size=12))+
      labs(y ="Median Income (US Dollars)") +
      ylim(35000, 75000)
    
    medianTimeGraph  <- grid.arrange(hamp_line, va_line, ncol=2)
    medianTimeGraph 
  })
  # Employment By Sector ----------------------------------------------------
  var_sectorEmployment <- reactive({
    input$SectorEmploymentYearDrop
  })
  
  output$sector_plot <- renderPlotly({
    if(var_sectorEmployment() == "2019") {
      sectors2019 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2019.csv")
      colnames(sectors2019) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2019 <- sectors2019  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40)) +  scale_fill_viridis_d()
      sectors2019  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2019 , tooltip=c("x", "y", "Sector"))) 
    }
    
    else if (var_sectorEmployment() == "2018") {
      sectors2018 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2018.csv")
      colnames(sectors2018) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2018 <- sectors2018  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2018  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2018 , tooltip=c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2017") {
      sectors2017 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2017.csv")
      colnames(sectors2017) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2017 <- sectors2017  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40)) + scale_fill_viridis_d()
      sectors2017  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2017 , tooltip=c("x", "y", "Sector"))) 
    }
    
    else if (var_sectorEmployment() == "2016") {
      sectors2016 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2016.csv")
      colnames(sectors2016) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2016 <- sectors2016  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2016  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2016, tooltip=c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2015") {
      sectors2015 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2015.csv")
      colnames(sectors2015) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2015 <- sectors2015  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2015  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2015, tooltip=c("x", "y", "Sector"))) 
    }
    
    else if (var_sectorEmployment() == "2014") {
      sectors2014 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2014.csv")
      colnames(sectors2014) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2014 <- sectors2014  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2014  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2014, tooltip=c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2013") {
      sectors2013 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2013.csv")
      colnames(sectors2013) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2013 <- sectors2013  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40)) +  scale_fill_viridis_d()
      sectors2013  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2013, tooltip=c("x", "y", "Sector"))) 
    }
    
    else if (var_sectorEmployment() == "2012") {
      sectors2012 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2012.csv")
      colnames(sectors2012) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2012 <- sectors2012  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40)) +  scale_fill_viridis_d()
      sectors2012  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2012, tooltip=c("x", "y", "Sector"))) 
    }
    
    else if (var_sectorEmployment() == "2011") {
      sectors2011 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2011.csv")
      colnames(sectors2011) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2011 <- sectors2011  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2011  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2011, tooltip=c("x", "y", "Sector")))
    }
    
    else if (var_sectorEmployment() == "2010") {
      sectors2010 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2010.csv")
      colnames(sectors2010) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2010 <- sectors2010  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "") +  theme(axis.text.x = element_text(angle = 40))  +  scale_fill_viridis_d()
      sectors2010 #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2010, tooltip=c("x", "y", "Sector")))
    }
  })
  
  # Unemployment Rate -------------------------------------------------------
  
  
  var_unemploymentRate <- reactive({
    input$UnemploymentRateSlider
  })
  
  output$unemployment_plot <- renderPlotly({
    if(var_unemploymentRate() == "2019") {
      unemp_19 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2019.csv") 
      colnames(unemp_19)[2] <- "Locality"
      colnames(unemp_19)[3] <- "Demographic"
      colnames(unemp_19)[4] <- "Unemployment Rate"
      va_unemp_19 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2019.csv")
      unemployment_2019 <- unemp_19 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>% 
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_19$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2019)
      
      
    }
    
    else if(var_unemploymentRate() == "2018") {
      unemp_18 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2018.csv") 
      colnames(unemp_18)[2] <- "Locality"
      colnames(unemp_18)[3] <- "Demographic"
      colnames(unemp_18)[4] <- "Unemployment Rate"
      va_unemp_18 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2018.csv")
      unemployment_2018 <- unemp_18 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>% 
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_18$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2018)
      
      
    }
    
    else if(var_unemploymentRate() == "2017") {
      unemp_17 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2017.csv") 
      colnames(unemp_17)[2] <- "Locality"
      colnames(unemp_17)[3] <- "Demographic"
      colnames(unemp_17)[4] <- "Unemployment Rate"
      va_unemp_17 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2017.csv")
      unemployment_2017 <- unemp_17 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>% 
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_17$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2017)
      
      
    }
    
    else if(var_unemploymentRate() == "2016") {
      unemp_16 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2016.csv") 
      colnames(unemp_16)[2] <- "Locality"
      colnames(unemp_16)[3] <- "Demographic"
      colnames(unemp_16)[4] <- "Unemployment Rate"
      va_unemp_16 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2016.csv")
      unemployment_2016 <- unemp_16 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>% 
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_16$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2016)
      
      
    }
    
    else if(var_unemploymentRate() == "2015") {
      unemp_15 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2015.csv") 
      colnames(unemp_15)[2] <- "Locality"
      colnames(unemp_15)[3] <- "Demographic"
      colnames(unemp_15)[4] <- "Unemployment Rate"
      va_unemp_15 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2015.csv")
      unemployment_2015 <- unemp_15 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>% 
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_15$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2015)
      
      
    }
    
    else if(var_unemploymentRate() == "2014") {
      unemp_14 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2014.csv") 
      colnames(unemp_14)[2] <- "Locality"
      colnames(unemp_14)[3] <- "Demographic"
      colnames(unemp_14)[4] <- "Unemployment Rate"
      va_unemp_14 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2014.csv")
      unemployment_2014 <- unemp_14 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>% 
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_14$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2014)
      
      
    }
    
    else if(var_unemploymentRate() == "2013") {
      unemp_13 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2013.csv") 
      colnames(unemp_13)[2] <- "Locality"
      colnames(unemp_13)[3] <- "Demographic"
      colnames(unemp_13)[4] <- "Unemployment Rate"
      va_unemp_13 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2013.csv")
      unemployment_2013 <- unemp_13 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>% 
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_13$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2013)
      
      
    }
    
    else if(var_unemploymentRate() == "2012") {
      unemp_12 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2012.csv") 
      colnames(unemp_12)[2] <- "Locality"
      colnames(unemp_12)[3] <- "Demographic"
      colnames(unemp_12)[4] <- "Unemployment Rate"
      va_unemp_12 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2012.csv")
      unemployment_2012 <- unemp_12 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>% 
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_12$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2012)
      
      
    }
    
    else if(var_unemploymentRate() == "2011") {
      unemp_11 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2011.csv") 
      colnames(unemp_11)[2] <- "Locality"
      colnames(unemp_11)[3] <- "Demographic"
      colnames(unemp_11)[4] <- "Unemployment Rate"
      va_unemp_11 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2011.csv")
      unemployment_2011 <- unemp_11 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>% 
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_11$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2011)
      
      
    }
    
    else if(var_unemploymentRate() == "2010") {
      unemp_10 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2010.csv") 
      colnames(unemp_10)[2] <- "Locality"
      colnames(unemp_10)[3] <- "Demographic"
      colnames(unemp_10)[4] <- "Unemployment Rate"
      va_unemp_10 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2010.csv")
      unemployment_2010 <- unemp_10 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>%
        arrange(desc(Locality)) %>% 
        ggplot(aes(fill = Demographic, y = `Unemployment Rate`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_10$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2010)
      
      
    }
  })
  
  # Poverty Rates in VA and Hampton Roads-------------------------------------
  var_poverty <- reactive ({
    input$PovertyYearDrop
  })
  
  output$pov_plot <- renderPlot({
    if(var_poverty() %in% c("2019", "2018", "2017", "2016", "2015")){
      if(var_poverty() == "2019") { 
        va_pov <- read.csv("data/poverty/va_poverty2019.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2019.csv")
      }
      else if(var_poverty() == "2018") { 
        va_pov <- read.csv("data/poverty/va_poverty2018.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2018.csv")
      }
      else if(var_poverty() == "2017") { 
        va_pov <- read.csv("data/poverty/va_poverty2017.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2017.csv")
      }
      else if(var_poverty() == "2016") { 
        va_pov <- read.csv("data/poverty/va_poverty2016.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2016.csv")
      }
      else if(var_poverty() == "2015") { 
        va_pov <- read.csv("data/poverty/va_poverty2015.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2015.csv")
      }
      va_pov <- va_pov[,2:6]
      va_pct_pov <- va_pov[123,4]
      va_pct_pov_blck <- va_pov[136,4]
      va_pov_vector <- rbind(va_pct_pov, va_pct_pov_blck)
      hamp_pov <- hamp_pov[,2:6]
      #General
      hamp_total <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(1))
      hamp_total2 <- colSums(hamp_total[,4])
      hamp_pov2 <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(62))
      hamp_pov3 <- colSums(hamp_pov2[,4])
      hamp_overall_pov <- hamp_pov3/hamp_total2 *100
      #Black
      hamp_total_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(14)
      hamp_total_blck2 <- colSums(hamp_total_blck[,4]) 
      hamp_pov_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(75)
      hamp_pov_blck2 <- colSums(hamp_pov_blck[,4])
      hamp_blck_overall_pov <- hamp_pov_blck2/hamp_total_blck2*100
      hamp_pov_vector <- rbind(hamp_overall_pov, hamp_blck_overall_pov)
      #combing hampton and VA
      pov_pct <- data.frame(va_pov_vector, hamp_pov_vector)
      pov_pct2 <- data.frame(Ratio = unlist(pov_pct, use.names=FALSE))
      pov_pct2 <- mutate(pov_pct2, Location = c("Virginia", "Virginia", "Hampton Roads", "Hampton Roads"))
      pov_pct2 <- mutate(pov_pct2, Demographic = c("Total Population", "Black Population", "Total Population",
                                                   "Black Population"))
      colnames(pov_pct2) <- c("Percentage (%)", "Location", "Demographic")
      pov_pct2$Location <- factor(pov_pct2$Location, levels = c("Hampton Roads", "Virginia"))
      #Graph for just hampton roads and VA
      pov_plot <- ggplot(pov_pct2, aes(x=Location, y=`Percentage (%)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=5)+
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
              legend.title = element_blank(),
              legend.key.height = unit(1, 'cm'), 
              legend.key.width = unit(1, 'cm'),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              axis.title=element_text(size=17),
              axis.title.x=element_blank(),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        scale_fill_manual(values=c("#D55E00","#0072B2")) 
      #plot
      pov_plot
    }
    #when table changes
    else if (var_poverty() %in% c("2014", "2013", "2012")){
      if(var_poverty() == "2014") { 
        va_pov <- read.csv("data/poverty/va_poverty2014.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2014.csv")
      }
      else if(var_poverty() == "2013") { 
        va_pov <- read.csv("data/poverty/va_poverty2013.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2013.csv")
      }
      else if(var_poverty() == "2012") { 
        va_pov <- read.csv("data/poverty/va_poverty2012.csv")
        hamp_pov <- read.csv("data/poverty/hamp_poverty2012.csv")
      }
      va_pov <- va_pov[,2:6]
      #General
      va_pct_pov <- va_pov[93,4]
      #Black
      va_pct_pov_blck <- va_pov[102,4]
      va_pov_vector <- rbind(va_pct_pov, va_pct_pov_blck)
      #Hampton Roads
      hamp_pov <- hamp_pov[,2:6]
      #General
      hamp_total <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(1))
      hamp_total2 <- colSums(hamp_total[,4])
      hamp_pov2 <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(47))
      hamp_pov3 <- colSums(hamp_pov2[,4])
      hamp_overall_pov <- hamp_pov3/hamp_total2 *100
      #Black
      hamp_total_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(10)
      hamp_total_blck2 <- colSums(hamp_total_blck[,4]) 
      hamp_pov_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(56)
      hamp_pov_blck2 <- colSums(hamp_pov_blck[,4])
      hamp_blck_overall_pov <- hamp_pov_blck2/hamp_total_blck2*100
      hamp_pov_vector <- rbind(hamp_overall_pov, hamp_blck_overall_pov)
      #combing hampton and VA
      pov_pct <- data.frame(va_pov_vector, hamp_pov_vector)
      pov_pct2 <- data.frame(Ratio = unlist(pov_pct, use.names=FALSE))
      pov_pct2 <- mutate(pov_pct2, Location = c(rep("Virginia",2), rep("Hampton Roads",2)))
      pov_pct2 <- mutate(pov_pct2, Demographic = rep(c("Total Population", "Black Population"),2))
      colnames(pov_pct2) <- c("Percentage (%)", "Location", "Demographic")
      pov_pct2$Location <- factor(pov_pct2$Location, levels = c("Hampton Roads", "Virginia"))
      #Graph for just hampton roads and VA
      pov_plot <- ggplot(pov_pct2, aes(x=Location, y=`Percentage (%)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=5)+
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
              legend.title = element_blank(),
              legend.key.height = unit(1, 'cm'), 
              legend.key.width = unit(1, 'cm'),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              axis.title=element_text(size=17),
              axis.title.x=element_blank(),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        scale_fill_manual(values=c("#D55E00","#0072B2"))
      #plot
      pov_plot
    }
    
    
  })
  
  # Hampton counties poverty -------------------------------------------------------------
  var_povertyCount <- reactive ({
    input$PovertyCountYearDrop
  })
  
  output$counties_pov <- renderPlotly({
    if( var_povertyCount() %in% c("2019", "2018", "2017", "2016", "2015")){
      if( var_povertyCount() == "2019") { 
        hamp_pov <- read.csv("data/poverty/hamp_poverty2019.csv")
      }
      else if( var_povertyCount() == "2018") { 
        hamp_pov <- read.csv("data/poverty/hamp_poverty2018.csv")
      }
      else if( var_povertyCount() == "2017") { 
        hamp_pov <- read.csv("data/poverty/hamp_poverty2017.csv")
      }
      else if( var_povertyCount() == "2016") { 
        hamp_pov <- read.csv("data/poverty/hamp_poverty2016.csv")
      }
      else if( var_povertyCount() == "2015") { 
        hamp_pov <- read.csv("data/poverty/hamp_poverty2015.csv")
      }
      hamp_pov <- hamp_pov[,2:6]
      hamp_pov_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(123)
      hamp_pov_blck_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(136)
      hamp_pov_tbl %>% ungroup()
      hamp_pov_blck_tbl %>% ungroup()
      hamp_pctG <- hamp_pov_tbl[,4]
      hamp_pctB <- hamp_pov_blck_tbl[,4] 
      hamp_comb <- rbind(hamp_pctG, hamp_pctB)
      colnames(hamp_comb) <- "Ratio"
      hamp_comb <- mutate(hamp_comb, Location =  rep(c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                       "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                       "Suffolk", "Virginia Beach", "Williamsburg", "York"),2))
      hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                                     rep("Black Population",16)))
      colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
      hamp_comb <- hamp_comb %>% filter(hamp_comb[,2] != "Poquoson")
      #Graph 
      counties_pov <-  ggplot(hamp_comb, aes(Location, y=`Percentage (%)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        #geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
        #position = position_dodge(0.9), size=3)+
        theme_minimal() +
        scale_fill_manual(values=c("#D55E00","#0072B2")) +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'), 
              #legend.key.height = unit(0.5, 'cm'), 
              #legend.key.width = unit(0.5, 'cm'), 
              legend.title = element_blank(),
              #legend.text = element_text(size=14),
              #axis.text=element_text(size=15),
              #axis.text.x = element_text(size=8, face="bold"),
              #axis.title=element_text(size=17),
              axis.title.x=element_blank()) +
        theme(axis.text.x = element_text(angle=40, vjust=0.95, hjust=1)) +
        labs(caption = "Source: ACS 5 Year Estimate Table S1701")
      #plot
      counties_pov <- ggplotly(counties_pov)
    }
    #when table changes
    else if (var_povertyCount() %in% c("2014", "2013", "2012")){
      if( var_povertyCount() == "2014") { 
        hamp_pov <- read.csv("data/poverty/hamp_poverty2014.csv")
      }
      else if( var_povertyCount() == "2013") { 
        hamp_pov <- read.csv("data/poverty/hamp_poverty2013.csv")
      }
      else if( var_povertyCount() == "2012") { 
        hamp_pov <- read.csv("data/poverty/hamp_poverty2012.csv")
      }
      hamp_pov <- hamp_pov[,2:6]
      hamp_pov_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(93)
      hamp_pov_blck_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(102)
      hamp_pov_tbl %>% ungroup()
      hamp_pov_blck_tbl %>% ungroup()
      hamp_pctG <- hamp_pov_tbl[,4]
      hamp_pctB <- hamp_pov_blck_tbl[,4] 
      hamp_comb <- rbind(hamp_pctG, hamp_pctB)
      colnames(hamp_comb) <- "Ratio"
      hamp_comb <- mutate(hamp_comb, Location = c(rep(c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                        "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                        "Suffolk", "Virginia Beach", "Williamsburg", "York"),2)))
      hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                                     rep("Black Population",16)))
      colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
      hamp_comb <- hamp_comb %>% filter(hamp_comb[,2] != "Poquoson")
      #Graph 
      
      counties_pov <-  ggplot(hamp_comb, aes(Location, y=`Percentage (%)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        #geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
        #position = position_dodge(0.9), size=3)+
        theme_minimal() +
        scale_fill_manual(values=c("#D55E00","#0072B2")) +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'), 
              legend.title = element_blank(),
              #legend.text = element_text(size=14),
              # axis.text=element_text(size=15),
              #axis.text.x = element_text(size=10, face="bold"),
              #axis.title=element_text(size=17),
              axis.title.x=element_blank())+
        theme(axis.text.x = element_text(angle=40, vjust=0.95, hjust=1)) +
        labs(caption = "Source: ACS 5 Year Estimate Table S1701")
      
      #plot
      counties_pov <- ggplotly(counties_pov)
    }
    
    
  })
  
  
  
  # Uninsured Population ----------------------------------------------------
  
  var_uninsuredpct <- reactive({
    input$UninsuredPctSlider
  })
  
  output$uninsured_plot <- renderPlotly({
    if(var_uninsuredpct() == "2019") {
      unins_19 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2019.csv")
      colnames(unins_19)[1] <- "Locality"
      colnames(unins_19)[2] <- "Demographic"
      colnames(unins_19)[3] <- "Percent Uninsured"
      unins_19 <- unins_19 %>% 
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2019 <- unins_19 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>% 
        ggplot(aes(fill = Demographic, y = `Percent Uninsured`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2019)
      
    }
    
    else if(var_uninsuredpct() == "2018") {
      unins_18 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2018.csv")
      colnames(unins_18)[1] <- "Locality"
      colnames(unins_18)[2] <- "Demographic"
      colnames(unins_18)[3] <- "Percent Uninsured"
      unins_18 <- unins_18 %>% 
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2018 <- unins_18 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>% 
        ggplot(aes(fill = Demographic, y = `Percent Uninsured`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2018)
    }
    
    else if(var_uninsuredpct() == "2017") {
      unins_17 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2017.csv")
      colnames(unins_17)[1] <- "Locality"
      colnames(unins_17)[2] <- "Demographic"
      colnames(unins_17)[3] <- "Percent Uninsured"
      unins_17 <- unins_17 %>% 
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2017 <- unins_17 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>% 
        ggplot(aes(fill = Demographic, y = `Percent Uninsured`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2017)
    }
    
    else if(var_uninsuredpct() == "2016") {
      unins_16 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2016.csv")
      colnames(unins_16)[1] <- "Locality"
      colnames(unins_16)[2] <- "Demographic"
      colnames(unins_16)[3] <- "Percent Uninsured"
      unins_16 <- unins_16 %>% 
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2016 <- unins_16 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>% 
        ggplot(aes(fill = Demographic, y = `Percent Uninsured`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2016)
    }
    
    else if(var_uninsuredpct() == "2015") {
      unins_15 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2015.csv")
      colnames(unins_15)[1] <- "Locality"
      colnames(unins_15)[2] <- "Demographic"
      colnames(unins_15)[3] <- "Percent Uninsured"
      unins_15 <- unins_15 %>% 
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2015 <- unins_15 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>% 
        ggplot(aes(fill = Demographic, y = `Percent Uninsured`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2015)
    }
    
    else if(var_uninsuredpct() == "2014") {
      unins_14 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2014.csv")
      colnames(unins_14)[1] <- "Locality"
      colnames(unins_14)[2] <- "Demographic"
      colnames(unins_14)[3] <- "Percent Uninsured"
      unins_14 <- unins_14 %>% 
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2014 <- unins_14 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>% 
        ggplot(aes(fill = Demographic, y = `Percent Uninsured`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2014)
    }
    
    else if(var_uninsuredpct() == "2013") {
      unins_13 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2013.csv")
      colnames(unins_13)[1] <- "Locality"
      colnames(unins_13)[2] <- "Demographic"
      colnames(unins_13)[3] <- "Percent Uninsured"
      unins_13 <- unins_13 %>% 
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2013 <- unins_13 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>% 
        ggplot(aes(fill = Demographic, y = `Percent Uninsured`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2013)
    }
    
    else if(var_uninsuredpct() == "2012") {
      unins_12 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2012.csv")
      
      colnames(unins_12)[1] <- "Locality"
      colnames(unins_12)[2] <- "Demographic"
      colnames(unins_12)[3] <- "Percent Uninsured"
      unins_12 <- unins_12 %>% 
        mutate(`Percent Uninsured` = round(`Percent Uninsured`, 2))
      
      uninsured_2012 <- unins_12 %>% 
        mutate(Locality = str_remove(Locality, "County, Virginia")) %>% 
        mutate(Locality = str_remove(Locality, "city, Virginia")) %>% 
        ggplot(aes(fill = Demographic, y = `Percent Uninsured`, x = Locality)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2012)
    }
    
  })
  
  
  # Veteran Status ----------------------------------------------------------
  var_veteran <- reactive({
    input$VeteranSlider
  })
  
  output$veteran_map <- renderLeaflet({
    if(var_veteran() == "2019") {
      vet_19 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2019.rds")
      military_bases <- read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_19$Percent, reverse = TRUE)
      veteran_19 <- vet_19 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2018") {
      vet_18 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2018.rds")
      military_bases <- read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_18$Percent, reverse = TRUE)
      veteran_18 <- vet_18 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2017") {
      vet_17 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2017.rds")
      military_bases <- read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_17$Percent, reverse = TRUE)
      veteran_17 <- vet_17 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2016") {
      vet_16 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2016.rds")
      military_bases <- read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <- colorNumeric(palette = "viridis",domain = vet_16$Percent, reverse = TRUE)
      veteran_16 <- vet_16 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2015") {
      vet_15 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2015.rds")
      military_bases <- read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_15$Percent, reverse = TRUE)
      veteran_15 <- vet_15 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2014") {
      vet_14 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2014.rds")
      military_bases <- read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_14$Percent, reverse = TRUE)
      veteran_14 <- vet_14 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2013") {
      vet_13 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2013.rds")
      military_bases <- read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_13$Percent, reverse = TRUE)
      veteran_13 <- vet_13 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2012") {
      vet_12 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2012.rds")
      military_bases <- read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")  
      pal <- colorNumeric(palette = "viridis", domain = vet_12$Percent, reverse = TRUE)
      veteran_12 <- vet_12 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2011") {
      vet_11 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2011.rds")
      military_bases <- read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_11$Percent, reverse = TRUE)
      veteran_11 <- vet_11 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2010") {
      vet_10 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2010.rds")
      military_bases <- read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_10$Percent, reverse = TRUE)
      veteran_10 <- vet_10 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
  })
  
  
  # Home Ownership Map -------------------------------------------------------
  var_hmown <- reactive({
    input$HomeOwnSlider
  })
  
  
  output$homeownership_map <- renderLeaflet({
    b_hm_19 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2019.rds")
    tot_hm_19 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2019.rds")
    all_hm_data <- read_rds("data/TableS2502FiveYearEstimates/allhomedata.rds")
    colnames(all_hm_data)[2] <- "Demographic"
    
    pick_n <- function(Locality){
      dataFiltered <- filter(all_hm_data, NAME == Locality)
      
      hm_line <- ggplot(dataFiltered, aes(x = Year, y = Percent, color = Demographic, group = Demographic)) +
        geom_line(position = "identity") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_discrete(name = "", labels = c("Black Home Owners", "White Home Owners"))+
        scale_fill_manual(values = c("#D55E00", "#0072B2")) +
        theme(legend.position = "bottom") +
        labs(title = Locality)
      
      #ggplotly(hm_line)
    }
    
    r <- lapply(1:length(unique(b_hm_19$NAME)), function(i){
      pick_n(b_hm_19$NAME[i])
    })
    
    pal <- colorNumeric(palette = "viridis", domain = b_hm_19$Percent, reverse = TRUE)
    b_hmown_leaf_19 <- b_hm_19 %>%
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>% 
      addPolygons(data = b_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                  highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                  label = ~paste0(NAME,  " Black Homeowners: ", Percent, "%"), group = "Black Home Owners",
                  popup = popupGraph(r)) %>% 
      addPolygons(data = tot_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                  highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                  label = ~paste0(NAME,  " Total Homeowners: ", Percent, "%"), group = "Total Home Owners",
                  popup = popupGraph(r)) %>% 
      addLayersControl(
        baseGroups = c("Total Home Owners"),
        overlayGroups = c("Black Home Owners"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup("Black Home Owners") %>% 
      addLegend("topleft",
                pal = pal,
                values = ~ Percent,
                title = "Home Owners",
                labFormat = labelFormat(suffix = "%"),
                opacity = 1)
  })
  
  
  # Household Wellbeing -----------------------------------------------------
  var_well <- reactive({
    input$select_wellbeing
  })
  
  output$wellbeing_maps <- renderLeaflet({
    if(var_well() == "Percent of Black Children under 18 in Female Head of Household") {
      fml <- read_rds("data/fml.rds")
      fml <- fml %>% 
        na.omit(fml)
      colnames(fml)[4] <- "Percent"
      fempal <- colorNumeric(palette = "viridis", domain = fml$Percent, reverse = TRUE)
      
      fml_map <- fml %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ fempal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", variable, ": ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = fempal,
                  values = ~ Percent,
                  title = "Female HOH",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_well() == "Percent of Black Households Receiving Foodstamps/SNAP Benefits"){
      
      foodstmp <- read_rds("data/foodstmp.rds")
      colnames(foodstmp)[4] <- "Percent"
      foodpal <- colorNumeric(palette = "viridis", domain = foodstmp$Percent, reverse = TRUE)
      
      foodstmp_map <- foodstmp %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ foodpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", variable, ": ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = foodpal,
                  values = ~ Percent,
                  title = "Food Stamps",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_well() == "Percent of Black County Migration"){
      
      mobile <- read_rds("data/mobile.rds")
      colnames(mobile)[4] <- "Percent"
      colnames(mobile)[3] <- "Intra-County Migration"
      mobpal <- colorNumeric(palette = "viridis", domain = mobile$Percent, reverse = TRUE)
      
      mobile_map <- mobile %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ mobpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, "", "Intra-County Migration: ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = mobpal,
                  values = ~ Percent,
                  title = "County Migration",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
      
    }
    
    else if(var_well() == "Percent of Black Grandparents who are Guardians"){
      
      grand <- read_rds("data/grand.rds")
      colnames(grand)[4] <- "Percent"
      colnames(grand)[3] <- "Grandparent Guardian"
      grandpal <- colorNumeric(palette = "viridis", domain = grand$Percent, reverse = TRUE)
      
      grand_map <- grand %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ grandpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", "Grandparent Guardian: ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = grandpal,
                  values = ~ Percent,
                  title = "Grandparent Guardian",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_well() == "Percent of Married Black Population 15 years and over"){
      married <- read_rds("data/married.rds")
      colnames(married)[4] <- "Percent"
      marriedpal <- colorNumeric(palette = "viridis", domain = married$Percent, reverse = TRUE)
      
      married_map <- married %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ marriedpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", "Married: ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = marriedpal,
                  values = ~ Percent,
                  title = "Married",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_well() == "Percent of Black Population that uses car/truck/van to get to work"){
      priv_trans <- read_rds("data/priv_trans.rds")
      priv_trans <- priv_trans %>% 
        na.omit(priv_trans)
      colnames(priv_trans)[4] <- "Percent"
      colnames(priv_trans)[3] <- "Private Transport"
      priv_transpal <- colorNumeric(palette = "viridis", domain = priv_trans$Percent, reverse = TRUE)
      
      priv_trans_map <- priv_trans %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ priv_transpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", "Private Transport: ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = priv_transpal,
                  values = ~ Percent,
                  title = "Private Transportation",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_well() == "Percent of Black Population that uses public transportation to get to work"){
      pub_trans <- read_rds("data/pub_trans.rds")
      pub_trans <- pub_trans %>% 
        na.omit(pub_trans)
      colnames(pub_trans)[4] <- "Percent"
      colnames(pub_trans)[3] <- "Public Transport"
      pub_transpal <- colorNumeric(palette = "viridis", domain = pub_trans$Percent, reverse = TRUE)
      
      pub_trans_map <- pub_trans %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pub_transpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", "Public Transport: ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = pub_transpal,
                  values = ~ Percent,
                  title = "Public Transportation",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
      
    }
    
    else if(var_well() == "Percent of Black Households with a computer with broadband internet"){
      compin <- read_rds("data/compin.rds")
      colnames(compin)[4] <- "Percent"
      colnames(compin)[3] <- "Computer and Internet"
      compinpal <- colorNumeric(palette = "viridis", domain = compin$Percent, reverse = TRUE)
      
      compin_map <- compin %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ compinpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", "Computer and Internet: ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = compinpal,
                  values = ~ Percent,
                  title = "Computer with Internet Access",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_well() == "Percent of Black Households without a computer"){
      nocomp <- read_rds("data/nocomp.rds")
      colnames(nocomp)[4] <- "Percent"
      colnames(nocomp)[3] <- "No Computer"
      nocomppal <- colorNumeric(palette = "viridis", domain = nocomp$Percent, reverse = TRUE)
      
      nocomp_map <- nocomp %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ nocomppal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
                    label = ~paste0(NAME, " - ", "No Computer: ", Percent, "%")) %>% 
        addLegend("topleft",
                  pal = nocomppal,
                  values = ~ Percent,
                  title = "No Computer Access",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
  })
  
  var_welltext <- reactive({
    input$select_wellbeing
  })
  
  output$description_text <- renderText({
    if(var_welltext() == "Percent of Black Children under 18 in Female Head of Household"){
      "Percentage of Black Children under the age of 18 that live in a female-headed household. We included this indicator as research has shown that female-headed households have
    a greater risk of poverty and are more likely to be food-insecure. In Hampton Roads, regardless of location, majority of Black households with children under 18 have a female as the head.  For 7 of the 11 areas, over 50% of Black households 
    for which data is available, is led by a female. This may suggest some family instability for half of the black children in the Hampton Roads region."
    }
    
    else if(var_welltext() == "Percent of Black Households Receiving Foodstamps/SNAP Benefits"){
      "The percentage of Black Households receiving Food stamps or SNAP Benefits across the localities of Hampton Roads.
     More than half (54.3%) of the total Black population in Hampton Roads receive one of these welfare programs. There are also considerable 
    variabilities across localities - in Franklin, approximately 92% received food stamps/SNAP benefits, in contrast to 12% in Gloucester 
    (the lowest number of recipients in Hampton Roads). "
    }
    
    else if(var_welltext() == "Percent of Black County Migration"){
      "Percent of Black population that moved within state but from a different county. There seems to be low mobility across counties and cities in the Hampton Roads region.
    Migration rates ranged from as low as 0.4% to a high of 14.7%. "
    }
    
    else if(var_welltext() == "Percent of Black Grandparents who are Guardians"){
      "Percent distribution of Black grandparents who live with grandchildren who are responsible for said grandchildren.
    Grandparents becoming principal guardians for their grandchildren suggest economic distress for families, as such, we included this indicator in our analysis.
    There are some differences in this distribution across the cities and counties in Hampton Roads.  For example, of those Black grandparents who live 
    with their own grandchildren, 80% of them are responsible for their grandchildren in Franklin, whereas in Gloucester, the rate is a low 4.8%."   
      
    }
    
    else if(var_welltext() == "Percent of Married Black Population 15 years and over"){
      "The percentage of the Black population 15 years and over who are married. The literature shows that married households tend to be less impoverished and are more 
    economically stable and stable. Except for York, Gloucester, Chesapeake (about 50%), there is a low marriage rate among the Black population. Marriage rates range from as low as 
    20% (Norfolk) to 51% (Chesapeake), with the average rate being around 35%. "
      
    }
    
    else if(var_welltext() == "Percent of Black Population that uses car/truck/van to get to work"){
      "Percent distribution of the Black population that uses a car/truck/van to get to work. We included this indicator as reliable transportation
     can improve economic efficiency (due to access to more jobs, childcare facilities, etc.) and even access to healthcare (e.g., appointments, emergency care). 
     Approximately 50% of the Black population uses private transportation to get to work  in Portsmouth. This is relatively high compared to Gloucester,
    where only 8.4% uses similar transportation for work. " 
    }
    
    else if(var_welltext() == "Percent of Black Population that uses public transportation to get to work"){
      "Percent distribution of the Black population that uses public transportation to work. We included this indicator as public transportation at times may be unreliable,
    increasing economic inequality. Majority of the Black population in Hampton Roads uses public transportation in order to get work. However, there are some differences
    across localities - a high of 94% in Southampton to a low of 14% in Gloucester."
    }
    
    else if(var_welltext() == "Percent of Black Households with a computer with broadband internet"){
      "Percent of the Black population with a computer that has a broadband internet subscription. The internet and digital devices have become an
   essential component of everyday life. Such access can exacerbate educational and economic inequality, 
   thus it is important to understand how the Black community in Hampton Roads engages in these technologies. Despite the rapid usage of technology, 
    there are some significant disparities with the Hampton Roads region. 
    For instance, over 90% of Black households had a computer with a broadband internet subscription in York compared to a low of 66% in James City."
    }
    
    else if(var_welltext() == "Percent of Black Households without a computer"){
      "Percent of the Black population without a computer. The internet and digital devices have become an
   essential component of everyday life. Such access can exacerbate educational and economic inequality, 
   thus it is important to understand how the Black community in Hampton Roads engages in these technologies. While the percent of Black households in Hampton Roads 
   without a computer is low, the percentage is surprising given the rapid usage of technology. In 2019, the average White household without a computer was 6%, however, for 
   the same period 11 of the 16 localities had a greater percentage of Black households without access - with a high of 76%, 39%, and 16% in Poquoson, Mathews, and 
   Franklin, respectively."
    }
  })
  
  
  # Ranked Graphs -----------------------------------------------------------
  
  dat <- read_csv("data/countyrankings.csv")  
  
  var_rank <- reactive({
    input$select_rank
  })
  
  output$ranked_chart <- renderPlotly({
    if(var_rank() == "Black Median Income") {
      ranked_median <- dat %>% 
        ggplot(data = ., aes(x = reorder(Counties, med.income),
                             y = med.income, fill = Counties)) + 
        geom_bar(stat = "identity", width = 0.7,
                 show.legend = FALSE) +
        coord_flip() + 
        labs(title = "Black Median Income",
             x= "", y = "Median Income") + 
        theme_bw()
      
      ggplotly(ranked_median)
    }
    
    else if(var_rank() == "Black Poverty rate") {
      ranked_pov <- dat %>% 
        ggplot(data = ., aes(x = reorder(Counties, 
                                         desc(pov.rate)),
                             y = pov.rate, fill = Counties)) + 
        geom_bar(stat = "identity", width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Black Poverty rate",
             x= "", y = "%") + 
        coord_flip() + 
        theme_bw()
      
      ggplotly(ranked_pov)
    }
    
    else if(var_rank() == "Black Unemployment rate"){
      ranked_unemp <- dat %>% 
        ggplot(data = ., aes(x = reorder(Counties, 
                                         desc(unemp)),
                             y = unemp, fill = Counties)) + 
        geom_bar(stat = "identity", width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Black Unemployment rate",
             x= "", y = "%") + 
        coord_flip() + 
        theme_bw()
      
      ggplotly(ranked_unemp)
    }
    
    else if(var_rank() == "Percent of Uninsured (Health) Black population"){
      ranked_unins <- dat %>% 
        ggplot(data = ., aes(x = reorder(Counties, 
                                         desc(uninsured)),
                             y = uninsured, fill = Counties)) + 
        geom_bar(stat = "identity", width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percent of Uninsured (Health) Black population",
             x= "", y = "%") + 
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_unins)
    }
    
    else if(var_rank() == "Percentage of Black HomeOwners") {
      ranked_hmown <- dat %>% 
        ggplot(data = ., aes(x = reorder(Counties, 
                                         (homeownership)),
                             y = homeownership, fill = Counties)) + 
        geom_bar(stat = "identity", width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black HomeOwners",
             x= "", y = "%") + 
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_hmown)
    }
    
    else if(var_rank() == "Percentage of Black Students Suspended") {
      ranked_sus <- dat %>% 
        ggplot(data = ., aes(x = reorder(Counties, 
                                         desc(suspension)),
                             y = suspension, fill = Counties)) + 
        geom_bar(stat = "identity", width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black Students Suspended",
             x= "", y = "%") + 
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_sus)
    }
    
    else if(var_rank() == "Percentage of Black Students 25 yrs and over that have Bachelor's Degree or Higher") {
      ranked_bach <- dat %>% 
        ggplot(data = ., aes(x = reorder(Counties, 
                                         (bachelor)),
                             y = bachelor, fill = Counties)) + 
        geom_bar(stat = "identity", width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black Students 25 yrs and over that have Bachelor's Degree or Higher",
             x= "", y = "%") + 
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_bach)
    }
    
    else if(var_rank() == "Percentage of Black Children under 18 in Female Headed Household") {
      ranked_fhoh <- dat %>% 
        ggplot(data = ., aes(x = reorder(Counties, 
                                         desc(femalehead)),
                             y = femalehead, fill = Counties)) + 
        geom_bar(stat = "identity", width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black Children under 18 in Female Headed Household",
             x= "", y = "%") + 
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_fhoh)
    }
    
    else if(var_rank() == "Percent of Black Households without a computer") {
      ranked_comp <- dat %>% 
        ggplot(data = ., aes(x = reorder(Counties, 
                                         desc(nocomp)),
                             y = nocomp, fill = Counties)) + 
        geom_bar(stat = "identity", width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black Households without a computer",
             x= "", y = "%") + 
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_comp)
    }
    
    else if(var_rank() == "Percentage of Black Population that uses car/truck/van for work") {
      ranked_car <- dat %>% 
        ggplot(data = ., aes(x = reorder(Counties, 
                                         (car)),
                             y = car, fill = Counties)) + 
        geom_bar(stat = "identity", width = 0.7,
                 show.legend = FALSE) +
        labs(title = "Percentage of Black Population that uses car/truck/van for work",
             x= "", y = "%") + 
        coord_flip() +
        theme_bw()
      
      ggplotly(ranked_car)
    }
  })
  
  
}


shinyApp(ui = ui, server = server)
