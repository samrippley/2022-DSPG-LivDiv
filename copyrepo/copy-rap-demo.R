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
# Read in Demographic Data ----------------------------------------------------------
#age
rappk_ageGroups <- read.csv("data/TableB01001FiveYearEstimates/rappkAgeGroups.csv")
va_ageGroups <- read.csv("data/TableB01001FiveYearEstimates/vaAgeGroups.csv")
counties_median_age<- read.csv("data/TableB01001FiveYearEstimates/countyMedianAge.csv")
sub_median_age<- read.csv("data/TableB01001FiveYearEstimates/subdivisionMedianAge.csv")
county_dep<- read.csv("data/TableB01001FiveYearEstimates/countyAgeDependency.csv")
sub_dep<- read.csv("data/TableB01001FiveYearEstimates/subdivisionAgeDependency.csv")
va_rappk_dep<- read.csv("data/TableB01001FiveYearEstimates/rappkAgeDependency.csv")
rappage_timeseries <- readRDS("data/rapp_age_time_series.Rda")
agetimeseries <- readRDS("data/district_age_time_series.Rds")
#income
income2010_2019 <- readRDS("data/income2010_2019.Rda")
#household characteristics
householdSize <- read.csv("data/TableS2501FiveYearEstimates/householdSize.csv")
own <- read.csv("data/TableS2501FiveYearEstimates/ownerOccupied.csv")
rent <- read.csv("data/TableS2501FiveYearEstimates/renterOccupied.csv")
rappk_veh <- read.csv("data/TableS2501FiveYearEstimates/vehiclesHousehold.csv")
county_veh <- read.csv("data/TableS2501FiveYearEstimates/vehiclesHouseholdCounty.csv")
#Broadband
intByIncome <- read.csv("data/TableS2801FiveYearEstimates/internetIncome.csv")
compDist <- read.csv("data/TableS2801FiveYearEstimates/districtComputers.csv")
intDist <- read.csv("data/TableS2801FiveYearEstimates/districtInternet.csv")
#race
race_district <- readRDS("data/race_district.Rds")

#housing
housing2010_2019 <- readRDS("data/housing_over_time.Rda")
housing2010_2019_by_district <- readRDS("data/housing2010_2019_by_district.Rds")
#population
population2010_2019 <- readRDS("data/population2010_2019.Rds")
#education
edu2019 <- readRDS("data/edu2019.Rds")


# Read in Housing Data ----------------------------------------------------------

# Read in Traffic Data ----------------------------------------------------------
traffic_2010 <- read_excel("data/traffic/traffic_data_2010.xls")
traffic_2011 <- read_excel("data/traffic/traffic_data_2011.xls")
traffic_2012 <- read_excel("data/traffic/traffic_data_2012.xls")
traffic_2013 <- read_excel("data/traffic/traffic_data_2013.xls")
traffic_2014 <- read_excel("data/traffic/traffic_data_2014.xls")
traffic_2015 <- read_excel("data/traffic/traffic_data_2015.xls")
traffic_2016 <- read_excel("data/traffic/traffic_data_2016.xls")
traffic_2017 <- read_excel("data/traffic/traffic_data_2017.xls")
traffic_2018 <- read_excel("data/traffic/traffic_data_2018.xls")
traffic_2019 <- read_excel("data/traffic/traffic_data_2019.xls")
traffic_2020 <- read_excel("data/traffic/traffic_data_2020.xlsx") 
coordinates <- read_excel("data/traffic/traffic_coordinates.xlsx")

# Read in Service Data ----------------------------------------------------------
#Save
#saveRDS(acs5, file = paste0('acs5','.RDS'))
acs5 <- readRDS("data/Isochrone/acs5.RDS")

#saveRDS(acs_RPK, file = paste0('acs_RPK','.RDS'))
acs_RPK <- readRDS("data/Isochrone/acs_RPK.RDS")

#saveRDS(acs_RPK_dist, file = paste0('acs_RPK_dist','.RDS'))
acs_RPK_dist <- readRDS("data/Isochrone/acs_RPK_dist.RDS")

#saveRDS(acs_RPK_area, file = paste0('acs_RPK_area','.RDS'))
acs_RPK_area <- readRDS("data/Isochrone/acs_RPK_area.RDS")

#saveRDS(va_sf, file = paste0('va_sf','.RDS'))
va_sf <- readRDS("data/Isochrone/va_sf.RDS")

#saveRDS(RPK_outline, file = paste0('RPK_outline','.RDS'))
RPK_outline <- readRDS("data/Isochrone/RPK_outline.RDS")

#saveRDS(RPK_area_outline, file = paste0('RPK_area_outline','.RDS'))
RPK_area_outline <- readRDS("data/Isochrone/RPK_area_outline.RDS")

#saveRDS(RPK_dist_outline, file = paste0('RPK_dist_outline','.RDS'))
RPK_dist_outline <- readRDS("data/Isochrone/RPK_dist_outline.RDS")
################################################################################################################################################################

#Centroid of the County
#CenterPop_county <- read.csv("data/Isochrone/CenPop2010_Mean_CO51.txt", header =T)

#centerPop_RPK <- CenterPop_county %>% filter(COUNAME %in% c("Rappahannock")
#) #%>% filter(COUNTYFP == 157)
#saveRDS(centerPop_RPK, file = paste0('centerPop_RPK','.RDS'))
centerPop_RPK <- readRDS("data/Isochrone/centerPop_RPK.RDS")

#saveRDS(centerPop_RPK_dist, file = paste0('centerPop_RPK_dist','.RDS'))
centerPop_RPK_dist <- readRDS("data/Isochrone/centerPop_RPK_dist.RDS")



pop_centroid_RPK_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_iso_15.RDS")
pop_centroid_RPK_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_iso_30.RDS")


#Centroid of the Districts
#Obtained from:
#https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.html

#CenterPop_dist <- read.csv("data/Isochrone/CenPop2010_Mean_CO51_157.txt", header =T)

#centerPop_RPK_dist <- CenterPop_dist %>%  filter(COUNTYFP == 157)
#%>% filter(COUNAME %in% c("Rappahannock")
#) 






#write.csv(centerPop_RPK_dist, file="centerPop_RPK_dist.csv")
CenterPop_dist1 <- read_excel("data/Isochrone/centerPop_RPK_dist.xlsx", sheet=1)
colnames(CenterPop_dist1)[8] <- "LONGITUDE"
colnames(CenterPop_dist1)[7] <- "LATITUDE"

CenterPop_dist2 <- read_excel("data/Isochrone/centerPop_RPK_dist.xlsx", sheet=2)
colnames(CenterPop_dist2)[8] <- "LONGITUDE"
colnames(CenterPop_dist2)[7] <- "LATITUDE"

CenterPop_dist3 <- read_excel("data/Isochrone/centerPop_RPK_dist.xlsx", sheet=3)
colnames(CenterPop_dist3)[8] <- "LONGITUDE"
colnames(CenterPop_dist3)[7] <- "LATITUDE"

CenterPop_dist4 <- read_excel("data/Isochrone/centerPop_RPK_dist.xlsx", sheet=4)
colnames(CenterPop_dist4)[8] <- "LONGITUDE"
colnames(CenterPop_dist4)[7] <- "LATITUDE"

CenterPop_dist5 <- read_excel("data/Isochrone/centerPop_RPK_dist.xlsx", sheet=5)
colnames(CenterPop_dist5)[8] <- "LONGITUDE"
colnames(CenterPop_dist5)[7] <- "LATITUDE"


################################################################################################################################################################
#travel time, I did this once to generate center RDS file
#to get the api and the id I visited the website and signed up:
#https://traveltime.com/docs/api/overview/getting-keys

traveltime_api <- "7bbbad489d637c38b2bea80e1e413721"
traveltime_id <- "c1fae682"

###################################################################################################################################
#INDIVIDUAL DISTRICTS
#1

#For 30 mins
#POP CENTROID
pop_centroid_RPK_dist_1_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_1_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_1_iso_30) = 4326

#For 15 mins
#POP CENTROID
pop_centroid_RPK_dist_1_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_1_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_1_iso_15) = 4326

#########################################################################################################################
#2

#For 30 mins
#POP CENTROID
pop_centroid_RPK_dist_2_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_2_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_2_iso_30) = 4326

#For 15 mins
#POP CENTROID
pop_centroid_RPK_dist_2_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_2_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_2_iso_15) = 4326

#########################################################################################################################
#3

#For 30 mins
#POP CENTROID
pop_centroid_RPK_dist_3_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_3_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_3_iso_30) = 4326

#For 15 mins
#POP CENTROID
pop_centroid_RPK_dist_3_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_3_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_3_iso_15) = 4326

#########################################################################################################################
#4

#For 30 mins
#POP CENTROID
pop_centroid_RPK_dist_4_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_4_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_4_iso_30) = 4326

#For 15 mins
#POP CENTROID
pop_centroid_RPK_dist_4_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_4_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_4_iso_15) = 4326
#########################################################################################################################
#########################################################################################################################

#5

#For 30 mins
#POP CENTROID
pop_centroid_RPK_dist_5_iso_30 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_5_iso_30.RDS")
st_crs(pop_centroid_RPK_dist_5_iso_30) = 4326



#For 15 mins
#POP CENTROID
pop_centroid_RPK_dist_5_iso_15 <- readRDS("data/Isochrone/pop_centroid_RPK_dist_5_iso_15.RDS")
st_crs(pop_centroid_RPK_dist_5_iso_15) = 4326
################################################################################################################################################################


################################################################################################################################################################

# Read in all files file
mapVA  <- st_read("data/Isochrone/tract/tl_2019_51_tract.shp",
                  stringsAsFactors = FALSE)

map_and_data <- inner_join(mapVA, acs_RPK_area, by = "GEOID")

################################################################################################################################################################

#Services
Adultcare <- read_excel("data/Isochrone/assistedAdultDisabilityElderly.xlsx", sheet=1)
colnames(Adultcare)[7] <- "Longitude"
colnames(Adultcare)[8] <- "Latitude"

Food_Clothing <- read_excel("data/Isochrone/FoodClothing.xlsx")
colnames(Food_Clothing)[7] <- "Longitude"
colnames(Food_Clothing)[8] <- "Latitude"

entertain <- read_excel("data/Isochrone/Entertainment.xlsx")
colnames(entertain)[7] <- "Longitude"
colnames(entertain)[8] <- "Latitude"

Hospital <- read_excel("data/Isochrone/hospitalsHealthDentalFreeClinics.xlsx")
colnames(Hospital)[7] <- "Longitude"
colnames(Hospital)[8] <- "Latitude"

education <- read_excel("data/Isochrone/education.xlsx")
colnames(education)[7] <- "Longitude"
colnames(education)[8] <- "Latitude"

professional <- read_excel("data/Isochrone/professional.xlsx")
colnames(professional)[7] <- "Longitude"
colnames(professional)[8] <- "Latitude"

recreation <- read_excel("data/Isochrone/recreation.xlsx")
colnames(recreation)[7] <- "Longitude"
colnames(recreation)[8] <- "Latitude"

transit <- read_excel("data/Isochrone/transit.xlsx")
colnames(transit)[7] <- "Longitude"
colnames(transit)[8] <- "Latitude"

banks <- read_excel("data/Isochrone/banks.xlsx")
colnames(transit)[7] <- "Longitude"
colnames(transit)[8] <- "Latitude"

################################################################################################################################################################


#I am reordering the districts to correct them in the map, without this the map
#does not have the right figures.
acs_RPK_dist <- read.csv("data/Isochrone/Distrpk.csv", header =T)
################################################################################################################################################################



# Examples of previous data being read in  ----------------------------------------------------------

# socdem_block <- readRDS("data/socdem_block.Rds")
# socdem_block <- st_transform(socdem_block, '+proj=longlat +datum=WGS84')
# 
# socdem_tract <- readRDS("data/socdem_tract.Rds")
# socdem_tract <- st_transform(socdem_tract, '+proj=longlat +datum=WGS84')
# 
# connectivity <- readRDS("data/connectivity.Rds")
# connectivity <- st_transform(connectivity, '+proj=longlat +datum=WGS84')
# 
# ems <- readRDS("data/ems.Rds")
# ems <- st_transform(ems, '+proj=longlat +datum=WGS84')
# 
# groceries <- readRDS("data/groceries.Rds")
# groceries <- st_as_sf(groceries, coords = c("longitude", "latitude"))
# st_crs(groceries) <- "+proj=longlat +datum=WGS84"
# groceries <- st_transform(groceries, '+proj=longlat +datum=WGS84')
# groceries <- subset(groceries, type == "farmers market" | type == "supermarket")
# groceries_latlong <- readRDS("data/groceries.Rds")
# groceries_latlong <- subset(groceries_latlong, type == "farmers market" | type == "supermarket")

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
ui <- navbarPage(title = "DSPG-Rappahannock 2021",
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
                 tabPanel("County Profile", value = "socio",
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
                            
                          ),
                          
                          
                 ),
                 
                 # traffic data tab-----------------------------------------------------------
                 tabPanel("Traffic", value = "connectivity",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Traffic in Rappahannock County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(8,
                                          withSpinner(leafletOutput("traffic_markers_map", height ="700px")),
                                          p(tags$small("Data Source: Virginia Department of Transportation"))
                                   ),
                                   column(4,
                                          h4(strong("Average Annual Daily Traffic in Rappahannock County", align = "center")),
                                          p("The map displays Average Annual Daily Traffic (AADT) data which is calculated by the traffic volume of a road segement for
                                            a year divided by the number of days in a year. AADT is shown on a map of Rappahannock county divided into distrcits and with the routes 
                                            in the area."),
                                          p("The circles on the map represent segments of routes that have experienced a relatively high percent change in AADT from 2010 to 2020. 
                                             Traffic Data was supplied to us by the Virginia Department of Transportation, who collected data from 262 route segments. The interactive map 
                                             includes the 18 route segments with the highest AADT percent change and count change greater than 30 from 2010 to 2020. The locations of the circles
                                             are at the midpoint of the segment it represents. The table below the map will list the names, start location, and end location of the segments represented
                                            in the map."),
                                          p("The size scale shows the absolute percent change of AADT from 2010 to 2020. The scale does not take positive and negative percent change into account,
                                            but by checking and unchecking the Positive and Negative boxes in the top right corner, segments with positive and negative percent change can be
                                            displayed separately. The color scale shows the AADT count change from 2010 to 2020. The scale does include negative count change in the lighter color,
                                            but the Positive and Negative boxes could also be used to analyze positive and negative count change separately."),
                                          p("Hovering over a circle displays the ID of the road segment, that corresponds to an ID in the table, count change and percent change of AADT from 2010 to 2020.
                                         Clicking on a circle pops up a time graph displaying the AADT counts for each year from 2010 to 2020 for that specific route segment.
                                           "),
                                          p("A few segments with the larger percent change seem to be clustering near the town of Washington. The map only includes
                                           three segments that have had a decrease in AADT, all of which are located in the southwest of Rappahannock. Looking at the services
                                           and where they are located in and outside of Rappahannock could ive us more insight into why we are seeing thes change in AADT in these areas.")
                                   ),
                                   
                                   column(8,
                                          h4("Route Segments", align = "center"),
                                          withSpinner(plotOutput("traffic_data_table"))
                                   )
                                   
                                   
                          )
                 ),
                 
                 # Services data tab-----------------------------------------------------------
                 tabPanel("Services", value = "",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Services Available in Rappahannock County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(12,h4(strong("Overview")),
                                          p("To understand a general suite of services available in Rappahannock County, 
                                          we sourced publicly available demographic and infrastructure data to provide an overview 
                                          of the services and amenities in Rappahannock County. The data were primarily obtained from the
                                            American Community Survey. The list of services was compiled from the Rappahannock news
                                            guide and resource guide as well as the Rappahanock Business Directory."),
                                          br(""),
                                          column(8,
                                                 withSpinner(leafletOutput("map_with_all_point", height ="700px")),
                                                 p(tags$small("Data Source: American Community Survey")),
                                                 p(tags$small("Services Sources:", a(href= 'https://www.rappahannock.com/business-directory', 'Rappahannock.com Business Directory|'), 
                                                              a(href='https://www.rappnews.com/news/the---guide-to-rappahannock-county/article_e48512f0-dca2-11e9-9561-0fe7b17a73b9.html',
                                                                'The 2020-2021 Guide to Rappahannock County|'),
                                                              a(href='https://www.agingtogether.org/uploads/1/3/0/9/130908318/aging_together_resource_guide_2021.pdf',
                                                                'Aging Together Resource Guide 2021'),)), 
                                                 p(tags$small(strong("Side Note on Piedmont District travel time map:"), "Concerning the driving distances for Piedmont District we notice an 
                                         unsusually small 15-minute travel time boundary.
                                          After conducting several sensitivity checks we are careful to conclude a possible reason for this is that there is a network of roads near
                                          the town of Washington and Sperryville that is causing the boundary to be so small which is not the case for other district centers.")),
                                                 
                                          ),
                                          
                                          column(4,
                                                 h4(strong("Services")),
                                                 p("In many respects, Rappahannock County is uniquely endowed with built amenities and services.
                                          However, most of them are clustered or available outside the county. There is a general limited
                                          number of services in the county. The primary services we consider include Food and Clothing, Hospitals, Education,
                                          Recreation, Entertainment, Professional, Transit, Banks, and Adult care Facilities.
                                          The map highlights the key resources that are available and accessed in the county."),
                                                 p("As the map shows, most of the services in Rappahannock county are clustered in and around Washington (Hampton district) 
                                          where inhabitants have access to among other supporting infrastructure. Additionally, most services can be
                                          accessed in the neighboring counties. Similar to the clusters of Services in Washington, the neighboring counties
                                          have clusters of many services as well."), 
                                                 p("Population-wise, the majority of the population in Rappahannock county 
                                          is situated in Hampton District which is home to a lot more services in the entire county. Hovering over each district displays 
                                          their corresponding population (according to the 2010 census data from the American Community Survey)."),
                                                 br(" "),
                                                 h4(strong("Travel Distance")),
                                                 p("We include driving distances (15 and 30 minutes) from specific points called the population centroids. First,
                                          we have travel time distance boundaries which show the services that are in  a certain proximity from the population 
                                          centroid of Rappahannock county (white dot around the center) in a 15-and-30 minute
                                          drive. Similarly, we have travel time distance boundaries for each of the five (5) 
                                          district centroids (red dot in each district)."),
                                                 p("From the map, we can see two driving boundaries. The inner driving boundary of the map 
                                          surrounding the centroid represents all parts of the region that are within a 15-minute 
                                          drive from the center of the county (the green cloud). The outer driving boundary on the 
                                          map surrounding the centroid represents all parts of the region that are within a 
                                          30-minute drive from the center of the county (the red cloud). ")
                                                 
                                                 
                                          ) ) ), ),     
                 # data tab -----------------------------------------------------------
                 tabPanel("Data", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data Sources"), align = "center"),
                                   br()
                          ),
                          tabsetPanel(
                            tabPanel("Data Sources",
                                     h3("", align = "center"),
                                     br(""),
                                     column(4,
                                            
                                            img(src = "data-gmaps.png", style = "display: inline; float: left;", width = "130px"),
                                            p(strong("Google Maps."), "Google Maps is a comprehensive web mapping service created by Google. Its goal is to provide an interactive map
                                              of all the geographical contents of the world. This resource has a variety of uses, ranging from examining all service locations within
                                              a city to finding the quickest route between locations. It provides data at latitude and longitude level. We used Google Maps to locate
                                              all services in Rappahannock County, and subsequently employed the information in calculating
                                              service access and coverage isochrones."),
                                            br(""),
                                     ),
                                     column(4,
                                            img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                                            p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census
                                            Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and
                                            socioeconomic characteristics including employment, disability, and health insurance coverage. We used AC 2014/18 5-year
                                            estimates to obtain census tract and census block group-level to explore Rappahannock County resident characteristics."),
                                            br(""),
                                            
                                            br(""),
                                            
                                     ),
                                     column(4,
                                            img(src = "data-traveltime.png", style = "display: inline; float: left;", width = "140px"),
                                            p(strong("TravelTime."), "TravelTime Application Programming Interface (API) aggregates data from OpenStreetMap, transport timetables and
                                           speed profiles to generate isochrones. An isochrone is a shape covering all locations that can be reached within the same timeframe
                                           given a start location, departure time, and a mode of transportation. We used the TravelTime API to produce isochrones of 15- and
                                           30-minute drive times interval from the county population centroids and district population centroids and in Rappahannock County."),
                                            br(""),
                                            
                                     )
                            )
                            
                          )
                 ) ,
                 
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
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  
  
  #age tabset -----------------------------------------------------
  ageVar <- reactive({
    input$agedrop
  })
  
  output$ageplot <- renderPlot({
    
    if (ageVar() == "ageGroups") {
      
      cbPalette <- c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00", "#CC79A7")
      
      rappk_ageGroups$Key[rappk_ageGroups$Key == "Young Adult: 18 to 30"] <- "Young Adult: 18 to 29"
      rappk_ageGroups$Key[rappk_ageGroups$Key == "Middle-Aged: 30 to 65"] <- "Middle-Aged: 30 to 64"
      
      rappk_ageGroups$Key <- factor(rappk_ageGroups$Key, levels = c("Adolescent: Under 18", "Young Adult: 18 to 29", "Middle-Aged: 30 to 64", "Senior: 65 and Over"))
      age_group_rappk_pie_plot  <- ggplot(rappk_ageGroups, aes(x="", y=`Percent.of.Population`, fill=Key)) +
        geom_bar(stat="identity", width=1, color =1) +
        coord_polar("y", start=0) +
        geom_text(aes(label = paste0(Rounded, "%")), position = position_stack(vjust=0.5), size =9) +
        labs(x = NULL, y = NULL) +
        theme_classic() +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.title = element_blank(),
              legend.text = element_text(size =15)) +
        scale_fill_manual(values=cbPalette) +
        ggtitle("Rappahannock") +
        theme(plot.title = element_text(hjust = 0.5, size = 25))+ scale_fill_viridis_d()
      
      va_ageGroups$Key[va_ageGroups$Key == "Young Adult: 18 to 30"] <- "Young Adult: 18 to 29"
      va_ageGroups$Key[va_ageGroups$Key == "Middle-Aged: 30 to 65"] <- "Middle-Aged: 30 to 64"
      va_ageGroups$Key <- factor(va_ageGroups$Key, levels = c("Adolescent: Under 18", "Young Adult: 18 to 29", "Middle-Aged: 30 to 64", "Senior: 65 and Over"))
      age_group_va_pie_plot <- ggplot(va_ageGroups, aes(x="", y=`Percent.of.Population`, fill=Key)) +
        geom_bar(stat="identity", width=1, color=1) +
        coord_polar("y", start=0) +
        geom_text(aes(label = paste0(Rounded, "%")), position = position_stack(vjust=0.5), size=9) +
        labs(x = NULL, y = NULL) +
        theme_classic() +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        scale_fill_manual(values=cbPalette) + 
        ggtitle("Virginia") +
        theme(plot.title = element_text(hjust = 0.5, size =20), legend.title = element_blank(),
              
              
              legend.text = element_text(size =15)) + scale_fill_viridis_d()
      
      ageplot <- grid.arrange(age_group_rappk_pie_plot,age_group_va_pie_plot, ncol =1,
                              bottom = textGrob("Data Source: ACS 2019 Five Year Estimate Table B01001",
                                                just= "left", gp = gpar(fontsize = 13)))  + scale_fill_viridis_d()
      ageplot
      
    }
    else if (ageVar() == "medAge") {
      counties_median_age_plot <- ggplot(counties_median_age, aes(x=County, y=`Median.Age`, fill=Key)) + 
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(`Median.Age`))), vjust=1.5, colour="white", size=10) +
        theme_minimal() +
        theme(axis.text = element_text(size = 15)) +
        geom_hline(aes(yintercept= 38.2, linetype = "   Virginia Median Age: 38"), 
                   color= "black", size = 1.5, alpha = 0.7) +
        ggtitle("Virginia Counties") +
        ylab("Median Age") +
        theme(plot.title = element_text(hjust = 0.5, size =20), legend.title = element_blank(), axis.title.x=element_blank(),
              legend.text = element_text(size =15),
              axis.title.y = element_text(size=15)) +
        scale_fill_viridis_d()
      
      district_median_age <- ggplot(data = sub_median_age, aes(x = District, y = `Median.Age`, fill = Key)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(`Median.Age`))), vjust=1.5, colour="white", size=10) +
        geom_hline(aes(yintercept= 50.1, linetype = "Rappahannock Median Age: 50"), color= "black", size = 1.5, alpha = 0.7) +
        ggtitle("Rappahannock by Districts") +
        ylab("Median Age")+
        #labs(caption = "Data Source: ACS 2019 Five Year Estimate Table S0101") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size =20),legend.title = element_blank(),
              axis.title.x=element_blank(),
              axis.text = element_text(size=15),
              legend.text = element_text(size =15),
              axis.title.y =element_text(size=15)) +
        #plot.caption = element_text(size=12)) +
        scale_fill_viridis_d()
      
      ageplot <- grid.arrange(counties_median_age_plot, district_median_age,
                              bottom = textGrob("Data Source: ACS 2019 Five Year Estimate Table S0101",
                                                just= "left", gp = gpar(fontsize = 13)))
      ageplot
    }
    else if (ageVar() == "ageDep") {
      counties_dep_plot <- ggplot(county_dep, aes(x=County, y=`Dependency.Ratio`, fill=Key)) +
        geom_bar(stat='identity', position='dodge') +
        ggtitle("Age Dependency Ratios in Virginia Counties") +
        ylab("Dependecy Ratio")+
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size=20)) +
        theme(axis.text = element_text(size = 15),legend.title = element_blank(), axis.title.x = element_blank(),
              legend.text = element_text(size=15),axis.title.y = element_text(size=15) ) +
        scale_fill_viridis_d()
      
      district_dep_plot <- ggplot(sub_dep, aes(x=District, y=`Dependency.Ratio`, fill=Key)) +
        geom_bar(stat='identity', position='dodge')  +
        ggtitle("Age Dependency Ratios in Rappahannock by Districts") +
        ylab("Dependecy Ratio")+
        theme_minimal() +
        #labs(caption = "Data Source: ACS 2019 Five Year Estimate Table S0101")+
        theme(plot.title = element_text(hjust = 0.5, size=20)) +
        theme(axis.text = element_text(size = 15), legend.title = element_blank(),
              axis.title.x = element_blank(), legend.text = element_text(size=15), axis.title.y = element_text(size=15))+
        # plot.caption = element_text(size=12)) +
        scale_fill_viridis_d()
      
      va_dep_plot <- ggplot(va_rappk_dep, aes(x=`Location`, y=`Dependency.Ratio`, fill=Key)) +
        geom_bar(stat='identity', position='dodge')  +
        ggtitle("Age Dependency Ratios") +
        ylab("Dependecy Ratio")+
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size =20)) +
        theme(axis.text = element_text(size = 15), legend.title = element_blank(),  axis.title.x=element_blank(),
              legend.text = element_text(size =15),axis.title.y = element_text(size=15)) +
        scale_fill_viridis_d()
      
      ageplot <- grid.arrange(va_dep_plot, counties_dep_plot,district_dep_plot, bottom = textGrob("Data Source: ACS 2019 Five Year Estimate Table S0101",
                                                                                                  just= "left", gp = gpar(fontsize = 13)))
      ageplot
    }
    else if (ageVar() == "ageTime") {
      ageplot <- ggplot(rappage_timeseries, aes(x = year, y = percent, group = ages, color = ages)) +
        geom_line(aes(size = estimate)) +
        labs(title = "Rappahannock Age of Population from 2010 to 2019", color = "Age Categories") +
        ylab("Percent of the population") + xlab("Year") +
        theme_minimal()+
        labs(caption = "Data Source: ACS Five Year Estimate Table B02001", size = "Number of Residents") +
        scale_color_viridis_d(
          labels = c("under18" = "Under 18", 
                     "age18_29" = "18 to 29", 
                     "age30_64" = "30 to 64", 
                     "age65_older" = "65 and Older")) +
        theme(plot.title = element_text(hjust=0.5, size =20),
              legend.text = element_text(size=15),
              legend.title =element_text(size=15),
              axis.title.x = element_text(size=15),
              axis.title.y = element_text(size=15),
              axis.text = element_text(size=15),plot.caption = element_text(size=13)) 
      ageplot
      
    }
    else if (ageVar() == "ageTime2") {
      ageplot <- ggplot(agetimeseries, aes(x = year, y = percent, color = ages, group = ages)) +
        geom_line(aes(size = estimate)) +
        labs(title = "Age of Population from 2010 to 2019", color = "Age Categories") +
        xlab("Year") +
        labs(caption = "Data Source: ACS Five Year Estimate Table B02001", size = "Number of Residents") +
        ylab("Percent of the population") +
        scale_color_viridis_d(
          labels = c("under18" = "Under 18", 
                     "age18_29" = "18 to 29", 
                     "age30_64" = "30 to 64", 
                     "age65_older" = "65 and Older")) +
        facet_wrap(~NAME) +
        theme_minimal()+
        theme(plot.title = element_text(hjust=0.5, size=20),
              axis.text = element_text(size=15),
              legend.title = element_text(size=15),
              legend.text = element_text(size=15),
              axis.title.x = element_text(size=15),
              axis.title.y = element_text(size=15),
              axis.text.x = element_text(angle = 40),
              plot.caption = element_text(size=13)
        )
      ageplot
    }
    
  })
  
  #Race plots --------------------------------------------------------------
  raceVar <- reactive({
    input$racedrop
  })
  output$raceplot <- renderPlot({
    if (raceVar() == "race1") {
      
      race_district$Year <- as.character(race_district$Year)
      racegraph <- race_district %>% filter(Race == "non-White") %>% 
        ggplot(aes(x = Year, y = Percent, fill = NAME, group = NAME)) + 
        geom_col() +   
        labs(title = "Non-White Population by 2010-2019", fill = "") +
        xlab("Year") +
        ylab("Percent of Population") +
        scale_fill_viridis_d() +
        theme_minimal()+
        theme(plot.title = element_text(hjust=0.5, size =20),
              axis.title.x = element_text(size=15),
              legend.text = element_text(size=15),
              legend.title = element_text(size=15),
              axis.text = element_text(size=15),
              axis.title.y=element_text(size=15),
              axis.text.x = element_text(angle = 40))
      
      racegraph
    }
    else if (raceVar() == "race2"){
      race_district$Year <- as.character(race_district$Year)
      blackgraph <- race_district %>% filter(Race == "Black") %>% 
        ggplot(aes(x = Year, y = Percent, fill = NAME, group = NAME)) + 
        geom_col() +   
        labs(title = "Black Population by District 2010-2019", fill = "") +
        xlab("Year") +
        ylab("Percent of Population") +
        scale_fill_viridis_d() +
        theme_minimal()+
        theme(plot.title = element_text(hjust=0.5, size =20),
              axis.title.x = element_text(size=15),
              legend.text = element_text(size=15),
              legend.title = element_text(size=15),
              axis.text = element_text(size=15),
              axis.title.y=element_text(size=15),
              axis.text.x = element_text(angle = 40))
      
      blackgraph
      
      
    }
    else if (raceVar() == "race3"){
      race_district$Year <- as.character(race_district$Year)
      asiangraph <- race_district %>% filter(Race == "Asian") %>% 
        ggplot(aes(x = Year, y = Percent, fill = NAME, group = NAME)) + 
        geom_col() +   
        labs(title = "Asian Population by District 2010-2019", fill = "") +
        xlab("Year") +
        ylab("Percent of Population") +
        scale_fill_viridis_d() +
        theme_minimal()+
        theme(plot.title = element_text(hjust=0.5, size =20),
              axis.title.x = element_text(size=15),
              legend.text = element_text(size=15),
              legend.title = element_text(size=15),
              axis.text = element_text(size=15),
              axis.title.y=element_text(size=15),
              axis.text.x = element_text(angle = 40))
      
      asiangraph
      
      
    }
    else if (raceVar() == "race4"){
      race_district$Year <- as.character(race_district$Year)
      othergraph <- race_district %>% filter(Race == "Other") %>% 
        ggplot(aes(x = Year, y = Percent, fill = NAME, group = NAME)) + 
        geom_col() +   
        labs(title = "Other Population by District 2010-2019", fill = "") +
        xlab("Year") +
        ylab("Percent of Population") +
        scale_fill_viridis_d() +
        theme_minimal()+
        theme(plot.title = element_text(hjust=0.5, size =20),
              axis.title.x = element_text(size=15),
              legend.text = element_text(size=15),
              legend.title = element_text(size=15),
              axis.text = element_text(size=15),
              axis.title.y=element_text(size=15),
              axis.text.x = element_text(angle = 40))
      
      othergraph
      
      
    }
    
    
  })
  
  #population--------------------------------------------------------------
  
  output$popplot <- renderPlot({
    
    
    popplot <- ggplot(population2010_2019 %>% filter(NAME != "Rappahannock"), aes(x = year, y = estimate, group = NAME, color = NAME)) +
      geom_line(aes(size = "Percent of Population" <- percent)) +
      theme_minimal()+ scale_fill_viridis_d() +
      ylab("Number of Residents")+xlab("Year")+
      labs(size = "Percent of Population", color = "District", caption = "Data Source: ACS Five Year Estimate Table B02001") +
      ggtitle(label = "Estimated Total Population 2010-2019") +
      theme(plot.title = element_text(hjust=0.5, size=20),
            axis.title.x = element_text(size=15),
            legend.text = element_text(size=15),
            legend.title = element_text(size=15),
            axis.text = element_text(size=15),
            axis.title.y=element_text(size=15),
            plot.caption = element_text(size=13))
    
    popplot
    
  })
  
  #household characteristics -----------------------------------------------
  hcVar <- reactive({
    input$hcdrop
  })
  
  output$hcplot <- renderPlot({
    
    if(hcVar() == "houseSize") {
      hcplot <- ggplot(householdSize, aes(x = "", y = estimate, fill = fct_inorder(People))) +
        geom_col(width = 1, color = 1) +
        geom_text(aes(label = paste0(estimate, "%")),
                  position = position_stack(vjust = 0.5), colour="white", size =8) +
        coord_polar(theta = "y") +
        labs(caption = "Data Source: ACS 2019 Five Year Estimate Table S2504")+
        guides(fill = guide_legend(title = "Number of People")) +
        theme(plot.title = element_text(hjust = 0.5, size =20),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(), 
              legend.text = element_text(size=15),
              legend.title = element_text(size=15),
              plot.caption = element_text(size=13),
              panel.background = element_rect(fill = "white")) +
        ggtitle("Rappahannock Household Size") +
        scale_fill_viridis_d()
      hcplot <- grid.arrange(hcplot)
    }
    else if(hcVar() == "rentOwn") {
      own_graph <- own %>%
        ggplot(aes(x = year, y = `Household.Units`)) + 
        geom_line(position = "identity", show.legend = TRUE, size=1.5) +
        ggtitle("Owner-Occupied") + theme_minimal() +
        ylab("Household Units")+
        theme(plot.title = element_text(hjust = 0.5, size=20),
              axis.title.y = element_text("Housing Units", size=15),
              axis.title.x = element_blank(),
              axis.text = element_text(size=15),
              axis.line = element_line(color = "black", size=0.5)) +
        scale_x_continuous(breaks=seq(2010,2019,by=1)) + scale_fill_viridis_d()
      #Graph (rent)
      rent_graph <- rent %>%
        ggplot(aes(x = year, y = `Household.Units`)) + 
        geom_line(position = "identity", show.legend = TRUE, size=1.5) +
        ylab("Household Units") +
        theme_minimal() +
        ggtitle("Renter-Occupied") +
        theme(plot.title = element_text(hjust = 0.5, size=20),
              axis.title.y = element_text("Housing Units", size =15),
              axis.title.x = element_blank(),
              axis.text = element_text(size=15),
              axis.line = element_line(color = "black", size=0.5)) +
        scale_x_continuous(breaks=seq(2010,2019,by=1))+ scale_fill_viridis_d()
      #putting the graphs together
      hcplot <- grid.arrange(own_graph, rent_graph, ncol=1,
                             bottom = textGrob("Data Source: ACS Five Year Estimate Table S2504",
                                               just= "left", gp = gpar(fontsize = 13)))
    }
    else if(hcVar() == "vehicles"){
      rappk_veh <- mutate(rappk_veh, type = c("None", "One", "Two", "Three or more"))
      rappk_veh_plot <- ggplot(rappk_veh, aes(x = "", y = estimate, fill = fct_inorder(type))) +
        geom_col(width = 1, color = 1) +
        geom_text(aes(label = paste0(estimate, "%")),
                  position = position_stack(vjust = 0.5),colour="white", size =8) +
        coord_polar(theta = "y") +
        guides(fill = guide_legend(title = "Number of Vehicles")) +
        theme(plot.title = element_text(hjust = 0.5, size =20),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(), 
              legend.text = element_text(size=15),
              legend.title = element_text(size=15),
              panel.background = element_rect(fill = "white")) +
        ggtitle("Vehicles Available per Household") +
        scale_fill_viridis_d()
      
      county_veh$num <- factor(county_veh$num, levels = c("None", "One", "Two", "Three or more"))
      county_veh3 <- county_veh %>%
        group_by(county) %>%
        arrange(county, desc(num)) %>%
        mutate(lab_ypos = cumsum(estimate) - 0.20 * estimate) 
      #Graph
      vehicle_graph <- ggplot(data = county_veh3, aes(x = county, y = estimate)) +
        geom_col(aes(fill = num), width = 0.7)+
        geom_text(aes(y = lab_ypos, label = paste0(estimate, "%"), group =county), color = "white",size=5, hjust =1.35)+
        coord_flip() +
        theme_minimal() +
        ylab("Percent")+xlab("County")+
        ggtitle("Number of Vehicles per Household in Surrounding Counties")+
        theme(plot.title = element_text(hjust = 0.5, size=20),
              legend.title = element_blank(),
              axis.text=element_text(size=12),
              legend.text = element_text(size=15),
              axis.title.x=element_text(size=15),
              axis.title.y=element_text(size=15)) +
        scale_fill_viridis_d()
      
      hcplot <- grid.arrange(rappk_veh_plot, vehicle_graph, ncol=1,
                             bottom = textGrob("Data Source: ACS 2019 Five Year Estimate Table S2504",
                                               just= "left", gp = gpar(fontsize = 13)))
      hcplot
      
    }
    
  })
  
  #education graphs------------------------------------------------------
  output$eduplot <- renderPlot({
    
    
    
    eduplot <- ggplot(edu2019, aes(x = NAME, y = Percent, group = EduLevel, fill = EduLevel)) + 
      geom_col() + scale_fill_viridis_d() +
      theme_minimal()+
      ggtitle("Education Levels by District")  + xlab("District")+ ylab("Percent") +
      labs(fill = "Education Level", caption ="Data Source: ACS 2019 Five Year Estimate Table B15003")+
      theme(plot.title = element_text(hjust = 0.5, size=20),
            axis.text.y=element_text(size=15),
            axis.text.x = element_text(size=14),
            legend.text = element_text(size=15),
            legend.title = element_text(size=15),
            axis.title.x=element_text(size =15),
            axis.title.y=element_text(size =15),
            plot.caption = element_text(size=13))
    eduplot
  })
  
  
  #income plot ----------------------------------------------------------
  output$incomePlot <- renderPlot({
    incomePlot <- ggplot(income2010_2019, aes(x = NAME.x, y = percent, fill =incomebracket, group = incomebracket)) +
      geom_col() +
      ylab("Percent") +xlab("District") +
      facet_wrap(~year) +
      coord_flip() +
      scale_fill_viridis_d(name="Income Group") +
      ggtitle("Household Income from 2010 to 2019") +
      theme_minimal()+
      labs(caption  = "Data Source: ACS Five Year Estimate Table B19001")+
      theme(plot.title = element_text(hjust=0.5, size=20),
            legend.text = element_text(size=15),
            axis.text = element_text(size=15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title=element_text(size=15), 
            plot.caption = element_text(size=13))
    incomePlot
  })
  
  
  #broadband tab -------------------------------------------------------------
  bbVar <- reactive({
    input$bbdrop
  })
  
  output$bbplot <- renderPlot({
    
    if (bbVar() == "intIncome") {
      intByIncome[,3][intByIncome[,3] == "< $20,000"] <- "Less than $20,000"
      intByIncome[,3][intByIncome[,3] == "$20,000-$74,999"] <- "$20,000 to $75,000"
      intByIncome[,3][intByIncome[,3] == "> $75,000"] <- "Greater than $75,000"
      intByIncome[,3] <- factor(intByIncome[,3], levels = c("Less than $20,000","$20,000 to $75,000","Greater than $75,000"))
      
      rappk_data4 <- intByIncome%>%
        group_by(`Income.Range`) %>%
        arrange(`Income.Range`, desc(Int)) %>%
        mutate(lab_ypos = cumsum(Percentage) - 0.25 * Percentage) 
      
      bbplot <- ggplot(data = rappk_data4, aes(x = `Income.Range`, y = Percentage)) +
        geom_col(aes(fill = Int), width = 0.7)+
        theme_minimal() +
        geom_text(aes(y = lab_ypos, label = paste0(round(Percentage),"%"), group =Int), color = "white", size =8)+
        coord_flip() +ggtitle("Internet Subscription based on Income in Rappahannock") +
        labs(caption  = "Data Source: ACS 2019 Five Year Estimate Table S2801")+
        theme(plot.title = element_text(hjust = 0.5, size=20),
              legend.title = element_blank(),
              axis.text=element_text(size=15),
              legend.text = element_text(size=15),
              axis.title.x=element_text(size=15),
              axis.title.y=element_text(size =15),
              plot.caption = element_text(size=12)) +
        xlab("Income Range") +ylab("Percent")+
        scale_fill_viridis_d()
      #plot
      bbplot
    }
    else if (bbVar() == "compDist") {
      sub_comp3 <- compDist %>%
        group_by(District) %>%
        arrange(District, desc(key)) %>%
        mutate(lab_ypos = cumsum(Percentage) - 0.25 * Percentage) 
      
      comp_plot <- ggplot(data = sub_comp3, aes(x = District, y = Percentage)) +
        geom_col(aes(fill = key), width = 0.7)+
        geom_text(aes(y = lab_ypos, label = paste0(round(Percentage),"%"), group =key), color = "white", size=8)+
        coord_flip() +
        theme_minimal() +
        ggtitle("Computer Ownership") +
        theme(plot.title = element_text(hjust=0.5, size=20),
              legend.title = element_blank(),
              axis.text=element_text(size=15),
              legend.text = element_text(size=15),
              axis.title.x=element_text(size=15),
              axis.title.y=element_text(size =15)) +ylab("Percent")+
        scale_fill_viridis_d()
      
      sub_int3 <- intDist %>%
        group_by(District) %>%
        arrange(District, desc(key)) %>%
        mutate(lab_ypos = cumsum(Percentage) - 0.25 * Percentage) 
      
      sub_plot <- ggplot(data = sub_int3, aes(x = District, y = Percentage)) +
        geom_col(aes(fill = key), width = 0.7)+
        geom_text(aes(y = lab_ypos, label = paste0(round(Percentage), "%"), group =key), color = "white", size=8)+
        coord_flip() +
        theme_minimal()+
        ggtitle("Internet Subscription") +
        theme(plot.title = element_text(hjust=0.5, size=20),
              legend.title = element_blank(),
              axis.text=element_text(size=15),
              legend.text = element_text(size=15),
              axis.title.x=element_text(size=15),
              axis.title.y=element_text(size =15)) +ylab("Percent")+
        scale_fill_viridis_d()
      
      bbplot <- grid.arrange(sub_plot, comp_plot, ncol=1,
                             bottom = textGrob("Data Source: ACS 2019 Five Year Estimate Table S2801",
                                               just= "left", gp = gpar(fontsize = 13)))
      bbplot
    }
    
  })
  
  #Housing Market -------------------------------------------------------------
  hmVar <- reactive({
    input$hmdrop
  })
  
  output$hmplot <- renderPlot({
    if(hmVar() == "housing1") {
      housing2010_2019 <- mutate(housing2010_2019, homevalues = rep(c("Under $100,000", "$100,000 to $300,000", "$300,000 to $500,000", "$500,000 to $1,000,000", "Over $1,000,000"), 10))
      housing2010_2019$homevalues <- factor(housing2010_2019$homevalues, levels = c("Under $100,000", "$100,000 to $300,000", "$300,000 to $500,000", "$500,000 to $1,000,000", "Over $1,000,000"))
      
      hmplot <- ggplot(housing2010_2019, aes(x = year, y = percent_of_houses, group = homevalues, color = homevalues)) +
        geom_line(aes(size = estimated_total)) +
        theme_minimal() + scale_fill_viridis_d()+
        ylab("Percentage of Homes") + xlab("Year")+
        labs(caption = "Data Source: ACS Five Year Estimate Table B25075", size = "Number of Homes", color = "Home Values") +
        ggtitle("Housing Prices (In US Dollars) From 2010 to 2019") +
        theme(plot.title = element_text(hjust=0.5, size =20),
              legend.text = element_text(size=15),
              legend.title =element_text(size=15),
              axis.title.x = element_text(size=15),
              axis.title.y = element_text(size=15),
              axis.text = element_text(size=15),
              plot.caption = element_text(size=13))
      hmplot
      
      
    }
    else if (hmVar() == "housing2") {
      
      housing2010_2019_by_district <- mutate(housing2010_2019_by_district, homevalues = rep(c("Under $100,000", "$100,000 to $300,000", "$300,000 to $500,000", "$500,000 to $1,000,000", "Over $1,000,000"), 50))
      housing2010_2019_by_district$homevalues <- factor(housing2010_2019_by_district$homevalues, levels = c("Under $100,000", "$100,000 to $300,000", "$300,000 to $500,000", "$500,000 to $1,000,000", "Over $1,000,000"))
      
      
      hmplot <- ggplot(housing2010_2019_by_district, aes(x = year, y = percent_of_houses, group = homevalues, color =homevalues)) +
        geom_line(aes(size = estimated_total)) +
        ylab("Percentage of Homes") +
        theme_minimal()+
        facet_wrap(~NAME) +
        labs(size = "Number of Homes",caption = "Data Source: ACS Five Year Estimate Table B25075", size = "Number of Homes", color = "Home Values") +
        ggtitle("Housing Prices (In US Dollars) From 2010 to 2019") +
        scale_color_viridis_d(name = "Home Values") + 
        theme(plot.title = element_text(hjust=0.5, size=20),
              axis.text.x = element_text(angle = 40),
              legend.text = element_text(size=15),
              legend.title =element_text(size=15),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=15),
              axis.text = element_text(size=15), plot.caption = element_text(size=13))
      hmplot
      
    }
    
  })
  
  
  
  #traffic  leaflet -------------------------------------------------------------
  output$traffic_markers_map <- renderLeaflet({
    #changing 2010-2015 data to have only the same roads that 2016-2020 has (getting rid of one route from 2014,2015)
    traff_2010 <- filter(traffic_2010, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2011 <- filter(traffic_2011, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2012 <- filter(traffic_2012, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2013 <- filter(traffic_2013, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2014 <- filter(traffic_2014, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2015 <- filter(traffic_2015, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    #combining them based on id (id name for id column in Virginia Department pf Transportation)
    traffic1 <- cbind(traff_2010[6:267,c(1,14)],traff_2011[6:267,c(1,14)],traff_2012[6:267,c(1,14)],traff_2013[6:267,c(1,14)],traff_2014[6:267,c(1,14)],traff_2015[6:267,c(1,14)])
    #traffic 2016-2019 has an extra header row than 2014-2015, so we are going to merge 2016-2019 first and then get rid of it(to merge w/ 2014-2015)
    traffic2 <- cbind(traffic_2016[7:268,c(1,14)], traffic_2017[7:268,c(1,14)], traffic_2018[7:268,c(1,14)], traffic_2019[7:268,c(1,14)], traffic_2020[7:268,c(1,14)])
    #combining all the years estimates
    traffic_year <- cbind(traffic1[,c(2,4,6,8,10,12)], traffic2[,c(2,4,6,8,10)])
    colnames(traffic_year) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
    #assigning the routes IDs
    traffic_year <- mutate(traffic_year, ID =1:n())
    #setting ID as the far left column
    traffic_year <- traffic_year[,c(12,1:11)]
    #getting route info (using 2020 to get the info but all the years should have the same info)
    traffic_info <- traffic_2020[7:268,c(1:3,4:13)]
    traffic_year2 <- cbind(traffic_info, traffic_year)
    #2010-2020
    traffic_data <- as.data.frame(sapply(traffic_year, as.numeric))
    traffic_data <- mutate(traffic_data, pctchange = (traffic_data[,12]-traffic_data[,2])/traffic_data[,2] *100)
    traffic_data2 <- mutate(traffic_data, abs=abs(traffic_data[,13]))
    #setting ID to character for graphing
    traffic_data2$ID<- as.character(traffic_data2$ID)
    #put in descending order then select top
    traffic_data3 <- traffic_data2 %>%
      arrange(desc(abs))
    #get's rid of the lnf(the ones that start with 0 for the initial year)
    traffic_data4 <- traffic_data3[7:262,] %>%
      slice(1:20)
    #traffic_data4 from above gives us the 20 roads with the highest pct change 
    traffic_data5 <- mutate(traffic_data4, count = `2020`-`2010`)
    traffic_data5 <- mutate(traffic_data5, count2 = abs(count))
    #We are getting rid of the roads that have changed less than 30 counts
    traffic_data6 <- traffic_data5 %>% filter(count2 >30)
    #merging the data with the coordinates(by ID)
    rappk_data <- merge(coordinates, traffic_data6, by="ID")
    #rearranging rappk_map data to be able to graph line plots for each marker
    rappk_data2 <- reshape(rappk_data, direction = "long",
                           varying = list(names(rappk_data)[7:17]),
                           v.names ="estimate",
                           idvar="ID",
                           timevar= "Year",
                           times = 2010:2020)
    #makes year a chracters to fix future graph formatting issues
    rappk_data2$Year <- as.character(rappk_data2$Year)
    #gets polygon data for the 5 subdivision in rappahannock county
    subdivisions <- county_subdivisions(state= "VA", county =157, cb = TRUE)
    #adds a column labeleing whether that road has a negative or posive change(used for layering)
    rappk_data2 <- mutate(rappk_data2, sign = if_else(sign(count)>0, "Positive", "Negative"))
    #making the size of the markers based on the percent change (absolute value) and the color based on count(absolute value)
    numPal <- colorNumeric(palette = "viridis", domain = rappk_data2$count, reverse = TRUE)
    pos_data <- rappk_data2 %>% filter(sign == "Positive")
    neg_data <- rappk_data2 %>% filter(sign== "Negative")
    #positive data charts
    pick_p <- function(id){
      dataFiltered <- filter(pos_data, ID== id)
      
      p <- ggplot(dataFiltered, aes(x=Year, y=estimate)) + 
        geom_line(position = "identity", aes(group=1)) +
        theme_minimal()+
        ggtitle("AADT from 2010 to 2020") +
        theme(plot.title = element_text(hjust=0.5),
              axis.title.x = element_blank()) +
        labs(y ="Annual Average Daily Traffic") 
      p
      return(p)
    }
    q <- lapply(1:length(unique(pos_data$ID)), function(i) {
      pick_p(pos_data$ID[i])
    })
    #negative data charts
    pick_n <- function(id){
      dataFiltered <- filter(neg_data, ID== id)
      
      p <- ggplot(dataFiltered, aes(x=Year, y=estimate)) + 
        geom_line(position = "identity", aes(group=1)) +
        theme_minimal()+
        ggtitle("AADT from 2010 to 2020") +
        theme(plot.title = element_text(hjust=0.5),
              axis.title.x = element_blank()) +
        labs(y ="Annual Average Daily Traffic") 
      p
      return(p)
    }
    r <- lapply(1:length(unique(neg_data$ID)), function(i) {
      pick_n(neg_data$ID[i])
    })
    #marker pop up labels
    label_p <- lapply(paste("ID: ", pos_data$ID, "<br />", "Count Change: ", pos_data$count,
                            "<br />", "Percent Change: ", round(pos_data$pctchange, digits =2),"%"), HTML)
    label_n <- lapply(paste("ID: ", neg_data$ID, "<br />", "Count Change: ", neg_data$count,
                            "<br />", "Percent Change: ", round(neg_data$pctchange, digits =2),"%"), HTML)
    map <- leaflet() %>% 
      addProviderTiles("Esri") %>%
      fitBounds(lng1=-78.345941, lat1=38.875265,
                lng2=-77.934027, lat2=38.518670) %>%
      addPolygons(data=subdivisions, popup = subdivisions$NAME, color = "black",
                  fillColor = "#ffffff00")
    #map
    traffic_markers_map <- map %>%
      addCircleMarkers(data= pos_data,
                       lng=~lng,
                       lat=~lat,
                       color = numPal(pos_data$count),
                       opacity = 0.3,
                       radius = sqrt(pos_data$abs),
                       label =label_p,
                       clusterId = pos_data$ID,
                       group= "Positive",
                       popup = popupGraph(q)) %>%
      addCircleMarkers(data= neg_data,
                       lng=~lng,
                       lat=~lat,
                       color=numPal(neg_data$count),
                       opacity = 0.1,
                       label =label_n,
                       radius = sqrt(neg_data$abs),
                       clusterId = neg_data$ID,
                       group = "Negative",
                       popup = popupGraph(r)) %>%
      addLayersControl(overlayGroups = c("Positive", "Negative"), options = layersControlOptions(collapsed = FALSE)) %>%
      addLegendSize(values =rappk_data2$abs, pal= numPal, opacity = 0.3, shape = "circle", title = "Percent Change",
                    position = "bottomright", strokeWidth = 10, breaks = 5) %>%
      addLegendNumeric(pal = numPal,bins=12, title = "Count change", values = rappk_data2$count, position= "bottomright",
                       height = 200)
    #plot
    traffic_markers_map
    
  })
  
  #trafficd route table ------------------------------------------------------------
  output$traffic_data_table <- renderPlot({
    #changing 2010-2015 data to have only the same roads that 2016-2020 has (getting rid of one route from 2014,2015)
    traff_2010 <- filter(traffic_2010, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2011 <- filter(traffic_2011, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2012 <- filter(traffic_2012, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2013 <- filter(traffic_2013, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2014 <- filter(traffic_2014, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    traff_2015 <- filter(traffic_2015, `Virginia Department of Transportation`  %in% traffic_2019$`Virginia Department of Transportation`)
    #combining them based on id (id name for id column in Virginia Department pf Transportation)
    traffic1 <- cbind(traff_2010[6:267,c(1,14)],traff_2011[6:267,c(1,14)],traff_2012[6:267,c(1,14)],traff_2013[6:267,c(1,14)],traff_2014[6:267,c(1,14)],traff_2015[6:267,c(1,14)])
    #traffic 2016-2019 has an extra header row than 2014-2015, so we are going to merge 2016-2019 first and then get rid of it(to merge w/ 2014-2015)
    traffic2 <- cbind(traffic_2016[7:268,c(1,14)], traffic_2017[7:268,c(1,14)], traffic_2018[7:268,c(1,14)], traffic_2019[7:268,c(1,14)], traffic_2020[7:268,c(1,14)])
    #combining all the years estimates
    traffic_year <- cbind(traffic1[,c(2,4,6,8,10,12)], traffic2[,c(2,4,6,8,10)])
    colnames(traffic_year) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
    #assigning the routes IDs
    traffic_year <- mutate(traffic_year, ID =1:n())
    #setting ID as the far left column
    traffic_year <- traffic_year[,c(12,1:11)]
    #getting route info (using 2020 to get the info but all the years should have the same info)
    traffic_info <- traffic_2020[7:268,c(1:3,4:13)]
    traffic_year2 <- cbind(traffic_info, traffic_year)
    traffic_data <- as.data.frame(sapply(traffic_year, as.numeric))
    traffic_data <- mutate(traffic_data, pctchange = (traffic_data[,12]-traffic_data[,2])/traffic_data[,2] *100)
    traffic_data2 <- mutate(traffic_data, abs=abs(traffic_data[,13]))
    #setting ID to character for graphing
    traffic_data2$ID<- as.character(traffic_data2$ID)
    #put in descending order then select top
    traffic_data3 <- traffic_data2 %>%
      arrange(desc(abs))
    #get's rid of the lnf(the ones that start with 0 for the initial year)
    traffic_data4 <- traffic_data3[7:262,] %>%
      slice(1:20)
    traffic_data5 <- mutate(traffic_data4, count = `2020`-`2010`)
    traffic_data5 <- mutate(traffic_data5, count2 = abs(count))
    #We are getting rid of the roads that have changed less than 30 counts
    traffic_data6 <- traffic_data5 %>% filter(count2 >30)
    traffic_year3 <-traffic_year2[,c(14,6,12,13)]
    leaflet_tbl <- filter(traffic_year3, ID %in% traffic_data6$ID) 
    leaflet_tbl<- merge(leaflet_tbl, traffic_data6[,c(1,13,15)], by ="ID")
    colnames(leaflet_tbl) <- c("ID", "Road Alias", "Start", "End", "pct", "Count Change")
    leaflet_tbl <- mutate(leaflet_tbl, `Percent Change`= paste0(round(pct, digits = 2)))
    leaflet_tbl <- leaflet_tbl[,c(1:4,7,6)]
    leaflet_tbl_final <- tableGrob(leaflet_tbl, rows = NULL)
    traffic_data_table<-as.ggplot(leaflet_tbl_final)
    #plot
    traffic_data_table
    
  })
  
  
  
  #Service  leaflet -------------------------------------------------------------
  output$map_with_all_point <- renderLeaflet({
    
    #making the map
    mypalette <- colorNumeric(palette="viridis", centerPop_RPK_dist$POPULATION)
    
    map_with_all_point <- leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri") %>%
      
      addPolygons(data=RPK_dist_outline,color = mypalette(centerPop_RPK_dist$POPULATION),
                  smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, 
                  label=paste(" ", acs_RPK_dist$Census_tract,", Population: ",centerPop_RPK_dist$POPULATION), 
      )%>%
      addLegend(pal = mypalette,position = "topleft",values = centerPop_RPK_dist$POPULATION,
                opacity = .6,title= paste("Total Population")) %>%
      
      addPolylines(data = RPK_dist_outline, color = "black", opacity = 2, weight = 2,)       %>%
      addPolylines(data = RPK_area_outline, color = "black", opacity = 1, weight = 1)       %>%
      addPolylines(data = RPK_outline, color = "black", opacity = 2, weight = 2 ) %>%
      
      ###Adding the travel time bounds
      addPolygons(data = pop_centroid_RPK_dist_1_iso_15 , color = "green",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Wakefield Driving")%>%
      #addLegend(colors = "slategray", labels = "60 Minute Drive Boundary") %>%
      addPolygons(data = pop_centroid_RPK_dist_1_iso_30, color = "red",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Wakefield Driving")%>%
      #addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
      
      
      addPolygons(data = pop_centroid_RPK_dist_2_iso_15 , color = "green",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Hampton Driving")%>%
      #addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
      addPolygons(data = pop_centroid_RPK_dist_2_iso_30 , color = "red",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Hampton Driving")%>%
      # addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%
      
      
      addPolygons(data = pop_centroid_RPK_dist_3_iso_15 , color = "green",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Jackson Driving")%>%
      #addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%
      addPolygons(data = pop_centroid_RPK_dist_3_iso_30 , color = "red",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Jackson Driving")%>%
      #addLegend(colors = "slategray", labels = "60 Minute Drive Boundary") %>%
      
      
      addPolygons(data = pop_centroid_RPK_dist_4_iso_15 , color = "green",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Piedmont Driving")%>%
      #addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
      addPolygons(data = pop_centroid_RPK_dist_4_iso_30 , color = "red",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Piedmont Driving")%>%
      # addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%
      
      addPolygons(data = pop_centroid_RPK_dist_5_iso_15 , color = "green",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Stonewall-Hawthorne Driving")%>%
      #addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
      addPolygons(data = pop_centroid_RPK_dist_5_iso_30 , color = "red",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Stonewall-Hawthorne Driving")%>%
      # addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%
      
      addPolygons(data = pop_centroid_RPK_iso_30 , color = "red",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Rappahannock Driving")%>%
      # addLegend(colors = "blue", labels = "30 Minute Drive Boundary") %>%
      addPolygons(data = pop_centroid_RPK_iso_15 , color = "green",
                  opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, group = "Rappahannock Driving")%>%
      # addLegend(colors = "green", labels = "15 Minute Drive Boundary") %>%
      
      
      addLayersControl(overlayGroups = c("Rappahannock Driving", "Wakefield Driving", 
                                         "Piedmont Driving", "Hampton Driving", "Jackson Driving", "Stonewall-Hawthorne Driving"), 
                       options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
      hideGroup("Rappahannock Driving")%>% 
      hideGroup("Wakefield Driving")%>% 
      hideGroup("Piedmont Driving")%>%
      hideGroup("Hampton Driving")%>%
      hideGroup("Jackson Driving")%>%
      hideGroup("Stonewall-Hawthorne Driving")%>%
      
      
      addCircleMarkers(centerPop_RPK_dist,lat = centerPop_RPK_dist$LATITUDE, lng= centerPop_RPK_dist$LONGITUDE,
                       radius =  4,
                       color = "red",
                       stroke = TRUE, fillOpacity = 1,
      ) %>%
      addLegend(colors = "red", labels = "District Population Centroid") %>%
      
      addCircleMarkers(centerPop_RPK,lat = centerPop_RPK$LATITUDE, lng= centerPop_RPK$LONGITUDE,
                       radius =  4,
                       color = "white",
                       stroke = TRUE, fillOpacity = 1
      ) %>%
      addLegend(colors = "white", labels = " Rappahannock Population Centroid") %>%
      
      ###SERVICES
      addCircleMarkers(entertain,lat = entertain$Latitude, lng= entertain$Longitude,
                       radius =  4,
                       color = "#55DDE0",
                       stroke = TRUE, fillOpacity = 1
      ) %>%
      addLegend(colors = "#55DDE0", labels = "Entertainment Centers") %>%
      
      addCircleMarkers(Adultcare,lat = Adultcare$Latitude ,
                       lng = Adultcare$Longitude,
                       radius =  3,
                       color = "magenta",
                       stroke = TRUE, fillOpacity = 1
      ) %>%
      addLegend(colors = "magenta", labels = "Adultcare/Assisted Living/Disability Center/Elderly") %>%
      
      addCircleMarkers(Food_Clothing,lat = Food_Clothing$Latitude ,
                       lng = Food_Clothing$Longitude,
                       radius =  3,
                       color = "tomato",
                       stroke = TRUE, fillOpacity = 1
      ) %>%
      addLegend(colors = "tomato", labels = "Food & Clothing")%>%
      addCircleMarkers(Hospital,lat = Hospital$Latitude ,
                       lng = Hospital$Longitude,
                       radius =  3,
                       color = "orange",
                       stroke = TRUE, fillOpacity = 1
      ) %>%
      addLegend(colors = "orange", labels = "Hospitals/Healthcare/Dental/FreeClinics")%>%
      
      addCircleMarkers(education,lat = education$Latitude ,
                       lng = education$Longitude,
                       radius =  3,
                       color = "black",
                       stroke = TRUE, fillOpacity = 1
      )%>%
      addLegend(colors = "black", labels = "Education")%>%
      addCircleMarkers(professional,lat = professional$Latitude ,
                       lng = professional$Longitude,
                       radius =  3,
                       color = "brown",
                       stroke = TRUE, fillOpacity = 1
      )%>%
      addLegend(colors = "brown", labels = "Professional")%>%
      addCircleMarkers(recreation,lat = recreation$Latitude ,
                       lng = recreation$Longitude,
                       radius =  3,
                       color = "purple",
                       stroke = TRUE, fillOpacity = 1
      )%>%
      addLegend(colors = "purple", labels = "Recreation")%>%
      
      addCircleMarkers(transit,lat = transit$Latitude ,
                       lng = transit$Longitude,
                       radius =  3,
                       color = "darkgreen",
                       stroke = TRUE, fillOpacity = 1
      )%>%
      addLegend(colors = "darkgreen", labels = "Transit")%>%
      addCircleMarkers(banks,lat = banks$Latitude ,
                       lng = banks$Longitude,
                       radius =  3,
                       color = "deeppink",
                       stroke = TRUE, fillOpacity = 1
      )%>%
      addLegend(colors = "deeppink", labels = "Banks")%>% 
      setView(lat = centerPop_RPK$LATITUDE, lng= centerPop_RPK$LONGITUDE, zoom =10)
    
    
    #Show the map
    map_with_all_point
    
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
