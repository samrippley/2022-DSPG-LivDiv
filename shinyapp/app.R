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
library(RColorBrewer)
library(plotly)
library(brio)

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
village_vector = c("Amrabati","Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar","Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur")


# data -----------------------------------------------------------

load("data/livdivdata.RData")

baseline <- livdiv %>%
  slice(1:306,)

#overview images
imgs <- list.files("www/photos/", pattern=".png", full.names = TRUE)

#borrowing data 
fdliv <- livdiv 
meals <- fdliv %>%
  select(c("hhid", "date", "fs_skipmeals", "fs_reducemeals", "village","hhid", "week_num"))
meals$date <- as_date(meals$date)
borrow <- fdliv %>%
  select(c("hhid", "date", "d_br", "d_br_cash","d_br_inkind", "br_amt","br_amtdue", "village","hhid", "week_num"))
borrow$date <- as_date(borrow$date)
purp <- fdliv %>%
  select(c("date", "br_purpose_cons", "br_purpose_exp", "br_purpose_fee", "br_purpose_loan", "br_purpose_asset", "br_purpose_ag", "village","hhid", "week_num"))
purp$date <- as_date(purp$date)
bramt <- borrow %>%
  select(c("br_amt", "date", "village", "week_num")) %>%
  group_by(village,week_num) %>%
  summarize_at(c("br_amt"), mean, na.rm=TRUE) 
dbr <- borrow %>%
  select(c("d_br", "d_br_cash", "d_br_inkind", "date", "village", "week_num")) %>%
  group_by(village,week_num) %>%
  summarize_at(c("d_br", "d_br_cash", "d_br_inkind"), sum, na.rm=TRUE) 

borrow <- livdiv %>%  select(c("hhid", "date", "d_br", "d_br_cash","d_br_inkind", "br_amt","br_amtdue", "village","hhid", "week_num"))
borrow$date <- as_date(borrow$date)

purp <- livdiv %>%  
  select(c("date", "br_purpose_cons", "br_purpose_exp", "br_purpose_fee", "br_purpose_loan", "br_purpose_asset", "br_purpose_ag", "village","hhid", "week_num"))

purposenv <- purp %>%  
  select(c("br_purpose_cons", "br_purpose_exp", "br_purpose_fee", "br_purpose_loan", "br_purpose_asset", "br_purpose_ag", "village", "week_num")) %>% summarize_at(c("br_purpose_cons", "br_purpose_exp", "br_purpose_fee", "br_purpose_loan","br_purpose_asset", "br_purpose_ag"), sum, na.rm=TRUE) 

purposenv <- t(purposenv)
purposenv <- as.data.frame(purposenv)
dfpurp <- data.frame(A = c("Consumption", "Other Expenses", "Fees Due", "Payback Other Loan", "Asset Purchase", "Agriculture Purchases"), B = c(purposenv$V1))
# pls purpose dynamic hist


# children data

avg_children <- baseline %>% group_by(village) %>% summarize(avg_children = sum(nb_children)/n())




# land fallowed data 
land_fallow <- baseline %>% 
  select(village, land_fallow)
land_fallow <- land_fallow %>% 
  group_by(village) %>% 
  summarise("avg" = mean(na.omit(land_fallow)), "sum" = sum(na.omit(land_fallow)))


# participate in ag data 

agfa <- baseline %>% group_by(village) %>% summarize(prop_farm = sum(farm_yn)/n())

# household asset data 

villages <- c("Amrabati","Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar","Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur") 
assets <- baseline %>% select(contains("asset")) %>% select(contains("num"))  %>% 
  summarize(Stove = sum(asset_stove_num)/n(), Bike = sum(asset_bike_num)/n(), 
            `Water Pump` = sum(asset_waterpump_num)/n(), `Solar Panel` = sum(asset_solarpanel_num)/n(), 
            Bed = sum(asset_bed_num)/n(), Fridge = sum(asset_fridge_num)/n(), Almirah = sum(asset_almirah_num)/n(), 
            PC = sum(asset_pc_num)/n(), TV = sum(asset_tv_num)/n(), Phone = sum(asset_mobile_num)/n(), 
            `Water Filter` = sum(asset_waterfilter_num)/n())

assets_long <- gather(assets, property, proportion, Stove:`Water Filter`)
assets_long["propotion"] = assets_long["proportion"]
assets_long["proportion"] <- round(assets_long$proportion, digits = 2)


#household size data
hhsize <- baseline %>% 
  select(village, hhid, nb_hhmem) 
median_hhsize <- hhsize %>% 
  group_by(village) %>% 
  summarise("median" = median(nb_hhmem))

# job duration 
job_duration <- baseline %>% 
  select(village, relationship1, relationship2, relationship3, relationship4,
         job1_duration1, job1_duration2, job1_duration3,
         job1_duration4, job1_duration5, job1_duration6, 
         job1_duration7, job1_duration9, job1_duration12)
amrabati_job_duration <- job_duration %>% 
  filter(village == "Amrabati") %>% 
  select(-c(relationship2, relationship3,relationship3, relationship4, job1_duration2, job1_duration3,
            job1_duration4, job1_duration5, job1_duration6, 
            job1_duration7, job1_duration9, job1_duration12)) 
amrabati_job_duration_summary <- amrabati_job_duration %>% 
  summarise("avg job duration" = mean(na.omit(job1_duration1 )))

beguakhali_job_duration <- job_duration %>% 
  filter(village == "Beguakhali") %>% 
  select(-c(relationship3, relationship4, job1_duration3,
            job1_duration4, job1_duration5, job1_duration6, 
            job1_duration7, job1_duration9, job1_duration12))
beguakhali_job_duration_summary <- beguakhali_job_duration %>% 
  summarise("avg job duration" = mean(na.omit(job1_duration1)))

bijoynagar_job_duration <- job_duration %>% 
  filter(village == "Bijoynagar") %>% 
  select(-c(relationship3, relationship4, job1_duration3,
            job1_duration4, job1_duration5, job1_duration6, 
            job1_duration7, job1_duration9, job1_duration12))
bijoynagar_job_duration_summary <- bijoynagar_job_duration %>% 
  summarise("avg job duration" = mean(na.omit(job1_duration1)))

birajnagar_job_duration <- job_duration %>% 
  filter(village == "Birajnagar") %>% 
  select(-c(relationship4,
            job1_duration4, job1_duration5, job1_duration6, 
            job1_duration7, job1_duration9, job1_duration12))
birajnagar_job_duration_summary <-  birajnagar_job_duration %>% 
  summarise("avg job duration" = mean(na.omit(job1_duration1)))

hsam_job_duration <- job_duration %>% 
  filter(village == "Haridaskati Samsernagar") %>% 
  select(-c(relationship2, relationship3, relationship4, job1_duration3,
            job1_duration4, job1_duration5, job1_duration6, 
            job1_duration7, job1_duration9, job1_duration12))
hsam_job_duration_summary <- hsam_job_duration %>% 
  summarise("avg job duration" = mean(na.omit(job1_duration1)))

ljan_job_duration <- job_duration %>% 
  filter(village == "Lakshmi Janardanpur")%>% 
  select(-c(relationship3, relationship4, job1_duration3,
            job1_duration4, job1_duration5, job1_duration6, 
            job1_duration7, job1_duration9, job1_duration12))
ljan_job_duration_summary <- ljan_job_duration %>% 
  summarise("avg job duration" = mean(na.omit(job1_duration1)))

pargumti_job_duration <- job_duration %>% 
  filter(village == "Pargumti") %>% 
  select(-c(relationship4,
            job1_duration4, job1_duration5, job1_duration6, 
            job1_duration7, job1_duration9, job1_duration12))
pargumti_job_duration_summary <- pargumti_job_duration %>% 
  summarise("avg job duration" = mean(na.omit(job1_duration1)))

pdwar_job_duration <- job_duration %>% 
  filter(village == "Purba Dwarokapur") %>% 
  select(-c(relationship4,
            job1_duration4, job1_duration5, job1_duration6, 
            job1_duration7, job1_duration9, job1_duration12))
pdwar_job_duration_summary <- pdwar_job_duration %>% 
  summarise("avg job duration" = mean(na.omit(job1_duration1)))

sagar_job_duration <- job_duration %>% 
  filter(village == "Sagar")%>% 
  select(-c(relationship3, relationship4, job1_duration3,
            job1_duration4, job1_duration5, job1_duration6, 
            job1_duration7, job1_duration9, job1_duration12))
sagar_job_duration_summary <- sagar_job_duration %>% 
  summarise("avg job duration" = mean(na.omit(job1_duration1)))

shibpur_job_duration <- job_duration %>% 
  filter(village == "Shibpur")%>% 
  select(-c(relationship4,
            job1_duration4, job1_duration5, job1_duration6, 
            job1_duration7, job1_duration9, job1_duration12))
shibpur_job_duration_summary <- shibpur_job_duration %>% 
  summarise("avg job duration" = mean(na.omit(job1_duration1)))

job_duration_avg <- c(amrabati_job_duration_summary$`avg job duration`, beguakhali_job_duration_summary$`avg job duration`,
                      bijoynagar_job_duration_summary$`avg job duration`,birajnagar_job_duration_summary$`avg job duration`,
                      hsam_job_duration_summary$`avg job duration`,
                      ljan_job_duration_summary$`avg job duration`, pargumti_job_duration_summary$`avg job duration`,
                      pdwar_job_duration_summary$`avg job duration`, sagar_job_duration_summary$`avg job duration`,
                      shibpur_job_duration_summary$`avg job duration`)

job_duration_summary <- data.frame(villages, job_duration_avg)

# remmitences v income data 

baseline.summary <- livdiv %>% select(village, full_inc, rmt_total) %>% 
  group_by(village) %>%
  summarise_all(mean, na.rm = TRUE)

#land owned data 
villages <- c("Amrabati","Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar","Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur") 
land_owners <- c(2, 18, 41, 14, 25, 21, 24, 21, 10, 24)
max_land_value <- c(16, 160, 510, 80, 120, 200, 250, 70, 80, 180 )
min_land_value <- c(10, 10, 8, 10, 1, 10, 3, 7, 2, 3)
mean_land_value <- c(13, 45.44, 60.95, 42.14, 45.44, 50.9, 64, 36.35, 23.7, 41.58)
sum_value <- c(26, 818, 2499, 590, 1136, 1069, 1537, 763.5, 237, 998)
land_stats <- data.frame(villages, land_owners, max_land_value, min_land_value, mean_land_value, sum_value)

#Migrant woker proportion data 

migrant_prop <- baseline %>% group_by(village) %>% summarize(migrant_proportion = (mean(hh_migrant)*100))

# savings data 
nbsav <- baseline %>% 
  group_by(village) %>% 
  summarize_at(c("nb_put_saving"), mean, na.rm=TRUE)

nbsavcount <- baseline %>% select(nb_put_saving, village) %>% count(nb_put_saving)


#land holding data 

land <- baseline %>% select(village, no_farm_reason) %>% na.omit(no_farm_reason)
land$no_farm_reason <- as.numeric(as.factor(land$no_farm_reason))

# salary data 

m_salary <-  baseline %>% group_by(village) %>% select(job1_salary1) %>% summarize(avg_salary = sum(job1_salary1, na.rm = TRUE)/n())

#edu and age data 
by_villagemore <- baseline %>% 
  group_by(village ) %>% 
  summarize_at(c("head_age", "head_edu", "head_married"), mean, na.rm=TRUE) %>%
  mutate(sub = substr(head_edu, 1, 4))

#occupation data 
occup1 <- baseline %>% 
  filter(relationship1 == 1) %>%
  select(c(village = "village", job = "job1_1"))
occup2 <- baseline %>% 
  filter(relationship2 == 1) %>%
  select(c(village = "village", job = "job1_2"))
occup3 <- baseline %>% 
  filter(relationship3 == 1) %>%
  select(c(village = "village", job = "job1_3"))
occup4 <- baseline %>% 
  filter(relationship4 == 1) %>%
  select(c(village = "village", job = "job1_4")) 

occup1 = na.omit(occup1)
occup2 = na.omit(occup2)
occup3 = na.omit(occup3)
occup4 = na.omit(occup4)

occup <- rbind(occup1, occup2, occup3, occup4)

countv <- occup %>% 
  group_by(village) %>%
  count(job) 

#secondary occupation data 

soccup1 <- baseline %>% 
  filter(relationship1 == 1) %>%
  select(c(village = "village", job = "job2_1"))
soccup2 <- baseline %>% 
  filter(relationship2 == 1) %>%
  select(c(village = "village", job = "job2_2"))
soccup3 <- baseline %>% 
  filter(relationship3 == 1) %>%
  select(c(village = "village", job = "job2_3"))
soccup4 <- baseline %>% 
  filter(relationship4 == 1) %>%
  select(c(village = "village", job = "job2_4")) 


soccup1 = na.omit(soccup1)
soccup2 = na.omit(soccup2)
soccup3 = na.omit(soccup3)
soccup4 = na.omit(soccup4)

soccup <- rbind(soccup1, soccup2, soccup3, soccup4)

scountv <- soccup %>% 
  group_by(village) %>%
  count(job) 
scountv <- scountv %>%
  filter(job != 0)

# Poverty line counts
Village <- c(rep("Amrabati", 2), rep("Beguakhali", 2), rep("Bijoynagar", 2), rep("Birajnagar", 2), rep("Haridaskati Samsernagar", 2), rep("Lakshmi Janardanpur",2), rep("Pargumti",2),rep("Purba Dwarokapur", 2), rep("Sagar", 2), rep("Shibpur",2))
Key <- rep(c("Live Above ₹204", "Live Below ₹204"), 2)
Households <- c(17,11,20,10,32,18,19,9,14,16,17,11,18,10,23,5,21,6,21,7)
Percentage<- c("60", "40", "67", "33", "64","36","68","32","53",
               "47","60","40","64","36","82","18","77","23","75","25" )
Village <- forcats::fct_rev(Village)
dat_pl <- data.frame(Village, Key, Households, Percentage)
# Business counts
Village <- c(rep("Amrabati", 2), rep("Beguakhali", 2), rep("Bijoynagar", 2), rep("Birajnagar", 2), rep("Haridaskati Samsernagar", 2), rep("Lakshmi Janardanpur",2), rep("Pargumti",2),rep("Purba Dwarokapur", 2), rep("Sagar", 2), rep("Shibpur",2))
#Village <- forcats::fct_rev(Village)
`Village` <- forcats::fct_rev(`Village`)
key <- rep(c("No", "Yes"), 2)
`households` <-c(26,2,27,3,48,2,24,2,27,3,25,3,24,4,21,7,21,7,25,3)
`percentage` <- c("93","7","90","10","96","4","86","14","90","10","90","10","86","14","75","25","75","25","90","10")
dat_bus <-  data.frame(dat_pl$Village, key, `households`,`percentage`)


# marital status
countmar <- baseline %>%
  count(head_married, head_female, head_married & head_female) %>% 
  mutate(Gender = case_when(head_female == 1 ~ "Female", 
                            head_female == 0 ~ "Male"))


fd <- livdiv %>%
  select(-(4:967))


# remittance data
rmt <- fd %>% 
  select(village, hhid, name, week, date, 
         rmt_total, rmt_method_bank, rmt_method_person, 
         rmt_method_mobile, rmt_method_moneyorder, rmt_method_oth, rmt_purpose_food, rmt_purpose_food,
         rmt_purpose_tuition, rmt_purpose_asset, rmt_purpose_med, rmt_purpose_oth, rmt_purpose_none)


rmt_mean <- rmt %>% 
  select(village, week, rmt_total) %>% 
  group_by(week, village) %>% 
  summarise(avg_rmt = mean(na.omit(rmt_total), na.rm = T))


village <- c("Amrabati","Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar","Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur") 


#--------------------------------------------------------------------


# rmt method plot:
method_counts <- c(397, 472, 1, 1, 13)
Method <- c("Bank", "In person", "Mobile", "Money Order", "Other")
method_dat <- data.frame(Method, method_counts, stringsAsFactors = T)
method_values <- c("       397", "       472", "   1", "   1", "    13")

rmt_method_plot <- ggplot(method_dat, aes( x= reorder(Method, method_counts), y = method_counts, fill = Method)) +
  geom_col() +
  labs(x = "", y = "Total Households") +
  theme_classic() +
  coord_flip()+
  theme(legend.position = "none", axis.text.y = element_text(size = 14))+
  #ggtitle("Method of Receiving Remittances")+
  geom_text(aes(label = method_values), size = 5) + scale_fill_brewer(palette = "Paired")


# leaflet data for villages tab--------------------------------------------------------------------

require(rgdal)

ind <- st_read(dsn = paste0(getwd(), "/data"), "gadm36_IND_3", stringsAsFactors = TRUE)

sundarban <- subset(ind, NAME_2 %in% c('North 24 Parganas','South 24 Parganas'))
d.sundarban<-st_union(sundarban)
village_all <- st_read(dsn = paste0(getwd(), "/data"), "Village, GP coordinates", stringsAsFactors = TRUE)

village <- subset(village_all, Village.Na %in% c("Amrabati","Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar","Lakshmi Janardanpur","Purba Dwarokapur","Shibpur")) 


icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "lightred"
)

map_leaflet <- leaflet(data = d.sundarban) %>%
  addTiles() %>%
  setView(lat= 21.9342, lng = 88.5345, zoom = 10) %>%
  addPolygons(
    fillColor = "green",
    stroke=TRUE,
    weight = 1,
    smoothFactor = 0.2,
    opacity = 1.0,
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(color = "white",
                                        weight = 2,
                                        bringToFront = FALSE)) %>%
  addAwesomeMarkers(
    lat = 21.6528, lng = 88.0753,
    label = "Sagar",
    labelOptions = , icon=icons) %>%
  addAwesomeMarkers(~lon, ~lat, label = ~as.character(Village.Na), labelOptions =  ,icon=icons, data=village) %>%
addAwesomeMarkers(
  lat = 22.227912, lng = 89.00475,
  label = "Pargumti",
  labelOptions = , icon=icons) %>%
addCircles(lat = 21.657, lng = 88.0591,
           radius=6000, color = 'blue', opacity = 1) %>%
  addCircles(lat = 22.227912, lng = 89.02,
             radius=6000, color = 'red') %>%
 addCircles(lat = 21.8619, lng = 88.43877,
            radius=6000, color = 'purple') %>%
  addCircles(lat = 22.16, lng = 88.789,
            radius=6000, color = 'yellow') %>%
  addCircles(lat = 21.5896, lng = 88.2653,
             radius=6000, color = 'black') %>%
  addLegend(title = "Administrative Blocks:", position = "bottomright", colors = c("blue", "black","purple", "yellow","red"), labels = c(
    "Beguakhali and Sagar - Sagar Block","Amrabati and Shibpur - Namkhana Block", "Lakshmi Janardanpur and Purba Dwarokapur - Patharpratima Block", 
    "Birajnagar and Bijoynagar - Gosaba Block", "Haridaskati Samsernagar and Pargumti - Hangalganj Block"
  ))
  
# leaflet data for age graph--------------------------------------------------------------------

icons2 <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "lightred"
)


map_leaflet2 <- leaflet(data = d.sundarban) %>%
  addTiles() %>%
  setView(lat= 21.9342, lng = 88.5345, zoom = 9) %>%
  addAwesomeMarkers(popup = paste0(tags$h4(strong("Village:"),"Amrabati"),
                                   #br(),
                                   tags$b("Average Age:"), " 46.68",
                                   br(),
                                   tags$b("Median Household Size:"), " 4",
                     br(),
                     tags$b("Average Number of Children per Household:"), " 2.68"),
                    lat = 21.570553, lng = 88.263257,
                    label = "Amrabati",
                    labelOptions = , icon=icons2)%>%
  addAwesomeMarkers(popup = paste0(tags$h4(strong("Village:"), " Beguakhali"),
                                   #br(),
                                   tags$b("Average Age:"), " 49.47",
                                   br(),
                                   tags$b("Median Household Size:"), " 5",
                                   br(),
                                   tags$b("Average Number of Children per Household:"), " 3.07"),
                    lat = 21.660021, lng = 88.046135,
                    label = "Beguakhali",
                    labelOptions = , icon=icons2)%>%
  addAwesomeMarkers(popup = paste0(tags$h4(strong("Village:"), " Bijoynagar"),
                                   #br(),
                                   tags$b("Average Age:"), " 52.6",
                                   br(),
                                   tags$b("Median Household Size:"), " 4",
                                   br(),
                                   tags$b("Average Number of Children per Household:"), " 1.92"),
                    lat = 22.141427, lng = 88.786918,
                    label = "Bijoynagar",
                    labelOptions = , icon=icons2)%>%
  addAwesomeMarkers(popup = paste0(tags$h4(strong("Village:"), " Birajnagar"),
                                   #br(),
                                   tags$b("Average Age:"), " 48.79",
                                   br(),
                                   tags$b("Median Household Size:"), " 4.5",
                                   br(),
                                   tags$b("Average Number of Children per Household:"), " 2.54"),
                    lat = 22.152742, lng = 88.790464,
                    label = "Birajnagar",
                    labelOptions = , icon=icons2)%>%
  addAwesomeMarkers(popup = paste0(tags$h4(strong("Village:"), " Haridaskati Samsernagar"),
                                  # br(),
                                   tags$b("Average Age:"), " 50.97",
                                   br(),
                                   tags$b("Median Household Size:"), " 4",
                                   br(),
                                   tags$b("Average Number of Children per Household:"), " 1.83"),
                    lat = 22.219522, lng = 89.033886,
                    label = "Haridaskati Samsernagar",
                    labelOptions = , icon=icons2)%>%
  addAwesomeMarkers(popup = paste0(tags$h4(strong("Village:"), " Lakshmi Janardanpur"),
                                  # br(),
                                   tags$b("Average Age:"), " 50.68",
                                   br(),
                                   tags$b("Median Household Size:"), " 4",
                                   br(),
                                   tags$b("Average Number of Children per Household:"), " 2.5"),
                    lat = 21.835391, lng = 88.45752,
                    label = "Lakshmi Janardanpur",
                    labelOptions = , icon=icons2)%>%
  addAwesomeMarkers(popup = paste0(tags$h4(strong("Village:"), " Pargumti"),
                                   #br(),
                                   tags$b("Average Age:"), " 53.78",
                                   br(),
                                   tags$b("Median Household Size:"), " 4",
                                   br(),
                                   tags$b("Average Number of Children per Household:"), " 1.57"),
    lat = 22.227912, lng = 89.00475,
    label = "Pargumti",
    labelOptions = , icon=icons2) %>%
  addAwesomeMarkers(popup = paste0(tags$h4(strong("Village:"), " Purba Dwarokapur"),
                                  # br(),
                                   tags$b("Average Age:"), " 43.64",
                                   br(),
                                   tags$b("Median Household Size:"), " 4",
                                   br(),
                                   tags$b("Average Number of Children per Household:"), " 3.32"),
                    lat = 21.885951, lng = 88.423895,
                    label = "Purba Dwarokapur",
                    labelOptions = , icon=icons2)%>%
  addAwesomeMarkers(popup = paste0(tags$h4(strong("Village:"), " Sagar"),
                                   #br(),
                                   tags$b("Average Age:"), " 47.5",
                                   br(),
                                   tags$b("Median Household Size:"), " 5",
                                   br(),
                                   tags$b("Average Number of Children per Household:"), " 3.11"),
                    lat = 21.6528, lng = 88.0753,
                    label = "Sagar",
                    labelOptions = , icon=icons2) %>%
  addAwesomeMarkers(popup = paste0(tags$h4(strong("Village:"), " Shibpur"),
                                   #br(),
                                   tags$b("Average Age:"), " 51.29",
                                   br(),
                                   tags$b("Median Household Size:"), " 4",
                                   br(),
                                   tags$b("Average Number of Children per Household:"), " 3.14"),
                    lat = 21.616568, lng = 88.253216,
                    label = "Shibpur",
                    labelOptions = , icon=icons2)




map_leaflet3 <- leaflet(data = d.sundarban) %>%
  addTiles() %>%
  setView(lat= 21.95, lng = 87.9, zoom = 8) %>%
  addCircles(lat = 21.657, lng = 88.0591,
             radius=6000, color = 'blue', opacity = 1) %>%
  addCircles(lat = 22.227912, lng = 89.02,
             radius=6000, color = 'red') %>%
  addCircles(lat = 21.8619, lng = 88.43877,
             radius=6000, color = 'purple') %>%
  addCircles(lat = 22.16, lng = 88.789,
             radius=6000, color = 'yellow') %>%
  addCircles(lat = 21.5896, lng = 88.2653,
             radius=6000, color = 'black') %>%
  addLegend(title = "Administrative Blocks:", position = "bottomright", colors = c("blue", "black","purple", "yellow","red"), 
            labels = c(
            "Beguakhali and Sagar - Sagar Block","Amrabati and Shibpur - Namkhana Block", "Lakshmi Janardanpur and Purba Dwarokapur - Patharpratima Block", 
             "Birajnagar and Bijoynagar - Gosaba Block", "Haridaskati Samsernagar and Pargumti - Hangalganj Block"
             ))


#-------------------------------


# rmt purpose plot:
Purpose <-  c("Food/Utility Purchases", "Other", "No Reason", "Medical Expenses","Tuition", "Assets/Durable Purchases")
purpose_count <- c(594, 128, 93, 43, 37, 27)
purpose_dat <- data.frame(Purpose, purpose_count, stringsAsFactors = T)
purpose_values <- c("      594", "      128", "     93", "     43", "      37", "      27")

rmt_purpose_plot <- ggplot(purpose_dat, aes(x = reorder(Purpose, purpose_count), y = purpose_count, fill = Purpose)) + 
  geom_col() +
  labs(x = "", y = "Total Households") +
  theme_classic() +
  #ggtitle("Purpose for Receiving Remittances")+
  #rotate_x_text(angle = 22, size = rel(0.8))
  coord_flip()+
  theme(legend.position = "none", axis.text.y = element_text(size = 14))+
  geom_text(aes(label = purpose_values), size = 5) + scale_fill_brewer(palette = "Paired")
#--------------------------------------------------------------------
# rmt table
fd <- livdiv %>%
  select(-(4:967))

rmt_dat <- fd %>% 
  select(village, date, week, rmt_total) %>% 
  arrange(week, village) %>% 
  group_by(week)
rmt_dat$date <- as_date(rmt_dat$date)
avg_rmt <- rmt_dat %>% 
  group_by(date, village) %>% 
  summarize("Average Remitances" = mean(rmt_total, na.rm = T))
avg_rmt[,3] <- format(round(unlist(avg_rmt[,3]), digits = 2), nsmall = 2)
names(avg_rmt) <- c("Date", "Village", "Average Remittances (INR ₹)")

#-----------------------------------------------------------------

# Expenditure plot data:
expen <- fd %>%
  select(c("hhid", "week_num", "date", "total_spending", "village","hhid")) 
expen$date <- as_date(expen$date)

exbyvil <- expen %>%
  select(c("week_num", "total_spending", "village")) %>%
  group_by(village,week_num) %>%
  summarize_at(c("total_spending"), mean, na.rm=TRUE) 

ggplot(exbyvil, aes(x=week_num, y=total_spending, color = village, na.rm=TRUE)) +
  geom_line() +
  labs(title="Average Weekly Expenditure by Village",
       x="Date", y="Average Weekly Expenditure (INR)") +
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) + scale_color_brewer(palette = "Paired")
#--------------------------------------------------------------------
# Expenditure table
expend_table <- expen %>% 
  group_by(date, village) %>% 
  summarize("Average Expenditure" = mean(total_spending, na.rm = T))
expend_table[,3] <- format(round(unlist(expend_table[,3]), digits = 2), nsmall = 2)
names(expend_table) <- c("Date", "Village", "Average Expenditure (INR ₹)")
#--------------------------------------------------------------------
# Income plot data:
fin_diary <- livdiv %>% select(village, date, week, name, full_inc) %>% arrange(week, village) %>% group_by(week) 
fin_diary$date <- as_date(fin_diary$date)
avg_tot_inc <- fin_diary %>% group_by(date, village, week) %>% summarize(avg_inc = mean(full_inc, na.rm = TRUE))
ggplot(avg_tot_inc, aes(date, avg_inc, color = village)) + geom_line() + labs(x = "", y = "Income (INR)", title = "Average Weekly Household Income by village", color = "Village") + scale_color_brewer(palette = "Paired")
#--------------------------------------------------------------------
#Income table 
avg_inc_table <- fin_diary %>% group_by(date, village) %>% summarize("Average Income" = mean(full_inc, na.rm = TRUE))
avg_inc_table[,3] <- format(round(unlist(avg_inc_table[,3]), digits = 2), nsmall = 2)
names(avg_inc_table) <- c("Date", "Village", "Average Income (INR ₹)")

#Shocks Data ------------------------------------------------------------------- 
## Frequency of each shock (Total baseline)
shocks <- baseline %>% select(village,shk1,shk2,shk3,shk4,shk5,shk6,shk7) 
shocks <- data.frame(y=unlist(shocks))
colnames(shocks) <- c('shock_nmb')
shock_labels <- c('None', 'Crop Loss', 'Loss of vegetation', 'Damage(saline water)','Forced to move(Flooding)', 'Loss of agricultural land(erosion)',
                  'Loss of home(erosion/cyclone)', 'Loss of livestock', 'Loss of business', 'Death/health', 'Other')

shocks_all <- ggplot(shocks, aes(shock_nmb)) + geom_bar(fill = "dark red", hoverinfo = "text", aes(text = paste(""))) + 
  labs(x = "", y = "Occurrences" ,title = "") + theme(axis.text = element_text(size = 7)) +
  theme_classic()+
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11),labels = str_wrap(shock_labels, width = 25) ,limits = c(0:11)) + 
  coord_flip()
## Average Shocks by Village
shocks2 <- baseline %>% select(village, shk_count) %>% 
  group_by(village) %>% summarize(avg_count = sum(shk_count, na.rm = TRUE)/n())

shocks_village <- ggplot(shocks2, aes(village, avg_count, fill = village)) + geom_col() + 
  labs(x = "", y = "Average Number of Shocks per Year (2009-2018)" ,title = "", fill = "Village") + 
  theme_classic()+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + scale_fill_brewer(palette = "Paired") + coord_polar()


## Total Shocks by Year
shock_year <- baseline %>% select(village, shk_2009_count, shk_2010_count, shk_2011_count, 
                                  shk_2012_count, shk_2013_count, shk_2014_count, 
                                  shk_2015_count, shk_2016_count, shk_2017_count,shk_2018_count) %>% 
  summarize("2009" = sum(shk_2009_count), "2010" = sum(shk_2010_count), "2011" = sum(shk_2011_count), "2012" = sum(shk_2012_count), 
            "2013" = sum(shk_2013_count), "2014" = sum(shk_2014_count), "2015" = sum(shk_2015_count), "2016" = sum(shk_2016_count),
            "2017" = sum(shk_2017_count), "2018" = sum(shk_2018_count))

shocks_year_long <- gather(shock_year, year, count, "2009":"2018")

shocks_by_year <- ggplot(shocks_year_long, aes(year, count, fill = year)) + geom_col(hoverinfo = "text", aes(text = paste("Count: ", count))) + 
  labs(x = "", y = "Total Shocks" ,title = "") + 
  theme_classic()+
  theme(axis.ticks.x=element_blank(), legend.position="none") + scale_fill_brewer(palette = "Paired")

## Frequency of each shocks in 2009

shocks_2009 <- baseline %>% select(shk_2009_type1, shk_2009_type2, shk_2009_type3, shk_2009_type4, shk_2009_type5, shk_2009_type6)
shock_labels_2009 <- c('Crop Loss', 'Loss of vegetation', 'Damage(saline water)',
                       'Forced to move(Flooding)', 'Loss of agricultural land(erosion)',
                       'Loss of home(erosion/cyclone)', 'Loss of livestock', 'Loss of business', 
                       'Death/health', 'Other')
shocks_2009 <- data.frame(y=unlist(shocks_2009))
colnames(shocks_2009) <- c('shk')

shocks_plot_2009 <-ggplot(shocks_2009, aes(shk)) + geom_bar(fill = "dark red",hoverinfo = "text", aes(text = paste(""))) + 
  labs(x = "", y = "Occurrences" ,title = "") + theme(axis.text = element_text(size = 8)) + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10),labels = str_wrap(shock_labels_2009, width = 25) ,limits = c(0:11)) +
  theme_classic()+
  coord_flip()
## Type of Cope after 2009 Shock

shocks_cope <- baseline %>% select(village, shk_2009_cope) 

cope_labels <- c("Did not do anything","Relatives/friends Aid",
                 "Local government aid", "Changed dietary practices", 
                 "Changed cropping practices", "Family increased non-farm employment", 
                 "Family increased farm wage employment", "Family migrated",
                 "Relied on savings", "Obtained credit", "Sold durable HH assets", "Sold land/building", 
                 "Rented out land/building","Distress sales of animal stock", "Relocated children",
                 "Reduced health/education","Did not know")

shocks_cope$shk_2009_cope<-replace(shocks_cope$shk_2009_cope, shocks_cope$shk_2009_cope == 997, 16)

cope_2009_plot <- ggplot(shocks_cope, aes(shk_2009_cope, fill = village)) + geom_bar(hoverinfo = "text", aes(text = paste(""))) +
  labs(x = "", y = "Total Shocks" ,title = "", fill = "Village") + scale_fill_brewer(palette = "Paired") +
  theme(axis.text = element_text(size = 8)) +
  theme_classic()+
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), labels = str_wrap(cope_labels, width = 30), limits = c(0:20)) + 
  coord_flip() 

## Relocation Status after 2009 Shock

shock_relocation <- baseline %>% select(village, shk_2009_reloc_yn)

relocation_labels <- c("No", "Yes, for under a month", "Yes, for over a month")

shock_relocation_2009_yn <- ggplot(shock_relocation, aes(shk_2009_reloc_yn, fill = village)) + geom_bar(hoverinfo = "text", aes(text = paste(""))) + 
  labs(x = "", y = "Total Households" ,title = "", fill = "Village") + 
  theme_classic()+
  scale_x_discrete(breaks = c(0,1,2), labels = str_wrap(relocation_labels, width = 30), limits = c(0:2)) + 
  scale_fill_brewer(palette = "Paired")

## Where they Relocated after 2009 Shock

shock_relocation_where <- baseline %>% select(village, shk_2009_reloc1)

relocation_where_labels <- c("Within same village","Other village in Sundarbans",
                             "Village outside Sundarbans, within West Bengal", "Kolkata", 
                             "Other Urban area outside Sundarbans, within West Bengal","Urban area outside West Bengal")

shock_relocation_2009 <- ggplot(shock_relocation_where, aes(shk_2009_reloc1, fill = village)) + geom_bar(hoverinfo = "text", aes(text = paste(""))) + 
  labs(x = "Relocation Areas", y = "Total Households" ,title = "", fill = "Village") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6), labels = str_wrap(relocation_where_labels, width = 20), limits = c(1:6)) +
  theme_classic()+
  scale_fill_brewer(palette = "Paired") + coord_flip() +  theme(axis.text = element_text(size = 8))


##Male and Female Income

malefemale_inc <- livdiv %>% select(village, week, inc_female, inc_male) %>% 
  group_by(week, village) %>% 
  summarize(avg_male_inc = mean(inc_male, na.rm = TRUE), avg_female_inc = mean(inc_female, na.rm = TRUE)) 

names(malefemale_inc) <- c("week", "village", " Avearge Male Income", "Avearge Female Income")
ggplot(malefemale_inc, aes(x = week)) + geom_line(aes(y = `Average Male Income`, color = village)) + 
  geom_line(aes(y = avg_female_inc, color = village)) + 
  labs(x = "", y = "Income (INR)", title = "Male and Female Income", color = "Village") + 
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) 

##Full income

fullinc <- livdiv %>%
  select(c("full_inc", "date", "village", "week_num")) %>%
  group_by(village,week_num) %>%
  summarize_at(c("full_inc"), mean, na.rm=TRUE) 

##Total income

totinc <- livdiv %>%
  select(c("inc_total", "village", "week_num")) %>%
  group_by(village,week_num) %>%
  summarize_at(c("inc_total"), mean, na.rm=TRUE) 

# Consumption data---------------------------

cs <- fd %>% 
  select(village,week_num, week, date, cs_count, cs_total, cs_ricegrains, cs_wheatflour, cs_veg,
         cs_tubers, cs_fishshrimp, cs_poultry, cs_eggs, cs_pulsespice,
         cs_redmeat, cs_dairy, cs_packaged, cs_fruit, cs_sinful, cs_other)

# expenditure on consumption items

cs_avg <- cs %>% 
  group_by(village, week) %>% 
  summarise(avg_cs = mean(na.omit(cs_total)))

# Average consumption expenditure plot
cs_avg_plot <- ggplot(cs_avg, aes(x = week, y = avg_cs , color = village)) +
  geom_line() +
  theme_classic()+
  #ggtitle("Average Weekly Consumption Expenditure by Village")+
  labs(x = "", y = "Average Consumption Expenditure (INR)", caption = "Mean: 766.13  Median: 731.68", color = "Villages")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
  theme(plot.caption = element_text(size = 10))
# Average consumption items bought

cs_avg_items <- cs %>% 
  group_by(village, week) %>% 
  summarise(avg_item = mean(na.omit(cs_count)))

filtered_cs_food <- reactive({
  avg_cs_food %>% 
    filter(village %in% input$village_cs_food)
})


# Average consumption items bought plot

cs_item_plot <- ggplot(cs_avg_items, aes(x = week, y = avg_item, color = village))+
  geom_line() +
  theme_classic()+
  #ggtitle("Average Consumption Items Bought a Week")+
  labs(x = "", y = "No. of Consumption Items Bought", color = "Villages")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))

# Average expenditure by group (staple, meat, other)

avg_cs_food <- cs %>% 
  group_by(village, week) %>% 
  summarise("Staple Items" = mean(na.omit(cs_ricegrains + cs_wheatflour + cs_veg +
                                            cs_fruit +cs_tubers + cs_pulsespice)), 
            "Meats" = mean(na.omit(cs_redmeat +cs_fishshrimp + cs_poultry)), 
            "Other" = mean(na.omit(cs_eggs + cs_dairy +cs_packaged + cs_sinful)))

# Staple items plot

cs_staple_plot <- ggplot(avg_cs_food, aes(x = week, y = `Average Staple Expenditure`, color = village)) +
  geom_line()+
  theme_classic()+
  labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 463.87  Median: 431.20",
       subtitle = "(Rice/Grains, Flour, Vegetables, Fruits, Tubers, Beans and Spices)")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
  theme(plot.caption = element_text(size = 10))

# Meat plot

cs_meat_plot <- ggplot(avg_cs_food, aes(x = week, y = `Average Meat Expenditure`, color = village))+
  geom_line()+
  theme_classic()+
  labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 158.97  Median: 431.20",
       subtitle = "(Red Meat, Fish, and Poultry)")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
  theme(plot.caption = element_text(size = 10))

# Other consumption items plot

cs_other_plot <- ggplot(avg_cs_food, aes(x = week, y = `Average Other Expenditure`, color = village))+
  geom_line() +
  theme_classic()+
  labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 113.75  Median: 111.94",
       subtitle = "(Eggs, Dairy, Packaged Foods, Tea, and Other Food Items)")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
  theme(plot.caption = element_text(size = 10))


# Non food consumption -------------------------
fin_diary <- livdiv
non_food_cs <- fin_diary %>% 
  select(village, week, aggregated_exp_clothes, exp_bookstuition, exp_utility, exp_toiletries,
         exp_health, exp_homerepairs, exp_transport, exp_livestock, exp_aginputs, exp_labor,exp_nonfoodother ) %>% 
  group_by(week, village) %>%
  summarise("Clothes" = mean(aggregated_exp_clothes, na.rm = TRUE), "Books/Tuition" = mean(exp_bookstuition, na.rm = TRUE),
            "Utilities" = mean(exp_utility, na.rm = TRUE), "Toiletries" = mean(exp_toiletries, na.rm = TRUE),
            "Health" = mean(exp_health, na.rm = TRUE), "Home Repairs" = mean(exp_homerepairs, na.rm = TRUE), 
            "Transportation" = mean(exp_transport, na.rm = TRUE), "Livestock" = mean(exp_livestock, na.rm = TRUE),
            "Agriculture" = mean(exp_aginputs, na.rm = TRUE), "Labor" = mean(exp_labor, na.rm = TRUE),
            "Other" = mean(exp_nonfoodother, na.rm = TRUE), avg_nonfood = (aggregated_exp_clothes +exp_bookstuition +exp_utility +exp_toiletries +
              exp_health +exp_homerepairs +exp_transport +exp_livestock +exp_aginputs +exp_labor + exp_nonfoodother))



filtered_non_food_cs <- reactive({
  non_food_cs %>% 
    filter(village %in% input$village_cs_nonfood)
})

# HFD by Block------------------------------------
blocks <- rep(c("Namkhana Block", "Sagar Block", "Gosaba Block", "Gosaba Block", "Hangalganj Block", "Patharpratima Block", "Hangalganj Block", "Patharpratima Block", "Sagar Block", "Namkhana Block"))

# expenditure
spending_vill <- fd %>% 
  select("village", "week", "total_spending")

exp_vill <- spending_vill %>%
  group_by(week, village) %>% 
  summarise(exp_total = mean(na.omit(total_spending))) %>% 
  mutate("Block" = c(blocks))

exp_block <- exp_vill %>% 
  group_by(week, Block) %>% 
  summarise(block_avg_exp = mean(exp_total))

# income
inc <- livdiv %>% 
  select(village, week, full_inc)

inc_vill <- inc %>% 
  group_by(week, village) %>% 
  summarise(inc_total = mean(na.omit(full_inc))) %>% 
  mutate("Block" = c(blocks))

inc_block <- inc_vill %>% 
  group_by(week, Block) %>% 
  summarise(block_avg_inc = mean(inc_total))

# remittances
rmt_v2 <- fd %>% 
  select(village, week, rmt_total)

rmt_v3 <- rmt_v2 %>% 
  group_by(week, village) %>% 
  summarise(remt_total = mean(na.omit(rmt_total))) %>% 
  mutate("Block" = c(blocks))

rmt_block <- rmt_v3 %>% 
  group_by(week, Block) %>% 
  summarise(block_avg_rmt = mean(remt_total))

# consumption
cs_v2 <- fd %>% 
  select(village, week, cs_total)

cs_vill <- cs_v2 %>% 
  group_by(week, village) %>% 
  summarise(cs_tot = mean(na.omit(cs_total))) %>% 
  mutate("Block" = c(blocks))

cs_block <- cs_vill %>% 
  group_by(week, Block) %>% 
  summarise(block_avg_cs = mean(cs_tot))

# non food consumption

nonfoodcs <- non_food_cs %>% 
  select(avg_nonfood) %>% 
  group_by(week, village) %>% 
  summarise(nf_total = mean(na.omit(avg_nonfood))) %>% 
  mutate("Block" = c(blocks))

nonfoodcs_block <- nonfoodcs %>% 
  group_by(week, Block) %>% 
  summarise(block_avg_nf = mean(nf_total))

blocks_vector <- c("Sagar Block", "Namkhana Block", "Patharpratima Block", "Gosaba Block", "Hangalganj Block")

# Events data -------------------------------------
Events <- c("Kharif Crop Preparation","Kharif Crop Harvest", "Rabi Crop Harvest","Honey Harvest", "Fani Cyclone", "Matmo Cyclone",
            "Republic Day", "Rama Navami", "Eid al-Fitr", "Indian Independence Day", "Dussehra", "Diwali" ,"Christmas")
start_week <- c(28, 1, 0, 19, 22, 48, 10, 20, 28, 38, 46, 49, 5)
end_week <- c(36, 12, 14, 32, 24, 49, 10.2, 20.2, 28.2, 38.2, 46.2, 49.2, 5.2)
event_periods <- data.frame(Events, start_week, end_week)
events_vector <- Events

filtered_event_cs <- reactive({
  event_periods %>% 
    filter(Events %in% input$event_choose_cs)
})

filtered_event_cs_food <- reactive({
  event_periods %>% 
    filter(Events %in% input$event_choose_cs_food)
})

filtered_event_cs_nonfood <- reactive({
  event_periods %>% 
    filter(Events %in% input$event_choose_cs_nonfood)
})

filtered_event_rmt <- reactive({
  event_periods %>% 
    filter(Events %in% input$event_choose_rmt)
})

filtered_event_exp <- reactive({
  event_periods %>% 
    filter(Events %in% input$event_choose_exp)
})

filtered_event_inc <- reactive({
  event_periods %>% 
    filter(Events %in% input$event_choose_inc)
})

#--------------------------------------------------------------------

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
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/events\">' +
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
ui <- navbarPage(title = "",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 
                 # main tab -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Assessing Livelihood Diversification in the Sundarbans (India) using High Frequency Data"),
                                      #h2(strong("Sundarbans")),
                                      # br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Polytechnic Institute and State University"),
                                      #h4("[updat this]"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;", align = "justify",
                                   column(4,
                                          h2(strong("The Setting")),
                                          p("The Sundarbans is a cluster of low-lying islands in the Bay of Bengal that spans across India and Bangladesh. The Sundarbans area hosts the largest mangrove forests in the world, supporting an exceptionally rich diversity of flora and endangered fauna such as the Bengal tiger, Estuarine crocodile, Indian python, and Irrawaddy dolphin. The United Nations Educational, Scientific and Cultural Organization (UNESCO) declared the Indian portion of the Sundarbans as a World Heritage Site in 1987 and the Bangladeshi portion in 1997, respectively. The Sundarbans Biosphere Reserve was designated under the UNESCO Man and the Biosphere Programme in 2001."),
                                          p("The vast delta is formed by the confluence of the Ganges, the Brahmaputra, and the Meghna rivers. The Sundarbans region has a complex network of tidal waterways, creeks, and mudflats. The Sundarbans is considered an endangered ecosystem under the ICUN Red List of Ecosystems due to increasing threats from climate change, densely populated villages, and declining agriculture.The Sundarbans supplies sustainable livelihoods to more than 4 million people living in small villages near the mangrove forests. Most households rely on a diverse set of local livelihood practices such as farming, woodcutting, fishing, foraging, and honey collection. Farmers, primarily landless laborers, commonly farm a single crop (Aman rice paddy) in the rainy season and sell their produce to intermediaries or traders. The woodcutters obtain traditional forest produce like timber, fuelwood, and pulpwood and sell these items in local markets. A large-scale harvest of shrimps, fish, crustaceans, and honey from the forests is also typical. However, with the ongoing climate and population changes, forest conservation efforts from the local government have placed a cap on harvesting."),
                                          img(src='sunphoto1.png', align = "center", width = "95%")
                                          
                                   ),
                                   column(4,
                                          h2(strong("Project Background")),
                                          p("The Sundarbans face an increasing threat to its ecological system due to several manmade and natural causes such as rising sea levels, coastal cyclones, and soil erosion. First, coastal cyclones, common to this area, are getting more frequent and more serve. From 1961 to 2022, 15 cyclones hit this area, with at least one occurring yearly in the past four years. This has led to severe damage to the ecosystem as well as the livelihood of locals. Second, there is an increase in deforestation due to increasing population and commercial uses. Additionally, the biological makeup of the forest, such as salinity, soil pH, and reduced freshwater, are being altered due to climate change, causing farmers to leave their land fallow due to the land’s decreasing productivity."),
                                          p("Agricultural households bear the brunt of these increasing threats to the Sundarbans. This is evident by the growing out-migration of the working population to cities and towns as a coping mechanism. Remittance income from domestic migration has become one of the significant sources of income in the Sundarbans since tropical cyclone Aila hit the region in 2009."),
                                          img(src='sunphoto.png', align = "center", width = "95%")
                                   ),
                                   
                                   column(4,
                                          h2(strong("Project Goals")),
                                          p("Climate change is a global issue; however, its impact is not felt equally across all regions. Developing countries, especially areas with widespread poverty and poor infrastructure, are more ill-equipped to cope with these environmental threats. The worsening effects of extreme weather patterns such as high temperatures, droughts, floods, and rising sea levels are especially problematic for countries with large coastal areas and populations that primarily depend on agriculture for their livelihood."),
                                          p("We examine the Sundarbans regions in West Bengal, India. This region has been facing impacts of climate change in recent years and has experienced several climate disasters such as flooding and cyclones over the past decade. Residents who primarily engage in small-scale agriculture are forced to diversify their likelihood strategies using out-migration and reduced farming to cope with the increasing challenges to livelihood due to environmental changes."),
                                          p("The overall goal of this project is to evaluate livelihood-diversification strategies using a high-frequency weekly financial dataset collected over a year for approximately 300 households from 10 representative villages in the region. This public-facing dashboard describes and visualizes households' livelihood diversification strategies, including changes in income, expenditure, and consumption patterns. The insights from this dashboard are essential for designing effective and targeted poverty-reducing strategies and aiding those affected by shocks such as natural disasters and climate change."),
                                          img(src='sunphoto2.png', align = "center", width = "95%")
                                   )
                          ),
                          fluidRow(align = "left",
                          p(tags$small(em('Source: Images taken by Sundarbans Field Team'))),
                          ),
                 ),

                 
                 ## Tab Date Intro--------------------------------------------
                 tabPanel("Data", value = "overview",
                          fluidRow(style = "margin: 6px;",
                                   column(6, 
                                          h3(strong("Overview")),
                                          p("For our analysis, we obtain baseline as well as weekly household financial and consumption data from Gupta et al. (2021). The authors originally collected the household-level data from a representative sample of approximately 300 rural households in the Sundarbans region in West Bengal, India, from November 2018 to October 2019. The baseline data are collected through standard household surveys, while the financial diaries data provide 52 weeks of time-series data points on the 300 representative households.", align = "justify"),
                                   #),
                                   #column(4,
                                          h3(strong("Initial Survey")),
                                          p("The initial or baseline surveys were conducted in November 2018. The baseline data allow the team to visualize and provide insights into the region's demographic and socio-economic characteristics. The baseline survey collected information on household demographics, economic activities, assets and landholding, shock history, migration, and agricultural behaviors.", align = "justify"),
                                          
                                          
                                   ),
                                   column(6,
                                          h3(strong("Financial Diaries")),
                                          p("The financial diaries captured high-frequency data on household income, expenditure, and consumption behavior. As such, we have weekly financial and economic activities for approximately 300 households for an entire year (November 2018 to October 2019)."),
                                          p("Household members were trained during the baseline interview to independently record their financial activities in their respective diaries (see images below for an example of a blank financial diary). Households received two more training sessions in the following two weeks and filled out the first four financial diaries during the training period. Additional support was given to families via phone calls and during the field team's monthly visit to collect completed diaries. These steps were implemented to ensure proper recording of weekly information. These diaries include data on weekly income, remittances, borrowing, lending, expenditure on consumption, and non-consumption items.", align = "justify"),
                                          br()
                                   )),
                        fluidRow(style = "margin: 6px;", align = "center", 
                                 h3(strong("Example of Financial Diary", align = "center")),
                            column(6, align = "center",
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                            h4(strong("Front Side- Financial Activity")),
                                   img(src='Picture2.png', width = "100%")),
                            column(6,
                                   h4(strong("Back Side- Expenditure Activity")),
                                   align = "center",
                                   img(src='fd.png', width = "90%")),
                           
                          
                          
                          )), 
                 ## Sundarbans Region--------------------------------------------
                 navbarMenu("Sundarbans Region" ,
                            tabPanel("Villages", 
                                     
                                     fluidRow(style = "margin: 2px;",
                                              align = "center",
                                              h1(strong("Representative Sample of Villages"))
                                                 
                                              ),
                                     
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              column(5, 
                                                     h2(strong("Sundarbans Area")),
                                                     p("Sampled households were surveyed in both North 24 –and South 24 – Parganas districts in West Bengal, India, where the Sundarbans region belongs administratively.
                                                       Specifically, ten representative villages were randomly chosen from five administrative blocks in the Sundarbans. Villages with the same administrative block are close in proximity to each other – the distance between villages is at most 15km. 
                                                       The villages were chosen in clusters of two so that they represent the diverse ecological variations within the Sundarbans region, including proximity to railways, roadways, and waterways. 
                                                       We visualize villages using an interactive map so a user can hover over the red markers to see the name and location of a specific village:"),
                                                     (tags$ul(
                                                     tags$li(tags$b("Beguakhali and Sagar"), "- Sagar Block"),
                                                     tags$li(tags$b("Amrabati and Shibpur"), "- Namkhana Block"),
                                                     tags$li(tags$b("Lakshmi Janardanpur and Purba Dwarokapur"), "- Patharpratima Block"),
                                                     tags$li(tags$b("Birajnagar and Bijoynagar"), "- Gosaba Block"),
                                                     tags$li(tags$b("Haridaskati Samsernagar and Pargumti"), "- Hangalganj Block"),
                                                     )),
                                                    br(),
                                                    h4(strong("Weather Related Events")),
                                                    br(),
                                                     p("The Sundarbans proximity to the Bay of Bengal causes it to be frequented by cyclones. These tropical cyclones usually form in May, October, and November. 
                                                       Although tropical cyclones are common to the area, the frequency and severity have increased in the past few years, with climate change as a contributing factor. During the data collection period, November 2018 to October 2019, the Sundarbans area was struck by two significant cyclones: "),
                                                    (tags$ul(
                                                     tags$li(tags$b("Fani"), "(Category 4): 26 April– 4 May 2019 "),
                                                     tags$li(tags$b("Matmo"), "(Category 2): 28 October"),
                                                    )),
                                                     p(tags$i("Fani"),"was reported as the strongest tropical cyclone in 2019 and the 10th most severe cyclone in the Indian subcontinent within the last 52 years. Although", tags$i("Fani"),"downgraded when it hit the Sundarbans, 
                                                       its high-speed winds and torrential rain destroyed homes, property, trees, and agricultural lands.",tags$i("Matmo"),"formed in the Philippine Sea on October 28th and dissipated as it went"),
                                                    br(),
                                                    h4(strong("Harvest Seasons")),
                                                    br(),
                                                    p("Agriculture is the backbone of the Sundarbans economy, with mostly small–scale farmers. 
                                                      The sector largely depends on a single crop, the rain-fed paddy Aman. In this region, however, agriculture is very seasonal as it depends on the monsoons: "),
                                                    (tags$ul(
                                                    tags$li(tags$b("Kharif"),"Season - This season occurs with the onset of monsoon."),
                                                    tags$ul(
                                                    tags$li("Preparation and cultivation of Aman paddy usually occur from", tags$b("June – August.")),
                                                    tags$li("Harvesting occurs between", tags$b("November – February.")),
                                                    ),
                                                    tags$li(tags$b("Rabi"),"Season - This is the dry season. While some vegetables are grown during this season, there are not many crops as most of the cultivated areas are fallowed."),
                                                    tags$ul(
                                                    tags$li("Crop Cultivation is between", tags$b("December – February")),
                                                    tags$li("Harvesting of rabi crops happens during summer,", tags$b("March - June")),
                                                    )
                                                    )),
                                                    (tags$ul(
                                                      tags$li(tags$b("Fisheries"),"is the dominant productive activity."),
                                                      tags$ul(
                                                        tags$li("Occurs year-round but majority of fish catch is during",tags$b("November – January ")),
                                                        tags$li("Some months", tags$b("(April, May, and June)"),"are closed for fishing.")
                                                      )
                                                      )),
                                                    tags$ul(
                                                    tags$li(tags$b("Honey Collection"), "occurs from", tags$b("April- June"))
                                                    ),
                                                    br(),
                                                    h4(strong("Festivals/Holidays")),
                                                    br(),
                                                    p("Several festivals and holidays that occur during the data collection period are: "),
                                                    (tags$ul(
                                                    tags$li(tags$b("Republic Day"),": Janurary 26"),
                                                    tags$li(tags$b("Rama Navami"),": April 14"),
                                                    tags$li(tags$b("Eid al-Fitr"),": June 4-5"),
                                                    tags$li(tags$b("Independence Day"),": August 15"),
                                                    tags$li(tags$b("Dussehra"),": October 8"),
                                                    tags$li(tags$b("Diwali"),": October 27"),
                                                    tags$li(tags$b("Christmas"),": December 25"),
                                                    )),
                                             ),

                                              column(7, 
                                                     h2(strong("")),
                                                     br(),
                                                     leafletOutput("map_leaflet", width = "100%", height = 800),
                                                     br(),
                                                     br(),
                                                    
                                                     img(src='months4.png', align = "center", width = "95%") 
                                              
                                                     
                                                     
                                              )),
                                   
                                              
                                     ),
                            tabPanel("Timelapse", 
                                     fluidRow(style = "margin: 2px;",
                                              align = "center",
                                              h1(strong("Coastal Degradation Timelapse")),
                                              column(4, 
                                                     h4(strong("Sundarbans Area"), align = "justify"),
                                                     p("The video shows a timelapse of the Sundarbans region from 1984 to 2022. There appears to be significant degradation of the Sundarbans coastline over the years. 
                                                       We impose circles on the map to indicate areas experiencing substantial coastline erosion. For instance, an island disappears entirely in the lower left-hand corner."), 
                                                     p("Climate change plays a primary role in this degradation. Rising sea levels, stronger waves, more extended tides, and increased frequency and intensity of storms and cyclones 
                                                       wear down rocks and soil, causing erosion of coastal areas. This erosion is a concern for Sundarbans residents as it reduces the sediment in the area, which acts as a", dQuote("natural buffer"),
                                                       "to flooding and wind. Moreover, the degradation increases groundwater's salinity, pushing salt water upstream, thus reducing the supply of drinking water. The thinning of the Sundarbans coast also negatively impacts households' agricultural yields, an essential source of income and food for families in this region."),
                                                     p("We expect that this erosion will continue in the next few years. The Bay of Bengal and the Arabian Sea are hotspots for cyclones. As such, with the increased frequency and intensity of 
                                                       cyclones in the Sundarbans region, these coastlines will likely continue to erode rapidly, increasing the negative impact of these cyclones, which will further threaten residents' 
                                                       safety and livelihood."), 
                                                     align = "justify"),
                                             
                                      column(8, 
                                             br(),
                                             br(),
                                              tags$video(type = "video/mp4",src = "Sundarbansv3 ‑ Made with FlexClip.mp4", width = "100%", controls = "controls", autoplay = T, loop = T)
                                            
                                             )),
                            
                                     
                            ),
                            
                 ),
                 
                 ## Tab Demographics --------------------------------------------
                 navbarMenu("Demographics" , 
                            tabPanel("Socioeconomic",
                                         fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Socioeconomic Characteristics"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Who lives in the Sundarbans Region?")),
                                                     p("We examine the data from the baseline survey collected in November 2018 to understand better the socio-demographic and economic characteristics of the Sundarbans population. Maps and Graphs are interactive – one can hover over an individual marker to produce a popup that provides specific attributes."),
                                                     p("Household heads in the Sundarbans area tend to be middle-aged adults, with the mean being around 49 years. Interestingly, there is a striking pattern by administrative block. Household heads that live in the Sagar, Namkhana, and Patharpratima Blocks are younger than their counterparts in Gosaba and Hangalganj. Additionally, families in Sagar, Namkhana, and Patharpratima Blocks have a greater median household size and, on average, have a higher number of children per household. Overall, most families are headed by married parents. Notably, males are more likely to be heads of married households, while females tend to be heads of unmarried households."),
                                                     p("Household heads also have low levels of education as the average education across villages is approximately five years, comparable to completing elementary school. This low level of education may contribute to the high levels of the poverty level in the region. More than half of families in Haridaskati Samsernagar live with less than ₹240 per week per person (Indian poverty line of 2018-2019). However, other villages like Purba Dwarokapur have a lower proportion of households (18%) below the poverty line.")
                                                     
                                              ) ,
                                              column(8,
                                                     tabsetPanel(
                                                       tabPanel("Household Formation",
                                                                h4(strong("Head of Household Demographics -  November 2018 (Baseline)")),
                                                                p(tags$small("Select marker for specific attributes:")),
                                                                withSpinner(leafletOutput("ageplo", height = "500px", width = "80%")),),
                                                       tabPanel("Socioeconomic Status",
                                                     h4(strong("Head of Household Demographics -  November 2018 (Baseline)")),
                                                     selectInput("agedrop", "Select Characteristic:", width = "100%", choices = c(
                                                       "Education" = "Mean Years of Education for Head of Households", 
                                                       "Poverty" = "Households that Live Below Poverty Line (₹204) per week", 
                                                       "Marital Status" = "Household Heads Marital Status"
                                                     ),
                                                     
                                                     ),
                                                     fluidRow(align = "center",
                                                              h4(strong(textOutput("result2")), align = "center"),
                                                     withSpinner(plotlyOutput("ageplot", height = "500px", width = "100%")),
                                                     textOutput("result6"), align = "left",
                                                     textOutput("result7"), align = "left",
                                                     ),
                                              ))),
                                              column(12, 
                                                     fluidPage(
                                                       p(tags$small("Select:")),                                                         
                                                       actionButton(inputId ="button", label = "Map")
                                                       
                                                     ),
                          
                                     
                                              ),
                                            
                                     )), 
                            tabPanel("Livelihood", 
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Livelihood"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Working in the Sundarbans ")),
                                                     p("Agriculture is the backbone of the Sundarbans' economy, where residents rely heavily on agriculture for their livelihood. This is supported by our 
                                                       interactive graphs where for most heads of households, their primary job is in the agricultural sector regardless of village, specifically farmer, 
                                                       casual labor, and agricultural wage-worker in 2018. However, there are some differences across villages. For example, if one selects Amrabati, the majority 
                                                       of household heads are employed as fishery workers (users can select village to see specific attributes). Agricultural jobs are seasonal, which may explain 
                                                       the short average job duration for household heads to an approximate 8 months in a year. Due to seasonality in agriculture, most household heads (about 60%) have 
                                                       a second job.  Similar to primary occupation, majority of household heads secondary occupation in in the agricultural sector."),
                                                     h4(strong("Agricultural Farming ")),
                                                     p("Household heads are not the only individuals involved in agriculture. On average, 63.9% of village households cultivated crops in the last 12 months. 
                                                       There are variations across villages, from as high as 86% in Haridaskati Samsernagar to as low as 7% in Amrabati."), 
                                                     p("Despite most of the population being involved in agriculture, land holding in the Sundarbans is mostly marginal. Pargumti and Bijoynagar have the 
                                                       highest average amount of land owned, with households owning over 60 kathas. Kathas is a land measurement commonly used in India. For reference, 
                                                       1 acre of land equals 32 kathas. To help rest and regenerate harvest, farmers tend to fallow their land. Bijoynagar had the largest average land fallow 
                                                       (about 95 kathas, approx. 3 acres) for the current agricultural season. "),
                                                     p(tags$small("Select:")),                                                         
                                                     actionButton(inputId ="button1", label = "Map")
                                                     
                                                     
                                              ) ,
                                              column(8, 
                                                     h4(strong("Livelihood – November 2018 (Baseline)")),
                                                     selectInput("ocudrop", "Select Characteristic:", width = "100%", choices = c(    
                                                       "Primary Occupation" = "Primary Occupation for Head of Households",
                                                       "Secondary Occupation" ="Secondary Occupation for Head of Households", 
                                                       "Job Duration" = "Average Job Duration for Head of Household",
                                                       "Agriculture" = "Percentage of Households Involved in Agricultural Farming",
                                                       "Land Holding" = "Average Amount of Land Owned by Village",
                                                       "Land Fallow" = "Total Land Fallowed (Kathas)"
                                                       
                                                       
                                                     ),
                                                     ),
                                                     fluidRow(align = "center",

                                                              h4(strong(textOutput("result1"), align = "center")),
                                                     withSpinner(plotlyOutput("ocuplot", height = "500px")),
                                                     #br(),
                                                     textOutput("result5"), align = "left"),
                                              ),
                                              #column(12, 
                                               #      fluidPage(
                                                #       p(tags$small("Select:")),                                                         
                                                 #      actionButton(inputId ="button1", label = "Map"),
                                                  #     br(),
                                                   #    br()
                                                   # ),
                                     )),
                            tabPanel("Financial", 
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Financial Practices"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("How do residents earn their money?")),
                                                     p("We present interactive graphs of the baseline survey to understand the financial behavior of the Sundarbans population. There are slight differences 
                                                       in average household monthly salary across villages ranging from as high as ₹4600 (US$ 71) in Amrabati to as low as ₹2500 (US$ 38.5) in Sagar, using 
                                                       1 US Dollar = ₹0.0154 exchange rate in 2018. The salary is more likely to come from working for someone else as most family members do not own a business."),
                                                     p("Migration appears to be a prominent livelihood strategy – on average, approximately 65% of households had at least one migrant in the last ten years. 
                                                       For some villages, migration is a dominant strategy with at least 80% of households in Beguakhali, Bijoynager, and Purba Dwarokapur having family members
                                                       migrating in the last ten years. Job opportunities may play a primary factor in the migration trend, thus explaining the high remittances in our sample. 
                                                       Remittances are any income household receives from someone working away from home. We visualize the relationship between average weekly remittances and 
                                                       average weekly income. In most villages, higher remittances correlate with a higher weekly income, except for Amrabti, Pargumti, and Sagar, 
                                                       suggesting other possible economic opportunities in these three villages."),
                                                    
                                              ),
                                              column(8, h4(strong("Financial – November 2018 (Baseline)")),
                                                     selectInput("findrop", "Select Practice:", width = "100%", choices = c( 
                                                       
                                                       "Household Business" = "Number of Households that Own a Business",
                                                       "Household Assets" = "Proportion of Households Owning Assets",
                                                       "Salary" = "Average Monthly Salary per Household by Village",
                                                       "Migrant Workers" = "Percentage of Household with Migrant Workers",
                                                       "Income/Remmitances" = "Income vs Remmitances (October 2018 - November 2019)",
                                                       "Savings" = "Number of Times Households Saved in Prior Year"
                                                     )),
                                                     fluidRow(align = "center",
                                                              h4(strong(textOutput("result")), align = "center"),
                                                     
                                                     withSpinner(plotlyOutput("finplot", height = "500px")),
                                                   
                                                     
                                              )),
                                               column(12, 
                                                      fluidPage(
                                                        p(tags$small("Select:")),
                                                        actionButton(inputId ="button2", label = "Map")
                                                      
                                                      ),
                                              
                                     )),
                            
                 )), 

                 
                 
                 # FD data tab-----------------------------------------------------------
                 
                 navbarMenu("High Frequency Data" ,
                            tabPanel("Expenditure",
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Expenditure"), align = "center"),
                                              column(12,h4(strong("How are residents spending their money?")),
                                                     p("We present average weekly expenditure from November 2018 - October 2019 to examine the spending habits of households in the Sundarbans. 
                                                       The graphs will provide information on the changing seasonal patterns of residents' spending in the region and how they react to events such as 
                                                       festivals and holidays, harvest seasons, and weather-related shocks. "),

                                                     p("Household expenditure comprises of spending on consumption (e.g., food) and non-consumption (e.g., rent) items. The average weekly expenditure 
                                                       over the 52-week data period was ₹1983 (US$ 30.53), with a median of ₹1832 (US$ 28.21), using 1 US Dollar = ₹0.0154 exchange rate in 2018.
                                                       The most significant expenses occurred during harvest seasons, particularly in villages with high amounts of land-holding and high proportions of 
                                                       agriculture, such as Beguakhali and Shibpur. We also observed increases in expenditure before the cyclones hit, which may reflect households' panic 
                                                       purchases to prepare for the onset of cyclones."),
                                                     
                                                     p("Villages in the Sagar and Namkhana Blocks have similar expenditure patterns over the 52 weeks. Average weekly expenditure peaks for both 
                                                       administrative blocks during the harvest season and after the Fani cyclone. Expenditure behavior in Gosba and Hanglganj blocks are also similar. 
                                                       In both blocks, expenditure fluctuates during Kharif crop harvest, increases on Republic Day, increases before the Fani cyclone and decreases after 
                                                       the hit of the Fani cyclone. These patterns suggest that households’ location may affect spending habits. "),

                                                     
                                              )),
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         #tags$h2("Select/Deselect all"),
                                         pickerInput("village_exp", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         pickerInput("event_choose_exp", "Select Event:", choices = events_vector, selected = "Kharif Crop Preparation", 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         pickerInput("block_choose_exp", "Select Administrative Block (For Expenditure by Blocks):", choices = blocks_vector, selected = blocks_vector, 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         p(tags$small("Select:")),                                                         
                                         actionButton(inputId ="button3", label = "Map")
                                         
                                         ),
                                       
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Weekly Expenditure",
                                                    h4(strong("Average Weekly Expenditure"), align = "center", style = "margin: 13px;"), 
                                                    plotOutput("exp", height = "500px"),
                                                    ),
                                           tabPanel("Weekly Expenditure by Blocks",
                                                    h4(strong("Average Weekly Expenditure by Administrative Block"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("exp_block", height = "500px")),
                                           tabPanel("Expenditure Table", 
                                                    h4(strong(""), align = "center"),
                                                    DT::DTOutput("exp_table"))
                                         )
                                       ),
                                       
                                     ),
                                     
                                     
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h4(strong(""), align = "justify"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Food Consumption")),
                                                     p("Consumption expenditure includes purchases by households on goods and services. Within the data period, households spent an average of ₹766.13 (US$ 11.79) 
                                                       per week on consumption items; they also consume on average, seven food items per week. "),
                                                     p("We decompose food consumption by staple food items, meats, and other consumable items:"),
                                                     (tags$ul(
                                                     tags$li("Staple Items - Rice/Grains, Flour, Vegetables, Fruits, Tubers, Beans and Spices"),
                                                     tags$li("Meats - Red Meat, Fish, and Poultry"),
                                                     tags$li("Other - Eggs, Dairy, Packaged Foods, Tea, and Sinful Items"),
                                                     )),
                                                     p("Families tend to mostly spend on staple food items followed by meats for their food consumption habits. For most villages, there was a significant increase
                                                       in the consumption of staple food items during Rama Navami and after the Fani cyclone. There is a significant spike for 'Other' items, in Shibpur, in late April. 
                                                       This spike is due to a large increase in expenditure on items such as tea, cigarettes, betel leaves, bidi, etc. These items are often deemed to be harmful to society 
                                                       but provide certain satisfaction to consumers. Therefore, the increase in consumption of these items could suggest a consumption mechanism for coping with the aftermath 
                                                       of Fani cyclone that coincided with this spike. Notably, households within the Sagar, Namkhana, and Patharpratima administrative blocks, on average, spend more on food 
                                                       consumption than their counterparts in Gosaba and Hangalganj blocks. "),
                                                
                                                     
                                              )),
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         pickerInput("village_cs", "Select Village:", choices = village_vector,
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         pickerInput("event_choose_cs", "Select Event:", choices = events_vector, selected = "Kharif Crop Preparation", 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         pickerInput("block_choose_cs", "Select Administrative Block (For Weekly Food Consumption by Blocks):", choices = blocks_vector, selected = blocks_vector, 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         p(tags$small("Select:")),                                                         
                                         actionButton(inputId ="button4", label = "Map")
                                         
                                       ),
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Weekly Food Consumption",
                                                    h4(strong("Average Weekly Food Consumption Spending"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("cs_exp", height = "500px")),
                                           tabPanel("Staple Items", 
                                                    h4(strong("Average Weekly Expenditure on Staple Food"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("cs_staple", height = "500px")),
                                           tabPanel("Meats", 
                                                    h4(strong("Average Weekly Expenditure on Meats"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("cs_meats", height = "500px")),
                                           tabPanel("Other", 
                                                    h4(strong("Average Weekly Expenditure on Other Food Items"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("cs_other", height = "500px")),
                                           tabPanel("Total Food Items", 
                                                    h4(strong("Total Weekly Consumption Items Purchased"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("cs_item", height = "500px")),
                                           tabPanel("Weekly Food Consumption by Blocks", 
                                                    h4(strong("Average Weekly Food Consumption by Administrative Block"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("cs_block", height = "500px"))
                                           #tabPanel("Table", DT::DTOutput("cs_table"))
                                         )
                                       ),
                                       
                                       
                                     ),
                                     
                                     
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h4(strong(""), align = "justify"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Non-Food Consumption")),
                                                     p("Consumption of non-food items includes clothes, books and tuition, utilities, toiletries, health, home repairs, transportation, livestock, 
                                                       agriculture, and labor."), 
                                                     p("Expenditures on health, home repairs, and books/tuition made up the largest but least recurring expenses, while utilities, toiletries, 
                                                       and transportation made up the most frequent purchases. Considering farmers make up the largest proportion of occupation in the Sundarbans, 
                                                       it is predictable to also see frequent expenditures on agriculture, livestock, and labor. The average weekly spending on non-food items was ₹882 (US$ 13.58). 
                                                       There were also increases in non-food expenditure during harvest seasons. The largest non-food spending occurred near the Bulbul cyclone and local festivals
                                                       such as Diwali and Dussehra."),
                                                   
                                                     
                                              )),
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         pickerInput("village_cs_nonfood", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         varSelectInput("nonfood_group", "Select Consumption Group:", non_food_cs[,-c(1,2,14)]),
                                         pickerInput("event_choose_cs_nonfood", "Select Event:", choices = events_vector, selected = "Kharif Crop Preparation", 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         pickerInput("block_choose_nf", "Select Administrative Block (For Non Food Consumption by Blocks):", choices = blocks_vector, selected = blocks_vector, 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         p(tags$small("Select:")),                                                         
                                         actionButton(inputId ="button5", label = "Map")
                                         
                                       ),
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Weekly Non-Food Consumption",
                                                    h4(strong("Average Weekly Non-Food Consumption"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("nonfood_plot", height = "500px")),
                                           tabPanel("Non-Food Consumption by Blocks",
                                                    h4(strong("Average Weekly Non-Food Consumption by Administrative Block"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("nf_block_plot", height = "500px"))
                                           #tabPanel("Table", DT::DTOutput("nonfood_table"))
                                         )
                                       ),
                                       
                                       
                                     ),
                                     
                                     
                            ), 
                            
                            tabPanel("Income",
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Income"), align = "center"),
                                              column(12,h4(strong("Overview")),
                                                     p("Income is defined as total male and female income, including remittances. On average, the weekly income per household across villages is approximately 
                                                       ₹1400 (US$ 21.56), and the median is about ₹1400 (US$ 21.56).  Sagar has a weekly average income of around ₹1954 (US$ 30.9), which is one of the higher averages
                                                       in the region. Since many of these households work for wages either as agriculture workers or casual laborers, the per week income is relatively the same throughout
                                                       the region. This indicates why the weekly income ranges consistently between ₹1000 and ₹2000."),
                                                     p("Throughout the year, however, income fluctuates for many households due to different factors such as harvest seasons, an influx of remittances, or other external factors. 
                                                       There are also considerable variations in average weekly household income across the sampled villages over the 52 weeks. For example, in late March, there was an increase 
                                                       in average income for families living in Beguakhali, Haridaskati Samsernagar, Purba Dwarokapur, and Sagar. The increase is especially significant for households in Beguakhali. 
                                                       Before March, the average weekly income was around ₹1500 (US$ 23.1), then it increased substantially to over ₹7500 (US$ 115.5) in the last week of March. This spike is caused 
                                                       by an increase in remittances shown in the remittances graph."),
                                                     p("Males earn more in this region than females. Males' average weekly income is ₹1065.54 (US$ 16.4 whereas for females the mean weekly income is ₹96 (US$ 1.48). 
                                                       Female income has a higher variance across villages than males.")
                                                     )),
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         #tags$h2("Select/Deselect all"),
                                         pickerInput("village_inc", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         pickerInput("event_choose_inc", "Select Event:", choices = events_vector, selected = "Kharif Crop Preparation", 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         varSelectInput("Gender", "Select Gender (For Income by Gender):", malefemale_inc[,-(1:2)]),
                                         pickerInput("block_choose_inc", "Select Administrative Block (For Income by Blocks):", choices = blocks_vector, selected = blocks_vector, 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         p(tags$small("Select:")),                                                         
                                         actionButton(inputId ="button6", label = "Map")
                                       ),
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Weekly Income",
                                                    h4(strong("Average Weekly Income"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("inc", height = "500px")),
                                           tabPanel("Weekly Income by Gender", 
                                                    h4(strong("Average Weekly Income by Gender"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("malefemaleinc", height = "500px")),
                                           #tabPanel("Full Income", plotOutput("fullinc")),
                                           tabPanel("Weekly Income by Blocks", 
                                                    h4(strong("Average Weekly Income by Administrative Block"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("inc_block", height = "500px")),
                                           tabPanel("Weekly Income Table", 
                                                    DT::DTOutput("inc_table"))
                                         )
                                       ),
                                     ),
                                     
                                     
                            ),
                            
                            tabPanel("Borrowing",
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Borrowing"), align = "center"),
                                              column(12,h4(strong("Borrowing Practices by Sundarbans households")),
                                                     p("Households in the Sundarbans seem to use borrowing as a livelihood coping strategy. On average, approximately 22% of households tend to borrow each week. 
                                                     It is most common to borrow no re usually borrow around 600₹ (US$ 9.25) a week. The amount borrowed and the number of households borrowing is relatively consistent
                                                     during the year, despite the differences in borrowing behavior across villages. For example, there is a significant spike in the amount borrowed between April and 
                                                     June by families living in Pargumti, Purba Dwarokapur, and Shibpur. This increase coincides with the dry season and after cyclone Fani. Bijoynagar had the largest number of households borrowing,
                                                     with over 30 households borrowing in January during the Karif harvest and Rabi planting seasons. During the Karif harvest season, there is a decrease in the number of households borrowing and the amount borrowed. 
                                                       This suggests that borrowing is used throughout the year to help support families outside the primary harvest season and to help with different shocks."),
                                                     p("Families borrow primarily for food and non-food items (consumption), with over 2000 total transactions across households and villages during the 52 weeks, 
                                                       roughly 6-7 loans per household. Usually, loans are used for investments with hopes of higher returns. In the Sundarbans region, however, this is not the 
                                                       case as there are very few occurrences of households making asset purchases or investing in agriculture. Approximately half of the loans were in cash, 
                                                       and the other half were in-kind. An in-kind loan is where a debtor can borrow without paying in cash. Repayment can be in multiple forms, such as lending 
                                                       land or labor. This loan often leads to more significant losses in the event of a default, where the borrower cannot pay back the lender."),

                                              )),
                                     sidebarLayout(
                                       sidebarPanel(pickerInput("village_bramt", "Select Village:", choices = village_vector, 
                                                                selected = village_vector, 
                                                                multiple = T, options = list(`actions-box` = T)),
                                                    pickerInput("event_choose_borr", "Select Event:", choices = events_vector, selected = "Kharif Crop Preparation", 
                                                                multiple = T, options = list(`actions-box` = T)),
                                                    p(tags$small("Select:")),                                                         
                                                    actionButton(inputId ="button7", label = "Map")
                                       ),
                                     
                                     mainPanel(
                                       tabsetPanel(
                                         tabPanel("Weekly Borrowing",
                                                  h4(strong("Total Weekly Amount Borrowed"), align = "center", style = "margin: 13px;"),
                                                  plotOutput("bor", height = "500px")),
                                         tabPanel("Total Households Borrowing",
                                                  h4(strong("Total Households Borrowing"), align = "center", style = "margin: 13px;"),
                                                  plotOutput("borr", height = "500px")),
                                         tabPanel("Borrowing Usage", 
                                                  h4(strong("Households’ Purpose for Borrowing"), align = "center", style = "margin: 13px;"),
                                                  plotOutput("purpplot", height = "500px"))
                                       
                                       )
                                     ),
                            ),  
                            ),
                                     
                                    
                            
                            tabPanel("Remittances", value = "",
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Remittances"), align = "center"),
                                              h4(strong("Remittances Sources and Usage")),
                                                     p("Sundarbans’ households receive a consistent flow of remittances during the 52 weeks suggesting that families in this area rely heavily on remittances 
                                                       as a significant source of income. There are substantial variations in the spike of remittances across villages, suggesting that remittances are used not 
                                                       only for shocks but for day-to-day livelihood. The median weekly remittance across all households is ₹107.14 (US$ 1.64), which is on average almost 
                                                       ₹206 (US$ 3.17)."),
                                                     p("Climate change has a heavier impact on coastal areas compared to other regions. The Sundarbans region is seeing the effects of these climate shocks
                                                       in numerous ways, including employment opportunities. Since farming and fishing are two of the biggest employment sectors in the region, the impact of 
                                                       climate change and cyclones is putting a strain on the working population. Due to this reason, many of the working population are seeking work in cities 
                                                       where wages are higher, and there are greater employment prospects. Since out-migration significantly impacts the households’ local incomes in the Sundarbans, 
                                                       the migrant workers send money back to their families. As threats from climate continue and only worsen, the Sundarbans region will see an increase in the lack 
                                                       of employment opportunities, potentially impacting the demographics by increasing the dependency ratio of the area with the working population migrating."),
                                                     p("With migration to different parts of West Bengal like Kolkata, one of the biggest cities in India, other cities in India, or overseas to Gulf countries 
                                                       or Southeast Asia, migrants have several options to send money back home to the Sundarbans. However, the most common way to send remittances is in-person 
                                                       when migrants return home to Sundarbans. This preference may be due to the additional costs associated with electronic banking/wire transfers, money orders, 
                                                       or mobile transactions. Over the year of weekly financial data, families usually use remittances for consumption expenses such as food or utility purchases. 
                                                       One-time “lump sum” transfers are also sent (seldomly) for expenses like tuition fees or assets needed to mitigate or cope with different shocks or unlikely circumstances."),
                                                     
                                                     
                                              ),
                                    
                                     
                                     
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         #tags$h2("Select/Deselect all"),
                                         pickerInput("village_rmt", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         pickerInput("event_choose_rmt", "Select Event:", choices = events_vector, selected = "Kharif Crop Preparation", 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         pickerInput("block_choose_rmt", "Select Administrative Block (For Remittances by Blocks):", choices = blocks_vector, selected = blocks_vector, 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         p(tags$small("Select:")),                                                         
                                         actionButton(inputId ="button8", label = "Map")
                                         
                                       ),
                                       
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Weekly Remittances",
                                                    h4(strong("Average Weekly Remittances"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("rmt", height = "500px")),
                                           tabPanel("Weekly Remittances by Blocks", 
                                                    h4(strong("Average Weekly Remittances by Administrative Block"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("rmt_block", height = "500px")),
                                           tabPanel("Method", 
                                                    h4(strong("Methods of Remittances Transfer"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("rmt_method", height = "500px")),
                                           tabPanel("Remittances Usage", 
                                                    h4(strong("Usage of Remittances"), align = "center", style = "margin: 13px;"),
                                                    plotOutput("rmt_purpose", height = "500px")),
                                           tabPanel("Weekly Remittances Table",
                                                    DT:: DTOutput("rmt_table")),
                                         )
                                       ), 
                                       
                                       
                                     ),
                            
                            
                            
                 )),
                 ## Shocks Tab --------------------------------------------
                 
                tabPanel("Shocks",
                                       fluidRow(style = "margin: 6px;", h1(strong("Shocks"), align = "center"), 
                                                column(4, 
                                                h4(strong("What Shocks are affecting this region (2009 – 2018)?"), align = "left"), 
                                                p("The Sundarbans area typically face yearly tropical events such as cyclones. However, the region is facing the effects of climate change as the frequency and intensity of cyclones have increased in the past decade. Specifically, the most devastating cyclones in the region occurred in 2007, 2009, 2019, 2020, and 2021. The impact of the cyclone in 2009 (Alia) is still evident as the majority of households reported experiencing a shock in 2009, even though this interview was done in 2018. Moreover, many families reported experiencing three shocks, with some reporting a high of four shocks in 2009. Shocks are any event, most likely negative, that can harm the households’ livelihoods. The majority of shocks families have experienced in the last decade are due to environmental events or the loss of various assets. The most frequent shocks households experienced during 2009-2018 were loss of home due to river erosion or cyclone, loss of livestock, crop loss, and forced migration due to flooding. The effects of shocks were similar across villages, as the average number of shocks was roughly three per year for most villages.", style = "padding-top:10px;", align = "justify")),
                                       # Show a plot of the generated plot
                                         column(8,
                                           tabsetPanel(
                                             tabPanel("Shocks Frequency", 
                                                      h4(strong("Frequency of Shocks"), align = "center", style = "margin: 13px;"),

                                                      plotlyOutput("shocks_all")),
                                             tabPanel("Total Shocks per Village", 
                                                      h4(strong("Average Number of Shocks per Village (2009-2018)"), align = "center", style = "margin: 13px;"),
                                                      plotOutput("shocks_village")),
                                             tabPanel("Yearly Shocks", 
                                                      h4(strong("Yearly Household Shocks"), align = "center", style = "margin: 13px;"),
                                                      plotlyOutput("shocks_by_year"))
                                                      )
                                                  ),
                                       ),
                         br(),
                         br(),
                                      fluidRow(style = "margin: 6px;", h1(strong("2009 Shocks"), align = "center"), 
                                               column(4, 
                                                      h4(strong("Coping Mechanisms"), align = "left"), 
                                              p("Given the disproportionate number of shocks households faced in 2009, we further examine the effects of these shocks. Cyclone Aila was a devastating force in 2009 and affected about 40 million people; washed away several thousand homes, took hundreds of lives, wounded more than seven thousand people, and caused almost billions of dollars’ worth of damage. Alia severely affected households’ livelihood as the most common shocks in 2009 were loss of home (river erosion/cyclone), loss of livestock, migration due to flooding), loss of vegetation, and crop loss.  Families in the Sundarbans obtained credit and diversified their livelihoods by pursuing other jobs to cope with the adverse effects of cyclone Alia. Notably, the most common coping mechanism was unconditional help from the government, followed by receiving support from friends or relatives. Often, families did nothing and tried to “weather the storm” until better times. Relocation is also common after devastating climate shocks since many homes are made from wood or materials easily wiped away by floods or high winds. Relocations in 2009 were usually not more than a month, but in some cases, a household may decide to move permanently. Most families typically relocate to a shelter/safe house within their villages during cyclones or when their homes are destroyed. These households temporarily relocate less frequently to Kolkata (the biggest city nearby) or other villages around the Sundarbans.")
                                                                            ),
                                        column(8,tabsetPanel(
                                          tabPanel("Frequency", 
                                                   h4(strong("Total Household Shocks (2009)"), align = "center", style = "margin: 13px;"),
                                                   plotlyOutput("shocks_plot_2009", height = "500")),
                                          tabPanel("Coping Mechanism", 
                                                   h4(strong("Coping Mechanism for Shocks (2009)"), align = "center", style = "margin: 13px;"),
                                                   plotlyOutput("cope_2009_plot", height = "600")),
                                          tabPanel("Relocation Status", 
                                                   h4(strong("Households Relocation Status for 2009 Shocks"), align = "center", style = "margin: 13px;"),
                                                   plotlyOutput("shock_relocation_2009_yn")),
                                          tabPanel("Relocation Areas", 
                                                   h4(strong("Relocation Areas due to 2009 Shocks"), align = "center", style = "margin: 13px;"),
                                                   plotlyOutput("shock_relocation_2009"))
                                                            )
                                               
                                               
                                              )
                                            )

                            
                          ),
                 
        
                 ## Tab Team --------------------------------------------
                 tabPanel("Team", 
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   align = "center",
                                   h1(strong("Team")),
                                   h4(strong("VT Data Science for the Public Good")),
                                   p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program held at the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural'), "and", a(href = 'https://ext.vt.edu/','Applied Economics and the Virginia Cooperative Extension Service.'),
                                     "In its third year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical
                                social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how 
                                information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply,
                                and our annual symposium, please visit", 
                                     a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Members")),
                                          img(src = "rippley.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/samantha-rippley-58846119b/', 'Samantha Rippley', target = '_blank'), "(M.S in Agriculture and Applied Economics, Virginia Tech)"),
                                          br(), 
                                          img(src = "das.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/nandini-das-390577104/', 'Nandini Das', target = '_blank'), "(Ph.D. in Economics, Virginia Tech)"),
                                          br(), 
                                          img(src = "Taj.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/taj-cole-83a738221', 'Taj Cole', target = '_blank'), "(B.S. in Environmental Economics: Management and Policy, Minoring in Data and Decisions, Virginia Tech)"),
                                          br(), 
                                          img(src = "Sid.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/siddarth-ravikanti-63374b207/', 'Siddarth Ravikanti', target = '_blank'), "(B.S. & B.A. Double-Majoring in Computational Modeling & Data Analytics and Political Science, Virginia Tech)"),
                                         p("", style = "padding-top:10px;") 
                                   ),
                                   column(6, align = "center",
                                          h4(strong("Faculty Advisor")),
                                          # img(src = "team-posadas.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "holmes.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://www.linkedin.com/in/chanita-holmes-385577234/", 'Dr. Chanita Holmes', target = '_blank'), "(Department of Agricultural and Applied Economics, Virginia Tech)"),
                                          br(),
                                          br(),
                                          br(),
                                          
                                   h4(strong("Project Stakeholders")), 
                                   #img(src = "anubhab.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                   p(a(href = 'https://www.linkedin.com/in/samantha-rippley-58846119b/', 'Dr. Anubhab Gupta', target = '_blank'), "(Assistant Professor, Department of Agriculture and Applied Economics, Virginia Tech)"),
                                  # br(), 
                                   #img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                   p(a(href = 'https://www.linkedin.com/in/aleksandr-michuda/', 'Dr. Aleksandr Michuda', target = '_blank'), "(Assistant Research Professor, Center for Data Science for Enterprise and Society, Cornell University)"),
                                  # br(), 
                                 #  img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                 p(a(href = 'https://www.researchgate.net/profile/Heng-Zhu-15', 'Dr. Heng Zhu', target = '_blank'), "(United Nations World Food Program, RAM)"),  
                                 p(a(href = 'https://www.linkedin.com/in/mikidoan/', 'Miki Doan', target = '_blank'), "(Ph.D. Candidate in Applied Economics, UC Davis)"),
                                 #  br(), 
                                  # img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                   p(a(href = 'https://www.linkedin.com/in/binoy-majumder-60703230/', 'Binoy Majumder', target = '_blank'), "(Sundarbans Field Team Lead, Independent Researcher, West Bengal, India)"),
                                  
                 ))),
                ## References Tab--------------------------------------------
                tabPanel("References", value = "overview",
                         fluidRow(style = "margin: 6px;", align = "justify",
                                  column(12, 
                                         h2(align = "center", strong("References")),
                                         p(tags$small(em('Alam, M. M., Hossain, M. A., &amp; Shafee, S. (2003). Frequency of bay of bengal cyclonic storms and depressions crossing different Coastal Zones. International Journal of Climatology, 23(9), 1119–1125. https://doi.org/10.1002/joc.927 '))),
                                         p(tags$small(em('CCSP (2008). Impacts of Climate Change and Variability on Transportation Systems and Infrastructure: Gulf Coast Study, Phase I. A Report by the U.S. Climate Change Science Program and the Subcommittee on Global Change Research. Savonis, M. J., V.R. Burkett, and J.R. Potter (eds.). Department of Transportation, Washington, DC, USA, 445 pp.'))),
                                         p(tags$small(em('Chandra, Ganesh & Sagar, R.. (2003). Fisheries in Sundarbans: Problems and Prospects. 10.2139/ssrn.2084014. '))),
                                         p(tags$small(em('Connect. Collaborate. Express | RoundGlass Living. (n.d.). Roundglass.com. Retrieved July 20, 2022, from https://roundglasssustain.com/photo-stories/honey-gatherers-sundarbans.'))),
                                         p(tags$small(em('Evan, Amato & Camargo, Suzana. (2011). A Climatology of Arabian Sea Cyclonic Storms. JOURNAL OF CLIMATE.'))),
                                         p(tags$small(em('Halder, Nirmol & Merchant, Andrew & Misbahuzzaman, Khaled & Wagner, Sven & Mukul, Sharif. (2021). Why some trees are more vulnerable during catastrophic cyclone events in the Sundarbans mangrove forest of Bangladesh?. Forest Ecology and Management. 490. 119117. 10.1016/j.foreco.2021.119117.'))),
                                         p(tags$small(em('Kabir, Kazi Ahmed & Saha, S B & Phillips, Michael. (2019). Aquaculture and Fisheries in the Sundarbans and Adjacent Areas in Bangladesh: Resources, Productivity, Challenges and Opportunities. 10.1007/978-3-030-00680-8_9.'))),
                                         p(tags$small(em('Kumar, Shubham & Lal, Preet & Kumar, Amit. (2020). Turbulence of tropical cyclone ‘Fani’ in the Bay of Bengal and Indian subcontinent. Natural Hazards.'))),
                                         p(tags$small(em('NRC (2010). Adapting to the Impacts of Climate Change. National Research Council. The National Academies Press, Washington, DC, USA.'))),
                                         p(tags$small(em('Pramanik, A., Sengupta, S., & Bhattacharyya, M. (2019, January 1). Chapter 5 - Microbial Diversity and Community Analysis of the Sundarbans Mangrove, a World Heritage Site (S. Das & H. R. Dash, Eds.). ScienceDirect; Academic Press. https://www.sciencedirect.com/science/article/pii/B9780128148495000058‌'))),
                                         p(tags$small(em('ReliefWeb. (2020, May 15). Bangladesh: Cyclone Bulbul final report - operation dref N° MDRBD023 - bangladesh. ReliefWeb. Retrieved July 20, 2022, from https://reliefweb.int/report/bangladesh/bangladesh-cyclone-bulbul-final-report-operation-dref-n-mdrbd023 '))),
                                         p(tags$small(em('Saha, Dipankar. (2015). Diversified Agriculture in Sundarbans. 10.13140/RG.2.1.4691.3446. '))),
                                         p(tags$small(em('Sundarbans - an overview | ScienceDirect Topics. (n.d.). Www.sciencedirect.com. https://www.sciencedirect.com/topics/agricultural-and-biological-sciences/sundarbans.'))),
                                         p(tags$small(em('Titumir, R. A. M. (n.d.). Sundarbans under threat. Prothomalo. Retrieved July 20, 2022, from https://en.prothomalo.com/environment/sundarbans-under-threat.'))),
                                         p(tags$small(em('UNESCO World Heritage Centre. (2018). The Sundarbans. Unesco.org. https://whc.unesco.org/en/list/798/.'))),
                                         p(tags$small(em('Velmurugan, A., Ambast, S. K., Swarnam, T. P., Burman, D., Mandal, S., & Subramani, T. (2018, January 1). Chapter 21 - Land Shaping Methods for Climate Change Adaptation in Coastal and Island Region (C. Sivaperuman, A. Velmurugan, A. K. Singh, & I. Jaisankar, Eds.). ScienceDirect; Academic Press. https://www.sciencedirect.com/science/article/pii/B9780128130643000211.'))),
                                         p(tags$small(em('Worlddata.info. (n.d.). Most recent cyclones in India. Worlddata.info. Retrieved July 19, 2022, from https://www.worlddata.info/asia/india/cyclones.php.'))),
                                         p(tags$small(em('USGCRP (2014). Moser, S. C., M. A. Davidson, P. Kirshen, P. Mulvaney, J. F. Murley, J. E. Neumann, L. Petes, and D. Reed, 2014: Ch. 25: Coastal Zone Development and Ecosystems. Climate Change Impacts in the United States: The Third National Climate As­sessment, J. M. Melillo, Terese (T.C.) Richmond, and G. W. Yohe, Eds., U.S. Global Change Research Program, , 579-618.')))
                                             ),
            
                                  )),
                 
                 inverse = T)



# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  

  
  #titles 
  output$result2 <- renderText({
    paste(input$agedrop)
  })  
  
  output$result1 <- renderText({
    paste(input$ocudrop)
  })  
  
  output$result <- renderText({
    paste(input$findrop)
  })  
  
  
  #note 
  
  
  output$result7 <- renderText({
    if (ageVar() == "Mean Years of Education for Head of Households") {
      paste("")
    }
    else if (ageVar() == "Households that Live Below Poverty Line (₹204) per week") {
      paste("(Live below the poverty line)")
    }
    else if (ageVar() == "Household Heads Marital Status") {
      
    }
  })
  
  
  output$result6 <- renderText({
    if (ageVar() == "Mean Years of Education for Head of Households") {
     paste("Mean: 5.30 Years | Median: 5.00 Years")
    }
    else if (ageVar() == "Households that Live Below Poverty Line (₹204) per week") {
      paste("Mean: 10.3 Households | Median: 10 Households")
    }
    else if (ageVar() == "Household Heads Marital Status") {
      
    }
  })
  
  output$result4 <- renderText({
    if (finVar() == "Number of Households that Own a Business") {
      paste("Mean: 3.60 households | Median: 3.00 households | (Own a Business)")
    }
    else if (finVar() == "Proportion of Households Owning Assets") {
      paste("")
    }
    
    else if (finVar() == "Income vs Remmitances (October 2018 - November 2019)") {
      paste("Mean: INR 4214 | Median: INR 3800")
    }
    else if (finVar() == "Average Monthly Salary per Household by Village")  {
      paste("")
    }
    else if (finVar() == "Number of Times Households Saved in Prior Year") {
      paste("")
    }
    
    else if (finVar() == "Percentage of Household with Migrant Workers") {
      paste("")
    }
    
  })  
  
  output$result5 <- renderText({ 
  if (ocuVar() == "Primary Occupation for Head of Households") {
    paste("*Note: Missing bar indicates zero value for occupation")
  } 
  else if (ocuVar() == "Secondary Occupation for Head of Households") {
    paste("*Note: Missing bar indicates zero value for occupation")
  }
  else if (ocuVar() == "Percentage of Households Involved in Agricultural Farming") {
    paste("Mean: 61.92% | Median: 60% | (Involved in Farming)")
  }
  else if (ocuVar() == "Average Amount of Land Owned by Village") {
    paste("Mean: 48.37 Kathas | Median: 40 Kathas")
    
  }
  else if (ocuVar() == "Total Land Fallowed (Kathas)") {
    paste("*Note: Data missing for some villages or missing bar means zero land was fallowed for village | Mean: 41.7 Kathas | Median: 39.5 Kathas")
  }
  else if (ocuVar() == "Average Job Duration for Head of Household") {
    paste("Mean: 8.02 Months | Median: 8.21 Months")
  }
  
})
  
  

  #overview photos 
  index <- reactiveVal(1)
  
  observeEvent(input[["previous"]], {
    index(max(index()-1, 1))
  })
  observeEvent(input[["next"]], {
    index(min(index()+1, length(imgs)))
  })
  
  output$image <- renderImage({
    x <- imgs[index()] 
    list(src = x, alt = "alternate text", width = "100%", align = "right")
  }, deleteFile = FALSE)
  
  # Events----------------
  filtered_event_cs <- reactive({
    event_periods %>% 
      filter(Events %in% input$event_choose_cs)
  })
  
  filtered_event_cs_food <- reactive({
    event_periods %>% 
      filter(Events %in% input$event_choose_cs_food)
  })
  
  filtered_event_cs_nonfood <- reactive({
    event_periods %>% 
      filter(Events %in% input$event_choose_cs_nonfood)
  })
  
  filtered_event_rmt <- reactive({
    event_periods %>% 
      filter(Events %in% input$event_choose_rmt)
  })
  
  filtered_event_exp <- reactive({
    event_periods %>% 
      filter(Events %in% input$event_choose_exp)
  })
  
  filtered_event_inc <- reactive({
    event_periods %>% 
      filter(Events %in% input$event_choose_inc)
  })
  
  filtered_event_borr <- reactive({
    event_periods %>% 
      filter(Events %in% input$event_choose_borr)
  })
  
  filtered_event_borr_count <- reactive({
    event_periods %>% 
      filter(Events %in% input$event_choose_borr_count)
  })
  
  
  #borrowing tab-------------------------
  
  output$datapls <- renderTable({
    pls %>% dplyr::select(!!!input$villages)
  }, rownames = TRUE)
  
  output$datapls <- renderPlot({
    ggplot(pls, aes(x =!!input$village, y = village)) + geom_bar(stat= "identity")
  })
  
  
  #borrowing amount
  
  filtered_bramt <- reactive({
    bramt %>%
      filter(village %in% input$village_bramt)
  })
  # Plot
  output$bor <- renderPlot({
    ggplot(filtered_bramt(), aes(x=week_num, y=br_amt, color = village, na.rm = T)) +
      geom_line() +
      theme_classic()+
      labs(color = "Villages", caption = "Mean: INR 2703.12  |  Median: INR 600.00") + 
      xlab("Date") +
      ylab("Total Weekly Borrowing (INR)")+
      theme(plot.caption = element_text(size = 12))+
      scale_y_discrete(breaks = c(0,10000,20000,30000,40000,50000), limits = c(0:50000)) +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) +
      scale_color_brewer(palette = "Paired") +
      #theme(legend.position = "none")+
      geom_rect(data = filtered_event_borr(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
  })  
  
  # borrowing count 
  filtered_dbr <- reactive({
    dbr %>%
      filter(village %in% input$village_bramt)
  })
  # Plot
  output$borr <- renderPlot({
    ggplot(filtered_dbr(), aes(x=week_num, y=d_br, color = village, na.rm=TRUE)) +
      geom_line() +
      labs(caption = "Mean: 22%  |  Median: 0.00", color = "Villages") +
      theme(plot.caption = element_text(size = 12))+
      xlab("Date") +
      ylab("Number of Households")+
      theme_classic()+
      scale_y_discrete(breaks = c(0,10,20,30), limits = c(0:30)) +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) +
      scale_color_brewer(palette = "Paired") +
      #theme(legend.position = "none")+
      geom_rect(data = filtered_event_borr(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
  })  
  
  #borrowing purpose ----------------------
  
  output$purpplot <- renderPlot({
    ggplot(dfpurp, aes(x= reorder(A,B), y = B, fill = A)) + geom_col() + 
      coord_flip()+
      labs(fill = "") + 
      xlab("") +
      ylab("Amount Borrowed (INR)")+
      theme_classic() +
      scale_fill_brewer(palette = "Paired") +
      theme(legend.position = "none", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) 
  })
  
  
  # Filtered consumption by group
  
  filtered_cs_food <- reactive({
    avg_cs_food %>% 
      filter(village %in% input$village_cs_food)
  })
  
  filtered_cs_food_staple <- reactive({
    avg_cs_food %>% 
      filter(village %in% input$village_cs)
  })
  
  filtered_cs_meats <- reactive({
    avg_cs_food %>% 
      filter(village %in% input$village_cs)
  })
  
  filtered_cs_other <- reactive({
    avg_cs_food %>% 
      filter(village %in% input$village_cs)
  })
  
  filtered_cs_block <- reactive({
    cs_block %>% 
      filter(Block %in% input$block_choose_cs)
  })
  

  output$cs_staple <- renderPlot({
    ggplot(filtered_cs_food_staple(), aes(x = week, y = `Staple Items`, color = village)) +
      geom_line()+
      theme_classic()+
      #ggtitle("Average Weekly Expenditure on Staple Items ")+
      labs(x = "Date", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: INR 463.87  |  Median: INR 431.20 ")+
      scale_y_discrete(breaks = c(0,250,500,750,1000,1250), limits = c(0:1250)) + 
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_brewer(palette = "Paired")
    
  })
  
  output$cs_meats <- renderPlot({
    ggplot(filtered_cs_meats(), aes(x = week, y = `Meats`, color = village))+
      geom_line()+
      theme_classic()+
      #ggtitle("Average Weekly Expenditure on Meat")+
      labs(x = "Date", y = "Average Weekly Staple Expenditure (INR)", color = "Villages", caption = "Mean: INR 158.97  |  Median: INR 431.20")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      scale_y_discrete(breaks = c(0,200,400), limits = c(0:400)) + 
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_brewer(palette = "Paired")
  })
  
  output$cs_other <- renderPlot({
    ggplot(filtered_cs_other(), aes(x = week, y = `Other`, color = village))+
      geom_line() +
      theme_classic()+
      #ggtitle("Average Weekly Expenditure on 'Other' Items")+
      labs(x = "Date", y = "Average Weekly Staple Expenditure (INR) ", color = "Villages", caption = "Mean: INR 113.75  |  Median: INR 111.94")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      scale_y_discrete(breaks = c(0,200,400,600), limits = c(0:600)) + 
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_brewer(palette = "Paired")
    
  })
  
  
  #map button -----------------------------------------------------
  
  output$map3 <- renderLeaflet({
    map_leaflet3
  })    
  
  observeEvent(input$button, {
    showModal(modalDialog(
      img(src='map5.png', height = "258px", align = "center"),
      easyClose = TRUE,
      footer = NULL
    ))
    
  })  
  
  observeEvent(input$button1, {
    showModal(modalDialog(
      img(src='map5.png', height = "258px", align = "center"),
      easyClose = TRUE,
      footer = NULL
    ))
    
  })    
  
  observeEvent(input$button2, {
    showModal(modalDialog(
      img(src='map5.png', height = "258px", align = "center"),
      easyClose = TRUE,
      footer = NULL
    ))
    
  })    
  
  observeEvent(input$button3, {
    showModal(modalDialog(
      img(src='map5.png', height = "258px", align = "center"),
      easyClose = TRUE,
      footer = NULL
    ))
    
  })     
  
  observeEvent(input$button4, {
    showModal(modalDialog(
      img(src='map5.png', height = "258px", align = "center"),
      easyClose = TRUE,
      footer = NULL
    ))
    
  })    
  
  observeEvent(input$button5, {
    showModal(modalDialog(
      img(src='map5.png', height = "258px", align = "center"),
      easyClose = TRUE,
      footer = NULL
    ))
    
  })
  
  observeEvent(input$button6, {
    showModal(modalDialog(
      img(src='map5.png', height = "258px", align = "center"),
      easyClose = TRUE,
      footer = NULL
    ))
    
  })   
  
  
  observeEvent(input$button7, {
    showModal(modalDialog(
      img(src='map5.png', height = "258px", align = "center"),
      easyClose = TRUE,
      footer = NULL
    ))
    
  })   
  
  observeEvent(input$button8, {
    showModal(modalDialog(
      img(src='map5.png', height = "258px", align = "center"),
      easyClose = TRUE,
      footer = NULL
    ))
    
  })   

  #sociodemo tabset -----------------------------------------------------
  ageVar <- reactive({
    input$agedrop
  }) 
  
   output$ageplo <- renderLeaflet({
      map_leaflet2
    })
  
  output$ageplot <- renderPlotly({
    if (ageVar() == "Mean Years of Education for Head of Households") {
      splot <- ggplot(by_villagemore, aes(x = "", y= head_edu, fill = village)) +
        geom_bar(width = 1, stat = "identity", hoverinfo = "text", aes(text = paste("Education: ", round(head_edu, 2), "<br>Village: ", village))) +
        facet_wrap(~village, ncol = 5) +
        #theme_minimal()+
        labs(x = NULL, y = "Years of Education") +
        theme(legend.position="none", strip.text.x = element_text(size = 9)) + scale_fill_brewer(palette = "Paired")
      ggplotly(splot, tooltip = c("text"))
    }
    else if (ageVar() == "Households that Live Below Poverty Line (₹204) per week") {
      village_pl_count_plot <- ggplot(dat_pl, aes(x= Village, y = Households, fill = Key)) + 
        geom_col(position = 'stack', hoverinfo = "text", aes(text = paste("Village:", Village,"<br>Key: ", Key, "<br>Percentage:", Percentage,"%","<br>Total Households: ", Households ))) + 
        labs(x= "", y = "Total Households", fill = "") + 
        theme_classic()+ 
        coord_flip()
      ggplotly(village_pl_count_plot, tooltip = c("text"))
    }
    else if (ageVar() == "Household Heads Marital Status") {
      marplot <- ggplot(countmar, aes(x = head_married, y = n, fill = Gender)) +
        geom_col(hoverinfo = "text", aes(text = paste(""))) +
        labs(x = "Not Married                                         Married", y = "Total Household Head", fill = "") +
        scale_x_discrete() + theme(legend.title=element_blank())+
        theme_classic()
      ggplotly(marplot, tooltip = c("text"))
    }
  })
  
  
  #livelihood tabset -----------------------------------------------------
  ocuVar <- reactive({
    input$ocudrop
  })
  
  output$ocuplot <- renderPlotly({
    if (ocuVar() == "Primary Occupation for Head of Households") {
      pocuplot <- ggplot(countv, aes(x = job, y = n, fill = village)) +
        geom_col(hoverinfo = "text", aes(text = paste("Village:", village, "<br>Households: ", n))) +
        scale_x_discrete(limits = factor(1:16), labels = c("1" = "Agricultural wage worker","2" =  "Livestock worker", "3" = "Farmer", "4" = "Casual labor","5" =  "Construction/brick labor","6" =  "Gleaning/foraging","7" =  "Fisherman","8" =  "Fishery worker", "9" = "Factory worker" , "10" = "Household help" ,"11" =  "Transport related work","12" =  "Own business", "13" = "Service Work (NGO, gov,etc.)", "14" = "NREGA","15" =  "Housewife","16" =  "Other")) +
        coord_flip() +
        theme_classic() +
        labs(x = "", y = "Total Households", fill = "Select Village:") + scale_fill_brewer(palette = "Paired")
      ggplotly(pocuplot, tooltip = c("text"))
    } 
    else if (ocuVar() == "Secondary Occupation for Head of Households") {
      socplot <- ggplot(scountv, aes(x = job, y = n, fill = village)) +
        geom_col(hoverinfo = "text", aes(text = paste("Village:", village, "<br>Household: ", n))) +
        scale_x_discrete(limits = factor(1:16), labels = c("1" = "Agricultural wage worker","2" =  "Livestock worker", "3" = "Farmer", "4" = "Casual labor","5" =  "Construction/brick labor","6" =  "Gleaning/foraging","7" =  "Fisherman","8" =  "Fishery worker", "9" = "Factory worker" , "10" = "Household help" ,"11" =  "Transport related work","12" =  "Own business", "13" = "Service Work (NGO, gov,etc.)", "14" = "NREGA","15" =  "Housewife","16" =  "Other")) +
        coord_flip() +
        theme_classic() +
        labs(x = "", y = "Total Households", fill = "Select Village:") + scale_fill_brewer(palette = "Paired")
      ggplotly(socplot, tooltip = c("text"))
    }
    else if (ocuVar() == "Percentage of Households Involved in Agricultural Farming") {
      agfaplot <- ggplot(agfa, aes(forcats::fct_rev(village),prop_farm*100, fill = village)) + 
        geom_col(hoverinfo = "text", aes(text = paste("Village:", village,"<br>Percentage: ", round(prop_farm*100, 2), "%"))) + 
        labs(x = "", y = "Percentage", title = "") + coord_flip() + theme(legend.position = "none") + scale_fill_brewer(palette = "Paired")+
        theme_classic()
      ggplotly(agfaplot,tooltip = c("text"))
    }
    else if (ocuVar() == "Average Amount of Land Owned by Village") {
      mean_land_plot <- ggplot(land_stats, aes(x = forcats::fct_rev(villages), y = mean_land_value, fill = villages)) +
        geom_col(hoverinfo = "text", aes(text = paste("Village:", villages,"<br>Mean Land Owned: ", round(mean_land_value,3)))) +
        coord_flip() + theme(legend.position = "none") +
        labs(x = "", y = "Land Owned (Kathas)") + scale_fill_brewer(palette = "Paired")
      ggplotly(mean_land_plot, tooltip = c("text"))
      
    }
    else if (ocuVar() == "Total Land Fallowed (Kathas)") {
      land_fallow_plot <- ggplot(land_fallow, aes(x = forcats::fct_rev(village), y = sum, fill = village)) +
        geom_col(hoverinfo = "text", aes(text = paste("Village:", villages,"<br>Land Fallowed: ", sum)))+
        theme(legend.position = "none") +
        labs(x = "", y = "Total Land Fallowed", caption = "*Note: For missing bars, villages did not have any land fallowed")+
        coord_flip() + scale_fill_brewer(palette = "Paired")
      ggplotly(land_fallow_plot, tooltip = c("text"))
    }
    else if (ocuVar() == "Average Job Duration for Head of Household") {
      job_duration_plot <- ggplot(job_duration_summary, aes(x = forcats::fct_rev(villages), y = job_duration_avg, fill = villages)) +
        geom_point(hoverinfo = "text", aes(text = paste("Village:", villages,"<br>Average Job Duration: ", round(job_duration_avg,2), "months"))) + 
        coord_flip()+
        labs(x= "", y = "Average Job Duration (Months)")+
        ggtitle("") +
        theme(legend.position = "none") + scale_fill_brewer(palette = "Paired")
      ggplotly(job_duration_plot, tooltip = c("text"))
    }
    
  })
  
  #financial  tabset -----------------------------------------------------
  finVar <- reactive({
    input$findrop
  })
  
  output$finplot <- renderPlotly({
    if (finVar() == "Number of Households that Own a Business") {
      village_bus_count_plot <- ggplot(dat_bus, aes(x= Village, y = households, fill = key)) + geom_col(position = 'stack') +
        geom_col(position = 'stack', hoverinfo = "text", aes(text = paste("Village:", Village,"<br>Key: ", key, "<br>Percentage:", percentage,"%","<br>Total Households: ", households ))) + 
        labs( x= "", y = "Total Households", fill = "") + 
        theme_classic() + 
        #ggtitle("Households That Own a Business") +
        coord_flip()
      ggplotly(village_bus_count_plot, tooltip = c("text"))
      
    }
    else if (finVar() == "Proportion of Households Owning Assets") {
      assetplot <- ggplot(assets_long, aes(property, proportion, fill = property, text = paste(""))) + 
        geom_col(hoverinfo = "text", aes(text = paste("Property: ", property, "<br>Proportion: ", proportion))) + 
        labs(x = "Asset", y = "Proportion" , title = "") + 
        theme(legend.position = "none") +
        rotate_x_text(angle = 33, size = rel(1)) +
        scale_fill_brewer(palette = "Paired")    
      
      ggplotly(assetplot, tooltip = c("text"))
    }
    
    else if (finVar() == "Income vs Remmitances (October 2018 - November 2019)") {
      rem_inc <- ggplot(baseline.summary, aes(rmt_total, full_inc, color= village)) +
        geom_point(data=baseline.summary, shape=17, size=3, hoverinfo = "text", 
                   aes(text = paste("Village: ", village, "<br>Total Remmitance: ", round(rmt_total,2), "<br>Total Income: ", round(full_inc, 2)))) +
        labs(x="Average Weekly Remmitances", y="Average Weekly Income", color="Villages") +
        theme_classic()+
        ggtitle("") + scale_color_brewer(palette = "Paired") +coord_flip() 
      ggplotly(rem_inc, tooltip = c("text"))
    }
    else if (finVar() == "Average Monthly Salary per Household by Village")  {
      salplot <- ggplot(m_salary, aes(village, round(avg_salary, digits = 2), fill = village)) + 
        geom_col(hoverinfo = "text", aes(text = paste("Average Salary:", round(avg_salary,2), "₹"))) + 
        labs(x = "", y = "Indian Rupees ₹" ,title = "", fill = "") +
        theme(legend.position = "none") + scale_fill_brewer(palette = "Paired") +
        rotate_x_text(angle = 33, size = rel(1))
      ggplotly(salplot, tooltip = c("text"))
    }
    else if (finVar() == "Number of Times Households Saved in Prior Year") {
      savplot <- ggplot(nbsavcount, aes(x = nb_put_saving, y = n, fill = "red")) +
        geom_point(hoverinfo = "text", aes(text = paste("Occurances: ", n)))+ 
        labs(x = "Number of Times Household Saved ", y = " Number of Occurances") + theme(legend.position = "none")
      ggplotly(savplot, tooltip = c("text"))
    }
    
    else if (finVar() == "Percentage of Household with Migrant Workers") {
      migplot <- ggplot(migrant_prop, aes(forcats::fct_rev(village), migrant_proportion, fill = village)) + 
        geom_col(hoverinfo = "text", aes(text = paste("Percentage: ", round(migrant_proportion, 2), "%"))) + theme(legend.position = "none") + 
        labs(x = "", y = "Percentage", title = "", fill = "") + coord_flip() +
        scale_fill_brewer(palette = "Paired")
      ggplotly(migplot, tooltip = c("text"))
    }
    
    
  })
  
  # High Frequency Data Output------------------- 
  
  # rmt plot output
  # Filter by input
  filtered_rmt <- reactive({
    rmt_mean %>%
      filter(village %in% input$village_rmt)
  })
  # Plot
  output$rmt <- renderPlot({
    ggplot(filtered_rmt(), aes(x = week
                               , y = avg_rmt, color = village)) + 
      geom_line() +
      theme_classic() +
      labs(x = "Date", y = "Average Weekly Remittance (INR)", caption = "Mean: INR 205.61  |  Median: INR 107.14", color = "Villages") +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) + 
      scale_color_brewer(palette = "Paired")+
      scale_y_discrete(breaks = c(0,1000,2000,3000,4000,5000), limits = c(0:5000)) +
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_rmt(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
  })
  
  filtered_rmt_block <- reactive({
    rmt_block %>% 
      filter(Block %in% input$block_choose_rmt)
  })
  
  output$rmt_block <- renderPlot({
    ggplot(filtered_rmt_block(), aes(x = week , y = block_avg_rmt, color = Block)) +
      geom_line() +
      theme_classic() +
      labs(x = "Date", y = "Average Weekly Remittances (INR)", color = "Blocks", caption = "Mean: INR 13549.16  |  Median: INR 10400.00") +
      theme(plot.caption = element_text(size = 12))+
      scale_y_discrete(breaks = c(0,500,1000,1500,2000,2500), limits = c(0:2500)) +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      geom_rect(data = filtered_event_rmt(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
  })
  
  # Render rmt table
  output$rmt_table <- DT::renderDT({
    avg_rmt
  })
  # Render method plot
  output$rmt_method <- renderPlot({
    rmt_method_plot
  })
  # Render map 
  output$map_leaflet <- renderLeaflet({
    map_leaflet
  })
  
  # Render purpose plot
  output$rmt_purpose <- renderPlot({
    rmt_purpose_plot
  })
  # exp plot ouput
  # Filter by input
  filtered_exp <- reactive({
    exbyvil %>% 
      filter(village %in% input$village_exp)
  })
  
  filtered_exp_block <- reactive({
    exp_block %>% 
      filter(Block %in% input$block_choose_exp)
  })
  
  # Plot
  output$exp <- renderPlot({
    ggplot(filtered_exp(), aes(x=week_num, y=total_spending, color = village, na.rm=TRUE)) +
      geom_line() +
      labs(x="Date", y="Average Weekly Expenditure (INR)", caption = "Mean: INR 1982.77  |  Median: INR 1832.61", color = "Villages") +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) +
      scale_y_discrete(breaks = c(0,2000,4000,6000,8000), limits = c(0:8000)) + 
      scale_color_brewer(palette = "Paired")+
      theme_classic()+
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_exp(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
    
  })
  
  output$exp_block <- renderPlot({
    ggplot(filtered_exp_block(), aes(x = week , y = block_avg_exp, color = Block)) +
      geom_line() +
      theme_classic() +
      labs(x = "Date", y = "Average Weekly Expenditure (INR)", color = "Blocks", caption = "Mean: INR 117784.90  |  Median INR 111505.00") +
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_exp(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      scale_y_discrete(breaks = c(0,1000,2000,3000,4000,5000), limits = c(0:5000))  
      
  })
  

  # Render exp table 
  output$exp_table <- DT::renderDT({
    expend_table
  })
  # Render income plot output
  # Filter by input
  filtered_inc <- reactive({
    avg_tot_inc %>% 
      filter(village %in% input$village_inc)
  })
  
  
  # Plot
  output$inc <- renderPlot({
    ggplot(filtered_inc(), aes(week, avg_inc, color = village)) + 
      geom_line() + 
      labs(x = "Date", y = "Average Weekly Income (INR)", color = "Village",
           caption = "Mean: INR 1395.61  |  Median: INR 1341.82") + 
      scale_color_brewer(palette = "Paired")+
      theme_classic()+
      theme(plot.caption = element_text(size = 12))+
      scale_y_discrete(breaks = c(0,2000,4000,6000), limits = c(0:6000)) + 
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) + 
      geom_rect(data = filtered_event_inc(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
  })
  
  filtered_malefemaleinc <- reactive({
    malefemale_inc  %>% 
      filter(village %in% input$village_inc)
  })
  
  output$malefemaleinc <- renderPlot({
    ggplot(filtered_malefemaleinc(), aes(x = week,y = !!input$Gender, color = village)) + geom_line() + 
      labs(x = "Date", y = "Average Weekly Income (INR)", color = "Village", caption = "Male - Mean: INR 1065.54  |  Median = INR 642.5   Female - Mean: INR 96.60  |  Median: INR 0.00 ") +
      theme_classic()+
      theme(plot.caption = element_text(size = 12))+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), 
                       limits = c(10:40)) + scale_color_brewer(palette = "Paired")+
      geom_rect(data = filtered_event_inc(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
  })
  
  
  filtered_totalinc <- reactive({
    totinc  %>% 
      filter(village %in% input$village_inc)
  })
  
  filtered_inc_block <- reactive({
    inc_block %>% 
      filter(Block %in% input$block_choose_inc)
  })
  
  output$inc_block <- renderPlot({
    ggplot(filtered_inc_block(), aes(x = week, y = block_avg_inc, color = Block)) +
      geom_line() +
      theme_classic() +
      labs(x = "Date", y = "Average Weekly Income (INR)", color = "Blocks", caption = "Mean: INR 83097.02  |  Median: INR 81977.00") +
      theme(plot.caption = element_text(size = 12))+
      scale_y_discrete(breaks = c(0,1000,2000,3000,4000,5000), limits = c(0:5000)) +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      geom_rect(data = filtered_event_inc(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
  })
  
  output$totalinc <- renderPlot({
    qplot(x=week_num, y=inc_total, color = village,
          data=filtered_totalinc(), na.rm=TRUE,
          xlab="Date", ylab="Total Income (INR)", geom = "line") +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) + scale_color_brewer(palette = "Paired")+
      geom_rect(data = filtered_event_inc(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
  })
  
  
  
  
  #Render inc table
  output$inc_table <- DT::renderDT({
    avg_inc_table
  })
  
  
  # Consumption ----------------------- 
  
  # Filtered cs expenditure plot
  filtered_cs_avg <- reactive({
    cs_avg %>% 
      filter(village %in% input$village_cs)
  })
  # consumption exp plot
  filtered_event <- reactive({
    event_periods %>% 
      filter(Events %in% input$event_choose)
  })
  
  output$cs_exp <- renderPlot({
    ggplot(filtered_cs_avg(), aes(x = week, y = avg_cs , color = village)) +
      geom_line() +
      theme_classic()+
      labs(x = "Date", y = "Average Weekly Expenditure (INR)", caption = "Mean: INR 766.13  |  Median: INR 731.68", color = "Villages")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      scale_y_discrete(breaks = c(0,500,1000,1500,2000), limits = c(0:2000)) + 
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_brewer(palette = "Paired")
  })
  
  # Filtered cs items
  filtered_cs_avg_items <- reactive({
    cs_avg_items %>% 
      filter(village %in% input$village_cs)
  })
  
  # cs items plot
  
  output$cs_item <- renderPlot({
    ggplot(filtered_cs_avg_items(), aes(x = week, y = avg_item, color = village))+
      geom_line() +
      theme_classic()+
      labs(x = "Date", y = "Weekly Items Purchased", color = "Villages", caption = "Mean: 7.20  |  Median: 7.20")+
      theme(plot.caption = element_text(size = 12))+
      scale_y_discrete(breaks = c(0,2,4,6,8,10), limits = c(0:10)) + 
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_brewer(palette = "Paired")
  })
  
  output$cs_block <- renderPlot({
    ggplot(filtered_cs_block(), aes(x = week , y = block_avg_cs, color = Block)) +
      geom_line() +
      theme_classic() +
      labs(x = "Date", y = "Average Weekly Consumption (INR)", color = "Blocks", caption = "Mean: INR 44745.59  |  Median: INR 43250.00") +
      theme(plot.caption = element_text(size = 12))+
      scale_y_discrete(breaks = c(0,250,500,750,1000,1250), limits = c(0:1250)) + 
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))
  })
  

  
  filtered_non_food_cs <- reactive({
    non_food_cs %>% 
      filter(village %in% input$village_cs_nonfood)
  })
  
  output$nonfood_plot <- renderPlot({
    ggplot(filtered_non_food_cs(), aes(x = week, y = !!input$nonfood_group, color = village)) +
      geom_line()+
      theme_classic()+
      labs(x = "Date", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: INR 882.22  |  Median: INR 769.75")+
      theme(plot.caption = element_text(size = 12))+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      geom_rect(data = filtered_event_cs_nonfood(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_brewer(palette = "Paired")
    
  })
  
  filtered_nf <- reactive({
    nonfoodcs_block %>% 
      filter(Block %in% input$block_choose_nf)
  })
  
  output$nf_block_plot <- renderPlot({
    ggplot(filtered_nf(), aes(x = week , y = block_avg_nf, color = Block)) +
      geom_line() +
      theme_classic() +
      labs(x = "Date", y = "Average Weekly Consumption (INR)", color = "Blocks", caption = "Mean: INR 53003.67  |  Median: INR 48385.00") +
      theme(plot.caption = element_text(size = 12))+
      scale_y_discrete(breaks = c(0,1000,2000,3000,4000), limits = c(0:4000)) +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      geom_rect(data = filtered_event_cs_nonfood(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
  })


  #Event Filtered
  
  filtered_event <- reactive({
    event_periods %>% 
      filter(Events %in% input$event_choose)
  })
  
  ###Shock plot output  -----------------------------------------------------
  
  #shock_all plot output
  output$shocks_all <- renderPlotly({
    ggplotly(shocks_all, tooltip = c("text"))
  })
  
  #shock_village plot output
  output$shocks_village <- renderPlot({
    shocks_village
  })
  
  #shock_by_year plot output
  output$shocks_by_year <- renderPlotly({
    ggplotly(shocks_by_year, tooltip = c("text"))
  })
  
  #shock_2009 plot output
  output$shocks_plot_2009 <- renderPlotly({
    ggplotly(shocks_plot_2009, tooltip = c("text"))
  })
  
  #cope_2009 plot output
  output$cope_2009_plot <- renderPlotly({
    ggplotly(cope_2009_plot, tooltip = c("text"))
  })
  
  #shock_relocation_2009_yn plot output
  output$shock_relocation_2009_yn <- renderPlotly({
    ggplotly(shock_relocation_2009_yn, tooltip = c("text"))
  })
  
  #shock_relocation_2009 plot output
  output$shock_relocation_2009 <- renderPlotly({
    ggplotly(shock_relocation_2009, tooltip = c("text"))
  })
  
  
}


shinyApp(ui = ui, server = server)