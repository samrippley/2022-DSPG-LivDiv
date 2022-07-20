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

grouped <- baseline %>% group_by(village) %>% summarize(prop_farm = sum(farm_yn)/n())



# household asset data 

villages <- c("Amrabati","Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar","Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur") 
assets <- baseline %>% select(contains("asset")) %>% select(contains("num"))  %>% 
  summarize(Stove = sum(asset_stove_num)/n(), Bike = sum(asset_bike_num)/n(), 
            `Water Pump` = sum(asset_waterpump_num)/n(), `Solar Panel` = sum(asset_solarpanel_num)/n(), 
            Bed = sum(asset_bed_num)/n(), Fridge = sum(asset_fridge_num)/n(), Almirah = sum(asset_almirah_num)/n(), 
            PC = sum(asset_pc_num)/n(), TV = sum(asset_tv_num)/n(), Phone = sum(asset_mobile_num)/n(), 
            `Water Filter` = sum(asset_waterfilter_num)/n())

assets_long <- gather(assets, property, percentage, Stove:`Water Filter`)
assets_long["percentage"] = assets_long["percentage"]*100
assets_long["percentage"] <- round(assets_long$percentage, digits = 2)


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

# participate in ag data 

grouped <- baseline %>% group_by(village) %>% summarize(prop_farm = sum(farm_yn)/n())


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

migrant_prop <- baseline %>% group_by(village) %>% summarize(migrant_proportion = (mean(hh_migrant_prop)*100))

# savings data 
nbsav <- baseline %>% 
  group_by(village) %>% 
  summarize_at(c("nb_put_saving"), mean, na.rm=TRUE)

nbsavcount <- baseline %>% 
  group_by(village) %>% 
  count(nb_put_saving) 

#land holding data 

land <- baseline %>% select(village, no_farm_reason) %>% na.omit(no_farm_reason)
land$no_farm_reason <- as.numeric(as.factor(land$no_farm_reason))

# salary data 

m_salary <-  baseline %>% group_by(village) %>% select(job1_salary1) %>% summarize(avg_salary = sum(job1_salary1, na.rm = TRUE)/n())

#crops data

grouped <- baseline %>% group_by(village) %>% summarize(prop_farm = sum(farm_yn)/n())

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
`Key` <- rep(c("Live Above ₹240", "Live Below ₹240"), 2)
`Households` <- c(17,11,20,10,32,18,19,9,14,16,17,11,18,10,23,5,21,6,21,7)
Percentage<- c("60", "40", "67", "33", "64","36","68","32","53",
               "47","60","40","64","36","82","18","77","23","75","25" )
Village <- forcats::fct_rev(Village)
dat_pl <- data.frame(`Village`, `Key`, `Households`, Percentage)
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
  labs(x = "", y = "Total") +
  theme_classic() +
  coord_flip()+
  #ggtitle("Method of Receiving Remittances")+
  geom_text(aes(label = method_values), size = 3) + scale_fill_viridis_d()


# leaflet data --------------------------------------------------------------------

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
  addAwesomeMarkers(~lon, ~lat, label = ~as.character(Village.Na), labelOptions =  ,icon=icons, data=village) %>%
addCircles(lat = 21.687, lng = 88.0591,
           radius=6000, color = 'blue', opacity = 1) %>%
  addCircles(lat = 22.227912, lng = 89.00475,
             radius=6000, color = 'red') %>%
 addCircles(lat = 21.8619, lng = 88.43877,
            radius=6000, color = 'purple') %>%
  addCircles(lat = 22.1396, lng = 88.7814,
            radius=6000, color = 'yellow') %>%
  addCircles(lat = 21.5856, lng = 88.2653,
             radius=6000, color = 'black') %>%
  addLegend(title = "Administrative Blocks:", position = "bottomright", colors = c("blue", "black","purple", "yellow","red"), labels = c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5"))
  


#-------------------------------


# rmt purpose plot:
Purpose <-  c("Food/Utility Purchases", "Other", "No Reason", "Medical Expenses","Tuition", "Assets/Durable Purchases")
purpose_count <- c(594, 128, 93, 43, 37, 27)
purpose_dat <- data.frame(Purpose, purpose_count, stringsAsFactors = T)
purpose_values <- c("      594", "      128", "     93", "     43", "      37", "      27")

rmt_purpose_plot <- ggplot(purpose_dat, aes(x = reorder(Purpose, purpose_count), y = purpose_count, fill = Purpose)) + 
  geom_col() +
  labs(x = "", y = "Total") +
  theme_classic() +
  #ggtitle("Purpose for Receiving Remittances")+
  #rotate_x_text(angle = 22, size = rel(0.8))
  coord_flip()+
  geom_text(aes(label = purpose_values), size = 3) + scale_fill_viridis_d()
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
names(avg_rmt) <- c("Date", "Village", "Average Remittances")

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
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) + scale_color_viridis_d()
#--------------------------------------------------------------------
# Expenditure table
expend_table <- expen %>% 
  group_by(date, village) %>% 
  summarize("Average Expenditure" = mean(total_spending, na.rm = T))
expend_table[,3] <- format(round(unlist(expend_table[,3]), digits = 2), nsmall = 2)
names(expend_table) <- c("Date", "Village", "Average Expenditure")
#--------------------------------------------------------------------
# Income plot data:
fin_diary <- livdiv %>% select(village, date, week, name, full_inc) %>% arrange(week, village) %>% group_by(week) 
fin_diary$date <- as_date(fin_diary$date)
avg_tot_inc <- fin_diary %>% group_by(date, village, week) %>% summarize(avg_inc = mean(full_inc, na.rm = TRUE))
ggplot(avg_tot_inc, aes(date, avg_inc, color = village)) + geom_line() + labs(x = "", y = "Income (INR)", title = "Average Weekly Household Income by village", color = "Village") + scale_color_viridis_d()
#--------------------------------------------------------------------
#Income table 
avg_inc_table <- fin_diary %>% group_by(date, village) %>% summarize("Average Income" = mean(full_inc, na.rm = TRUE))
avg_inc_table[,3] <- format(round(unlist(avg_inc_table[,3]), digits = 2), nsmall = 2)
names(avg_inc_table) <- c("Date", "Village", "Average Income")

#Shocks Data ------------------------------------------------------------------- 
## Frequency of each shock (Total baseline)
shocks <- baseline %>% select(village,shk1,shk2,shk3,shk4,shk5,shk6,shk7)
shocks <- data.frame(y=unlist(shocks))
colnames(shocks) <- c('shock_nmb')
shock_labels <- c('None', 'Crop Loss', 'Loss of vegetation', 'Damage(saline water)','Forced to move(Flooding)', 'Loss of agricultural land(river erosion)',
                  'Loss of home(river erosion/cyclone)', 'Loss of livestock', 'Loss of business/shop/etc.', 'Death/health issues', 'Other')

shocks_all <- ggplot(shocks, aes(shock_nmb)) + geom_bar(fill = "dark red") + 
  labs(x = "", y = "Occurances" ,title = "Frequency") + theme(axis.text = element_text(size = 7)) + 
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10),labels = str_wrap(shock_labels, width = 25) ,limits = c(0:10)) + 
  coord_flip()
## Average Shocks by Village
shocks2 <- baseline %>% select(village, shk_count) %>% 
  group_by(village) %>% summarize(avg_count = sum(shk_count, na.rm = TRUE)/n())

shocks_village <- ggplot(shocks2, aes(village, avg_count, fill = village)) + geom_col() + 
  labs(x = "", y = "No. of Shocks" ,title = "Shocks by Village", fill = "Village") + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + scale_fill_viridis_d() + coord_polar()


## Total Shocks by Year
shock_year <- baseline %>% select(village, shk_2009_count, shk_2010_count, shk_2011_count, 
                                  shk_2012_count, shk_2013_count, shk_2014_count, 
                                  shk_2015_count, shk_2016_count, shk_2017_count,shk_2018_count) %>% 
  summarize("2009" = sum(shk_2009_count), "2010" = sum(shk_2010_count), "2011" = sum(shk_2011_count), "2012" = sum(shk_2012_count), 
            "2013" = sum(shk_2013_count), "2014" = sum(shk_2014_count), "2015" = sum(shk_2015_count), "2016" = sum(shk_2016_count),
            "2017" = sum(shk_2017_count), "2018" = sum(shk_2018_count))

shocks_year_long <- gather(shock_year, year, count, "2009":"2018")

shocks_by_year <- ggplot(shocks_year_long, aes(year, count, fill = year)) + geom_col() + 
  labs(x = "", y = "Number of Shocks" ,title = "Total Shocks by Year") + 
  theme(axis.ticks.x=element_blank(), legend.position="none") + scale_fill_viridis_d() 

## Frequency of each shocks in 2009

shocks_2009 <- baseline %>% select(shk_2009_type1, shk_2009_type2, shk_2009_type3, shk_2009_type4, shk_2009_type5, shk_2009_type6)
shock_labels_2009 <- c('None', 'Crop Loss', 'Loss of vegetation', 'Damage caused by saline water encroachment',
                       'Forced to move due to Flooding', 'Loss of agricultural land by river erosion',
                       'Loss of home by river erosion/cyclone', 'Loss of livestock', 'Loss of business/shop/etc. due to rising water levels', 
                       'Unexpected death or health consequence in Household', 'Other')
shocks_2009 <- data.frame(y=unlist(shocks_2009))
colnames(shocks_2009) <- c('shk')

shocks_plot_2009 <-ggplot(shocks_2009, aes(shk)) + geom_bar(fill = "dark red") + 
  labs(x = "", y = "Occurances" ,title = "Shocks for 2009") + theme(axis.text = element_text(size = 7)) + 
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10),labels = str_wrap(shock_labels_2009, width = 25) ,limits = c(0:10)) + 
  coord_flip()
## Type of Cope after 2009 Shock

shocks_cope <- baseline %>% select(village, shk_2009_cope) 

cope_labels <- c("Did not do anything","Unconditional help provided by relatives/friends",
                 "Unconditional help provided by local government", "Changed dietary practices involuntarily", 
                 "Changed cropping practices", "Household member(s) took on more non-farm employment", 
                 "Household member(s) took on more farm wage employment", "Household member(s) migrated",
                 "Relied on savings", "Obtained credit", "Sold durable household assets", "Sold land/building", 
                 "Rented out land/building","Distress sales of animal stock", "Sent children to live elsewhere",
                 "Reduced expenditures on health and education","Do not know/Do not want to answer")

shocks_cope$shk_2009_cope<-replace(shocks_cope$shk_2009_cope, shocks_cope$shk_2009_cope == 997, 16)

cope_2009_plot <- ggplot(shocks_cope, aes(shk_2009_cope, fill = village)) + geom_bar() +
  labs(x = "", y = "" ,title = "Type of Cope after 2009 shocks", fill = "Village") + scale_fill_viridis_d() +
  theme(axis.text = element_text(size = 6)) +
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), labels = str_wrap(cope_labels, width = 30), limits = c(0:20)) + 
  coord_flip() 

## Relocation Status after 2009 Shock

shock_relocation <- baseline %>% select(village, shk_2009_reloc_yn)

relocation_labels <- c("No", "Yes, for under a month", "Yes, for over a month")

shock_relocation_2009_yn <- ggplot(shock_relocation, aes(shk_2009_reloc_yn, fill = village)) + geom_bar() + 
  labs(x = "", y = "No. of Households" ,title = "Relocation Status after Shock", fill = "Village") + 
  scale_x_discrete(breaks = c(0,1,2), labels = str_wrap(relocation_labels, width = 30), limits = c(0:2)) + 
  scale_fill_viridis_d()

## Where they Relocated after 2009 Shock

shock_relocation_where <- baseline %>% select(village, shk_2009_reloc1)

relocation_where_labels <- c("Within same village","Other village in Sundarbans",
                             "Village outside Sundarbans, within West Bengal", "Kolkata", 
                             "Other Urban area outside Sundarbans, within West Bengal","Urban area outside West Bengal")

shock_relocation_2009 <- ggplot(shock_relocation_where, aes(shk_2009_reloc1, fill = village)) + geom_bar() + 
  labs(x = "", y = "No. of Households" ,title = "Relocation Areas", fill = "Village") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6), labels = str_wrap(relocation_where_labels, width = 20), limits = c(1:6)) + 
  scale_fill_viridis_d() + coord_flip() +  theme(axis.text = element_text(size = 8))


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
  #summarise(avg_inc_clothes = mean(aggregated_exp_clothes, na.rm = TRUE), avg_bookstuition = mean(exp_bookstuition, na.rm = TRUE),
  #avg_utility = mean(exp_utility, na.rm = TRUE), avg_toilet = mean(exp_toiletries, na.rm = TRUE),
  #avg_health = mean(exp_health, na.rm = TRUE), avg_homerepairs = mean(exp_homerepairs, na.rm = TRUE), 
  #avg_transport = mean(exp_transport, na.rm = TRUE), avg_livestock = mean(exp_livestock, na.rm = TRUE),
  #avg_aginputs = mean(exp_aginputs, na.rm = TRUE), avg_labor = mean(exp_labor, na.rm = TRUE),
  #avg_nonfoodother = mean(exp_nonfoodother, na.rm = TRUE))
  summarise("Clothes" = mean(aggregated_exp_clothes, na.rm = TRUE), "Books/Tuition" = mean(exp_bookstuition, na.rm = TRUE),
            "Utilities" = mean(exp_utility, na.rm = TRUE), "Toiletries" = mean(exp_toiletries, na.rm = TRUE),
            "Health" = mean(exp_health, na.rm = TRUE), "Home Repairs" = mean(exp_homerepairs, na.rm = TRUE), 
            "Transportation" = mean(exp_transport, na.rm = TRUE), "Livestock" = mean(exp_livestock, na.rm = TRUE),
            "Agriculture" = mean(exp_aginputs, na.rm = TRUE), "Labor" = mean(exp_labor, na.rm = TRUE),
            "Other" = mean(exp_nonfoodother, na.rm = TRUE))
#names(non_food_cs) <- c("village", "week", "Clothes", "Books/Tuition", "Utilities", "Toilitries", "Health", "Home Repairs",
#"Transport", "Livestock", "Agriculure", "Labor", "Other")

# food consupmtion table -----------------
#avg_cs_table <- fin_diary %>% 
#  select(village, date, week, cs_count, cs_total, cs_ricegrains, cs_wheatflour, cs_veg,
#         cs_tubers, cs_fishshrimp, cs_poultry, cs_eggs, cs_pulsespice,
#         cs_redmeat, cs_dairy, cs_packaged, cs_fruit, cs_sinful, cs_other) %>% 
#  group_by(date, village) %>% 
#  summarise( "Average Food Expenditure" = mean(na.omit(cs_total)), "Average Food Items Bought" = mean(na.omit(cs_count)),
#             "Clothes" = mean(aggregated_exp_clothes, na.rm = TRUE), "Books/Tuition" = mean(exp_bookstuition, na.rm = TRUE),
#             "Utilities" = mean(exp_utility, na.rm = TRUE), "Toiletries" = mean(exp_toiletries, na.rm = TRUE),
#             "Health" = mean(exp_health, na.rm = TRUE), "Home Repairs" = mean(exp_homerepairs, na.rm = TRUE), 
#             "Transportation" = mean(exp_transport, na.rm = TRUE), "Livestock" = mean(exp_livestock, na.rm = TRUE),
#             "Agriculture" = mean(exp_aginputs, na.rm = TRUE), "Labor" = mean(exp_labor, na.rm = TRUE),
#             "Other" = mean(exp_nonfoodother, na.rm = TRUE))

#nonfood_table <- non_food_cs
#nonfood_table[,(3:13)] <- format(round(unlist(nonfood_table[,3:13]), digits = 2), nsmall = 2)



filtered_non_food_cs <- reactive({
  non_food_cs %>% 
    filter(village %in% input$village_cs_nonfood)
})

# Events data -------------------------------------
Events <- c("Kharif Crop Harvest", "Rabi Crop Harvest","Honey Harvest", "Fani Cyclone", "Matmo/Bulbul Cyclone", "Vayu Cyclone", "Hikaa Cyclone","Kyaar Cyclone","Maha Cyclone",
            "Republic Day", "Rama Navami", "Eid al-Fitr", "Indian Independence Day", "Dussehra", "Diwali" ,"Christmas")
start_week <- c(2, 0, 19, 22, 48, 30, 43, 47, 48, 10, 20, 28, 38, 46, 49, 5)
end_week <- c(12, 14, 32, 24, 49, 31, 44, 49, 49, 10.2, 20.2, 28.2, 38.2, 46.2, 49.2, 5.2)
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
                                   h1(strong("Assessing Livelihood Diversification in Sundarbans, India using High Frequency Data "),
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
                                          p("The Sundarbans is a cluster of low-lying islands in the Bay of Bengal that spans across India and Bangladesh. The Sundarbans area hosts the largest mangrove forests in the world, supporting an exceptionally rich diversity of flora and endangered fauna such as the Bengal tiger, Estuarine crocodile, Indian python, and Irrawaddy dolphin."),
                                          p("The vast delta is formed by the connection of the Ganga, Brahmaputra, and Meghna rivers. It also has a complex network of tidal waterways, creeks, and mudflats. The area's unique boundaries act as a shelter belt from natural disasters such as cyclones, tidal surges, and seawater seepage. Despite this natural protective system and being a World Heritage Site with conservation requirements, the Sundarbans is considered endangered under the ICUN Red List of Ecosystems due to increasing climate, population and agricultural farming."),
                                          p("The Sundarbans supplies sustainable livelihoods for 4 million people living in small villages near the mangrove forests. Most residents work in various agricultural occupations, including farmers, woodcutters, fishers, and honey gatherers. Farmers, primarily landless laborers, commonly farm a single crop (Aman paddy) in the rainy season and sell food to intermediaries or traders. The woodcutters obtain traditional forest produce like timber, fuelwood, and pulpwood. A large-scale harvest of shrimps, fish, crustaceans, and honey from the forests are also typical. However, with the ongoing climate and population changes, forest conservation efforts have placed a cap on harvesting. For example, in 2022, authorities began issuing three-month honey passes to collect wax from beehives."),
                                          img(src='sunphoto1.png', align = "center", width = "95%")
                                          
                                   ),
                                   column(4,
                                          h2(strong("Project Background")),
                                          
                                          p("The Sundarbans faces an increasing threat to its ecological system due to several manmade and natural causes. First, cyclones, common to this area, are getting more frequent and more serve. From 1961 to 2022, 15 cyclones hit this area, with at least one occurring yearly in the past four years. This has led to the forest incurring severe damages, gradually causing the area to shrink. Second, there is an increase in deforestation due to increasing population and commercial uses. There is also a decrease in aquatic animals due to increased fishing. Additionally, the biological makeup of the forest, such as salinity, soil pH, and reduced freshwater, are being altered due to climate change leading to more fallow land."),
                                          p("Agricultural-dependent families bear the brunt of these increasing threats to the Sundarbans. This is evident by the growing out-migration of the working population to cities and towns as a coping mechanism. Remittance income from this domestic migration has become one of the significant sources of income to protect residents' livelihood."),
                                          img(src='sunphoto.png', align = "center", width = "95%")
                                   ),
                                   
                                   column(4,
                                          h2(strong("Project Goals")),
                                          p("Climate change is a global issue; however, its impact is not felt equally across all regions. Developing countries, especially areas with widespread poverty and poor infrastructure, are more ill-equipped to cope with these environmental threats. The worsening of extreme weather patterns such as high temperatures, droughts, floods, and rising sea levels are especially problematic for countries with large coastal areas and populations that primarily depend on agriculture for their livelihood."),
                                          p("We examine the Sundarbans in West Bengal, India. This region has faced climate changes in recent years and has experienced a number of climate disasters such as flooding and cyclones over the past decade. Residents who primarily engage in small-scale agriculture are forced to diversify their likelihood strategies using out-migration and reduced farming to cope with the increasing environmental changes."),
                                          p("The overall goal of this project is to evaluate livelihood-diversification strategies using weekly financial data for approximately 300 households from 10 representative villages in the region. The team aims to create a public-facing dashboard to describe and visualize households' livelihood diversification strategies, including changes in income, expenditure, and consumption patterns. The insights from this dashboard are important for designing effective and targeted poverty-reducing strategies and aiding those affected by shocks such as natural disasters and climate change."),
                                          img(src='sunphoto2.png', align = "center", width = "95%")
                                   )
                          ),
                          fluidRow(align = "center",
                          p(tags$small(em('Source: Images taken by Sundarbans Field Team'))))
                          
                 ),
                 
                 
                 ## Tab Date Intro--------------------------------------------
                 tabPanel("Data", value = "overview",
                          fluidRow(style = "margin: 6px;", align = "justify",
                                   column(4, 
                                          h2(strong("Data")),
                                          p("We acquire weekly household financial and consumption data from Gupta et al. (2021). Gupta et al. (2021) originally collected household-level data from a representative sample of rural households in the Sundarbans region in West Bengal, India. They collected information from approximately 300 households in 10 villages from November 2018 to October 2019.")
                                   ),
                                   column(4,
                                          h2(strong("Initial/Baseline")),
                                          p("The initial or baseline survey was conducted in November 2018. This data allows the team to visualize and provide insights into the region's demographic and socio-economic characteristics. The baseline survey collected information on household demographics, economic activities, assets and landholding, shock history, migration, and agricultural behaviors.")
                                          
                                          
                                   ),
                                   column(4,
                                          h2(strong("Financial Diaries")),
                                          p("Gupta et al. (2021) use financial diaries to capture high-frequency data on household income, expenditure, and consumption behavior. As such, we have weekly financial and economic activities for approximately 300 households for an entire year (November 2018 to October 2019). "),
                                          p("Household members were trained during the baseline interview to independently record their financial activities in their respective diaries (see image below for an example of a financial diary). Household received two more training sessions in the following two weeks and filled out the first four financial diaries during the training period. Additional support was given to families via phone calls and during the field teams monthly visit to collect completed diaries. These steps were implemented to ensure proper recording of weekly information. These diaries include data on weekly income, remittances, borrowing, lending, expenditure on consumption, and non-consumption items.")
                                          
                                          
                                   ),
                                  
                          ),
                          fluidRow(h4(strong("Example of Financial Diary")),
                            align = "center",
                                   img(src='Picture2.png', width = "50%"),
                          ),
                          fluidRow(align = "center",
                              p(tags$small(em('Source: Gupta et al. (2021)'))))
                          
                          ), 
                 ## Sundarbans Region--------------------------------------------
                 navbarMenu("Sundarbans Region" ,
                            tabPanel("Villages", 
                                     
                                     fluidRow(style = "margin: 2px;",
                                              align = "center",
                                              h1(strong("Representative Villages in the Sundarbans"))
                                                 
                                              ),
                                     
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              column(4, 
                                                     h2(strong("Sundarbans Area")),
                                                     p("This project examines households living in the Sundarbans in West Bengal, India – a coastal delta region in the Bay of Bengal.  Gupta et al. (2021) surveyed households in the North 24 – Parganas and South 24 – Parganas districts. Specifically, ten representative villages were randomly chosen from five administrative blocks in the Sundarbans:"),
                                                     p("    • Beguakhali and Sagar - Block 1 "),
                                                     p("    • Amrabati and Shibpur - Block 2 "),
                                                     p("    • Lakshmi Janardanpur and Purba Dwarokapur - Block 3 "),
                                                     p("• Birajnagar and Bijoynagar - Block 4 "),
                                                     p("• Haridaskati Samsernagar and Pargumti - Block 5 "),
                                                     p("As shown on the map, villages within the same block are close in proximity to each other – at most the distance is 15km between the two villages."),
                                                     
                                                    h4(strong("Weather Related Events")),
                                                     p("The Sundarbans proximity to the Bay of Bengal causes it to be frequented by cyclones. These tropical cyclones usually form in May, October, and November. Although tropical cyclones are common to the area, the frequency and severity have increased in the past few years, with climate change as a contributing factor."),
                                                     p("During the data collection period, November 2018 to October 2019, the Sundarbans area was struck by two significant cyclones (Worlddata.info): "),
                                                     p("• Fani (Category 4): 26 April– 4 May 2019 "),
                                                     p("• Matmo/Bulbul (Category 2): 28 October - 11 November 2019 "),
                                                     p("The Fani Cyclone was an extremely severe tropical storm, reported as the strongest in 2019, and the 10th most severe cyclone in the Indian subcontinent within the last 52 years (Kumar et al. 2020). 
                                                       Its high-speed winds and torrential rain caused extensive flooding, destroying property, assets, agricultural lands, and leading to a significant loss of approximately sixty-four lives across Eastern and Northern Inida.
                                                       The Matmo cyclone formed in the Philippine Sea on October 28th, dissipated as it went West over land (Cambodia), then regained energy and reached peak strength as it went over the Andaman Sea and into the Bengal Bay, making landfall on November 7th, where it was renamed Bulbul (ReliefWeb, 2020).
                                                       This cyclone also caused severe impacts to the property and agricultural lands of the Sundarbans."),
                                                     p("Four cyclones also developed along the Arabian Sea during this period (Worlddata.info):"),
                                                     p("• Vayu (Category 1): 8 - 18 June 2019 "),
                                                     p("• Hikka (Category 1): 20 - 26 September 2019"),
                                                     p("• Kyaar (Category 4): 22 October - 3 November 2019"),
                                                     p("• Maha (Category 3): 28 October - 11 November 2019"),
                                                     p("While the Sundarbans was not reported as a region directly affected by these four cyclones, it is very likely that the they still experienced some of the negative effects of these storms due to their proximity to the Arabian Sea."),
                                              
                                                    h4(strong("Harvest Seasons")),
                                                    p("Agriculture is the backbone of the Sundarbans economy, with mostly small–scale farmers. The sector largely depends on a single crop, the rain-fed paddy Aman. In this region, however, agriculture is very seasonal as it depends on the monsoons:"),
                                                    p("• Kharif Season - This season occurs with the onset of monsoon."),
                                                    p(      "º Preparation and cultivation of Aman paddy usually occurs from June – August."),
                                                    p(      "º Harvesting occurs between December – February."),
                                                    p("• Rabi Season - This is the dry season. While some vegetables are grown during this season, there are not many crops as most of the cultivated areas are fallow."),
                                                    p(      "º Crop Cultivation is between December – February"),
                                                    p(      "º Harvesting of rabi crops happens during summer, March - June"),
                                                    p("Fisheries is the next dominant productive activity. This occurs year-round but majority of fish catch occurs during November to January. Some months (April, May, and June) are closed for fishing. Honey collection on the other hand occurs from April to June."),
                                                    
                                                    h4(strong("Festivals/Holidays")),
                                                    p("Several festivals and holidays that occur during the data collection period are: "),
                                                    p("• Republic Day: Janurary 26th"),
                                                    p("• Rama Navami: April 14th"),
                                                    p("• Eid al-Fitr: June 4-5th"),
                                                    p("• Independence Day: August 15th"),
                                                    p("• Dussehra: October 8th"),
                                                    p("• Diwali: October 27th"),
                                                    p("• Christmas: December 25th"),
                                             ),

                                              column(8, 
                                                     h2(strong("")),
                                                     leafletOutput("map_leaflet", width = "100%", height = 700)
                                              
                                                     
                                                     
                                              )),
                                     fluidRow(align = "center",
                                              p(tags$small(em('Sources: ')))),
                                     fluidRow(align = "center",
                                              p(tags$small(em('Worlddata.info. (n.d.). Most recent cyclones in India. Worlddata.info. Retrieved July 19, 2022, from https://www.worlddata.info/asia/india/cyclones.php.')))),
                                     fluidRow(align = "center",
                                              p(tags$small(em('Kumar, Shubham & Lal, Preet & Kumar, Amit. (2020). Turbulence of tropical cyclone ‘Fani’ in the Bay of Bengal and Indian subcontinent. Natural Hazards.')))),
                                     fluidRow(align = "center",
                                              p(tags$small(em('ReliefWeb. (2020, May 15). Bangladesh: Cyclone Bulbul final report - operation dref N° MDRBD023 - bangladesh. ReliefWeb. Retrieved July 20, 2022, from https://reliefweb.int/report/bangladesh/bangladesh-cyclone-bulbul-final-report-operation-dref-n-mdrbd023 '))))

                                              
                                     ),
                            tabPanel("Timelapse", 
                                     fluidRow(style = "margin: 6px;",
                                              align = "center",
                                              p("", style = "padding-top:10px;"),
                                              column(12, 
                                                     h2(strong("Coastal Degradation Timelapse of Sundarbans Area"), align = "center"),
                                                     h1(""),
                                                     p("The video below shows the coastline of the Sundarbans from 1984 to 2022. This timelapse shows that the coastline has degraded significantly over the years. The circles indicate where this degredation is most evident; some islands have disappeared completely."), 
                                                     p("One such factor of this degradation are the effects caused by climate change; one of these effects being the rising of the sea level, resulting in an increase of runoff and the accelerated erosion of the coast. This coastal erosion reduces the sediment in the area that acts as a natural buffer to flooding (CCSP, 2008), as well as
                                                       increasing the salinity of groundwater, pushing salt water up stream, ultimately causing a decrease in the supply of drinkable water (USGRCP, 2014).
                                                       The thinning of the Sundarbans coast also negatively impacts households’ agricultural yields. Families heavily depend on these yields to support their livelihoods, as it serves as an
                                                       essential source of income and food. Due to this, we've observed fluctuations in income, and frequent occurences of households having to reduce or skip meals."),
                                                     p("The Bay of Bengal and the Arabian Sea have proven to be hotspots for cyclones. As such, these frequent cyclones that occur in the Sundarbans region are a factor that greatly
                                                       contribute to the degredation of its coastline. On average, the Bay of Bengal is hit by seven cyclones per year (Alam, 2003), with the Arabian sea experiencing an average of two (Evan, 2020).
                                                       These cyclones are occuring more often, and their effects are becoming more severe, as the rising sea level increases the base upon which these storm surges are built (NRC, 2010).
                                                       The impacts of these cyclones include flooding, extreme winds, erosion, and further raising the sea level, considerably increasing the potential to damage property and threaten human health and safety."), 
                                                     align = "justify"),
                                                     br(""), tags$video(type = "video/mp4",src = "Sundarbansv3 ‑ Made with FlexClip.mp4", width = "70%", align = "right", controls = "controls", autoplay = T, loop = T)
                                              ),
                                     fluidRow(align = "center",
                                              p(tags$small(em('Sources: ')))),
                                     fluidRow(align = "center",
                                              p(tags$small(em('CCSP (2008). Impacts of Climate Change and Variability on Transportation Systems and Infrastructure: Gulf Coast Study, Phase I. A Report by the U.S. Climate Change Science Program and the Subcommittee on Global Change Research. Savonis, M. J., V.R. Burkett, and J.R. Potter (eds.). Department of Transportation, Washington, DC, USA, 445 pp.')))),
                                     fluidRow(align = "center",
                                              p(tags$small(em('USGCRP (2014). Moser, S. C., M. A. Davidson, P. Kirshen, P. Mulvaney, J. F. Murley, J. E. Neumann, L. Petes, and D. Reed, 2014: Ch. 25: Coastal Zone Development and Ecosystems. Climate Change Impacts in the United States: The Third National Climate As­sessment, J. M. Melillo, Terese (T.C.) Richmond, and G. W. Yohe, Eds., U.S. Global Change Research Program, , 579-618.')))),
                                     fluidRow(align = "center",
                                              p(tags$small(em('NRC (2010). Adapting to the Impacts of Climate Change. National Research Council. The National Academies Press, Washington, DC, USA.')))),
                                     fluidRow(align = "center",
                                              p(tags$small(em('Evan, Amato & Camargo, Suzana. (2011). A Climatology of Arabian Sea Cyclonic Storms. JOURNAL OF CLIMATE.')))),
                                     fluidRow(align = "center",
                                              p(tags$small(em('Alam, M. M., Hossain, M. A., &amp; Shafee, S. (2003). Frequency of bay of bengal cyclonic storms and depressions crossing different Coastal Zones. International Journal of Climatology, 23(9), 1119–1125. https://doi.org/10.1002/joc.927 '))))
                                     )
                                     
                            ),
                            #tabPanel("Gallery",
                             #        fluidRow(style = "margin: 6px;", 
                              #                column(12,
                               #                      h2(strong("Images"))
                                                     
                                #                     
                                 #             ),   
                                  #            mainPanel( 
                                   #             actionButton("previous", "Previous"),
                                    #            actionButton("next", "Next"),
                                     #           imageOutput("image")
                                                
                                      #        ))),
                 #),
                 
                 ## Tab Demographics --------------------------------------------
                 navbarMenu("Demographics" , 
                            tabPanel("Socioeconomic", 
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Socioeconomic Characteristics"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Who lives in the Sundarbans Region?")),
                                                     p("We examine data from the baseline survey collected in November 2018 to understand better the socio-demographic and economic characteristics of the Sundarbans population in West Bengal, India.  "
                                                     ),
                                                     p("Household heads in the Sundarbans area tend to be middle-aged adults, with the mean being around age 49. These individuals also have low levels of education as the average education across villages is approximately five years, comparable to completing elementary school. This low level of education may contribute to the varying poverty level in the region. More than half of families in Haridaskati Samsernagar live with less than ₹240 per week per person (Indian poverty line). However, other villages like Purba Dwarokapur have a lower proportion of households (18%) below the poverty line."
                                                     ),
                                                     p("Most households are headed by married parents. Interestingly, males are more likely to be heads of married households, while females tend to be heads of unmarried households. The number of children is consistent across villages as families living in Sagar, Shibpur, Beguakhali, and Pirba Dwarokapur have about three children per household, slightly higher than average across all villages - i.e., two children per household."
                                                     )
                                                     
                                              ) ,
                                              column(8, 
                                                     h4(strong("Head of Household Demographics")),
                                                     selectInput("agedrop", "Select Characteristic:", width = "100%", choices = c(
                                                       "Age" = "Mean Age for Head of Households",
                                                       "Education" = "Mean Years of Education for Head of Households", 
                                                       "Poverty" = "Households that Live Below Poverty Line (₹240) per week", 
                                                       "Marital Status" = "Household Heads Marital Status",
                                                       "Household Size" = "Household Size by Village", 
                                                       "Children per Household" = "Total Children per Household"
                                                     ),
                                                     
                                                     ),
                                                     fluidRow(align = "center",
                                                              h4(strong(textOutput("result2")))),
                                                     withSpinner(plotlyOutput("ageplot", height = "500px", width = "100%")),
                                                     
                                              ),
                                              # column(12, 
                                              #     h4("References: "), 
                                              #   p(tags$small("[1] Groundwater: Groundwater sustainability. (2021). Retrieved July 27, 2021, from https://www.ngwa.org/what-is-groundwater/groundwater-issues/groundwater-sustainability")) ,
                                              #  p("", style = "padding-top:10px;")) 
                                     )), 
                            tabPanel("Livelihood", 
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Livelihood Behavior"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Occupation")),
                                                     p("While combing through the data our team realized that a household with only one means of support has nothing to fall back on if that livelihood is 
                                                       destroyed by a disaster. Due to the increasing threats in the Sundarbans region, most of the 
                                                       population depends on multiple sources of income. This is a common practice for climate 
                                                       vulnerable regions, to manage and reduce risks."),
                                                     p("We began our evaluation of livelihood behavior by looking at the head of households’ primary 
                                                       occupation, primary occupation duration, and secondary occupation. The most common primary 
                                                       and secondary occupations are Farming followed by Casual labor. These graphs show that there 
                                                       are differences in occupation by village. For example, the village of Amrabati has the highest 
                                                       proportion of workers in jobs related to fishing. The head of households’ occupations are 
                                                       distributed relatively similarly from primary to secondary. However, the total number 
                                                       of household heads in each occupation is lower in secondary occupation. The total number of 
                                                       household heads with a primary occupation is 252 while 149 has a secondary occupation."),
                                                     p("The average job duration by village for head of households is 6 to 8 months. This implies that 
                                                       on average, they work in their primary occupation for ½ to ¾ of the year. This is evidence that 
                                                       the population is relying on different, multiple activities within a year."),
                                                     h4(strong("Agriculture")),
                                                     p("Our data set contained an agricultural crop variable where participants gave a yes/no answer to 
                                                       the question “Did your household cultivate any agriculture crops in the last 12 months?” On 
                                                       average, across all the villages, 63.9% of the households participate in farming. When broken 
                                                       down by village, Amrabati had the lowest percentage of households that had cultivated crops in 
                                                       the given year, at 7%. Haridaskati Samsernagar had the highest percentage of households that had 
                                                       cultivated crops in the given year, at 86%. This was closely followed by Pargumti at 85%. Given 
                                                       that on average, across all the villages, 63.9% of the households participate in farming it is 
                                                       evident that a very shows that a large proportion of our households participated in some sort of 
                                                       crop cultivation."), 
                                                     p("We next evaluated Land Holding and Land Fallow by village. Land holding is the amount of land 
                                                       owned in Kathas. A Katha is unit of area mostly used for land measure in India and Bangladesh. 
                                                       One unit of katha is equivalent to 720 square feet. Pargumti and Bijoynagar have the highest 
                                                       average amount of land owned, with households owning over 60 kathas of land. Amrabati and Sagar 
                                                       have the lowest amount of land owned. Land fallow is agriculture land that needs to be left to 
                                                       rest and regenerate. This land is typically not in good enough condition to sustain crops. The 
                                                       two villages that had the lowest amount of land owned, have no land fallow. This is evidence that 
                                                       these villages that these villages are less involved in agriculture.")
                                                     
                                                     
                                              ) ,
                                              column(8, h4(strong("Head of Household Demographics")),
                                                     selectInput("ocudrop", "Select Characteristic:", width = "100%", choices = c(    
                                                       "Primary Occupation" = "Primary Occupation for Head of Households",
                                                       "Secondary Occupation" ="Secondary Occupation for Head of Households", 
                                                       "Job Duration" = "Average Job Duration for Head of Household",
                                                       "Agricultural Farming" = "Proportion of Households Involved in Agricultural Farming",
                                                       "Land Holding" = "Average Amount of Land Owned by Village",
                                                       "Land Fallow" = "Average Amount of Land Fallowed by Village",
                                                       "Household Assets" = "Proportion of Households Owning Assets"
                                                       
                                                     ),
                                                     ),
                                                     fluidRow(align = "center",
                                                              h4(strong(textOutput("result1")))),
                                                     withSpinner(plotlyOutput("ocuplot", height = "500px")),
                                                     
                                              ),
                                     )),
                            # column(12, 
                            #       h4("References: "), 
                            #       p(tags$small("[1] Groundwater: Groundwater sustainability. (2021). Retrieved July 27, 2021, from https://www.ngwa.org/what-is-groundwater/groundwater-issues/groundwater-sustainability")) ,
                            #      p("", style = "padding-top:10px;")) 
                            
                            
                            tabPanel("Financial", 
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Financial Practices"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Description")),
                                                     p("We examine data from the baseline survey to establish a foundational understanding of the financial behavior of the Sundarbans population. The financial diaries provide an extensive amount of information on the fiscal practices of our representative population. The baseline information provides us with a strong starting point in our economic analysis of the data. "),
                                                     p("On average, the monthly salaries for the households range from 2500 INR to 4600. On average, Amrabati reported the highest salary and Sagar has the lowest on average."),
                                                     
                                                     p("In our sample population, a large proportion of the household incomes come from remittances. Remittances are any income a household receives from someone working away from the household. In most villages, higher remittances correlate with higher income. While our analysis showed that remittances are a common source of income, only 12% of households own a business, making this an uncommon source of income throughout the villages. Not many households are able to save- given that such a high proportion of the population live below the Indian poverty line of less than ₹240 per week per person.")
                                                     
                                              ) ,
                                              column(8, h4(strong("Head of Household Demographics")),
                                                     selectInput("findrop", "Select Characteristic:", width = "100%", choices = c( 
                                                       
                                                       "Household Business" = "Number of Households that Own a Business",
                                                       "Salary" = "Average Monthly Salary per Household by Village",
                                                       "Migrant Workers" = "Percentage of Migrant Workers per Household",
                                                       "Income/Remmitances" = "Income vs Remmitances (October 2018 - November 2019)",
                                                       "Savings" = "Number of Times Households Saved Money in Year Prior to Baseline Survey (October 2018 - November 2019)"
                                                     )),
                                                     fluidRow(align = "center",
                                                              h4(strong(textOutput("result")))),
                                                     
                                                     withSpinner(plotlyOutput("finplot", height = "500px")),
                                                     
                                              ),
                                              # column(12, 
                                              #   h4("References: "), 
                                              #  p(tags$small("[1] Groundwater: Groundwater sustainability. (2021). Retrieved July 27, 2021, from https://www.ngwa.org/what-is-groundwater/groundwater-issues/groundwater-sustainability")) ,
                                              #  p("", style = "padding-top:10px;")) 
                                              
                                     )),
                            
                 ), 
                 
                 
                 
                 
                 # FD data tab-----------------------------------------------------------
                 
                 navbarMenu("High Frequency Data" ,
                            tabPanel("Expenditure",
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Expenditure"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Total Spending")),
                                                     p("We present average weekly expenditure from Nov 2018 - Oct 2019 to examine the spending behaviors of households in the region. This will provide information on the changing nature of spending in the Sundarbans region due to events such as festivals and holidays, 
                                                       harvest seasons, and weather-related shocks."),

                                                     p("Expenditure is defined as spending on consumption (e.g., food) and non-consumption (e.g., rent) items.
                                                     The average weekly expenditure over the data period was 1982.77 rupees, with a median of 1832.1 rupees. 
                                                       It appears that the largest expenses occured during harvest seasons, partculary in villages with high amounts of land holding
                                                       and proportions of agricultrue farming, such asBeguakhali and Shibpur. We also observed increases in expenditure near when cyclones hit, when households could be buying ssupplies
                                                       to prepare for the storms."),

                                                     br("")
                                                     
                                              )),
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         #tags$h2("Select/Deselect all"),
                                         pickerInput("village_exp", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         pickerInput("event_choose_exp", "Select Event:", choices = events_vector, selected = "Kharif Crop Harvest", 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         
                                         
                                       ),
                                       
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Average Weekly Expenditure",plotOutput("exp")),
                                           tabPanel("Table", DT::DTOutput("exp_table"))
                                         )
                                       ),
                                       
                                     ),
                                     
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h4(strong(""), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Food Consumption")),
                                                     p("We present the average weekly expenditure on food consumption items from November 2018 - October 2019. Consumption expenditure includes purchases by 
                                                     households on goods and services, excluding housing. By visualizing consumption expenditures over time, we can gain information about household spending behavior, 
                                                     identifying changes in spending, as well as which consumption items are bought most frequently. Within the data period, the Sundrabans region spent
                                                       an average of 766.13 Rupees per week on consumption items; they also had bought an average of seven food items per week."),
                                                     p("First, we provide time series of average expenditure on all food, and quantity of food items, then staple food items, meats, and other consumable items."),
                                                     p("• Staple Items - Rice/Grains, Flour, Vegetables, Fruits, Tubers, Beans and Spices"),
                                                     p("• Meats - Red Meat, Fish, and Poultry"),
                                                     p("• Other - Eggs, Dairy, Packaged Foods, Tea, and Sinful Items"),
                                                     p("We identified that most of food consumption is being used for staple food items, followed by meats. We observed
                                                       a siginficant spike in 'Other' items, in Shibpur, in late April due to a large increase in expenditure on sinful items (tea,
                                                       cigarettes, betel leaves, bidil, etc.). These items are often deemed to be harmful to society, but provide certain satisfaction to
                                                       consumers. Therefore, this increase in consumption could suggest a communal need to cope from the Fani cyclone tat happened at this time."),
                                                     br("")
                                                     
                                              )),
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         pickerInput("village_cs", "Select Village:", choices = village_vector,
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         pickerInput("event_choose_cs", "Select Event:", choices = events_vector, selected = "Kharif Crop Harvest", 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         
                                       ),
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Average Food Consumption",plotOutput("cs_exp")),
                                           tabPanel("Staple Items", plotOutput("cs_staple")),
                                           tabPanel("Meats", plotOutput("cs_meats")),
                                           tabPanel("Other", plotOutput("cs_other")),
                                           tabPanel("No. of Food Items", plotOutput("cs_item"))
                                           #tabPanel("Table", DT::DTOutput("cs_table"))
                                         )
                                       ),
                                       
                                       
                                     ),
                                     
                                     # Sidebar with a select input for village
                                     #sidebarLayout(
                                     #sidebarPanel(
                                     #pickerInput("village_cs_food", "Select Village:", choices = village_vector, 
                                     #selected = village_vector,
                                     #multiple = T, options = list(`actions-box` = T)),
                                     #varSelectInput("food_group", "Select Consumption Group:", avg_cs_food[,-(1:2)]),
                                     #pickerInput("event_choose_cs_food", "Select Event:", choices = events_vector, selected = "Kharif Crop Harvest", 
                                     #multiple = T, options = list(`actions-box` = T)),
                                     
                                     #),
                                     # Show a plot of the generated plot
                                     #mainPanel(
                                     #tabsetPanel(
                                     #tabPanel("Plot", plotOutput("food_plot"))                                         
                                     #)
                                     #),
                                     
                                     
                                     #),
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h4(strong(""), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Non-Food Consumption")),
                                                     p("Furthermore, we examined consumption expenditure on non-food items, inluding: clothes, 
                                                       books and tuition, utilities, toiletries, health, home repairs, transportation, livestock,
                                                       agriculture, labor, and other non-food items."), 
                                                     p("Expenditures on health, home repairs, and books/tuition
                                                       made up the largest but least frequent expenses, while utilities, toilitries, and transportation made up the most frequent
                                                       purchases. Considering farmers make up the largest proportion of occupation in the Sundarbans, it is predictable to also see 
                                                       frequent consumption expenditures on agriculture, livestock, and labor. The average weekly expenditure
                                                       on non-food items was 882.22 rupees. We observed increases in expenditure
                                                       on non-food items during harvest seasons. The largest expenditure on non-food items occured
                                                       near the Bulbul, matmo, and Hikaa cyclone, as well as near Diwali and Dusshera"),
                                                     br("")
                                                     
                                              )),
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         pickerInput("village_cs_nonfood", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         varSelectInput("nonfood_group", "Select Consumption Group:", non_food_cs[,-(1:2)]),
                                         pickerInput("event_choose_cs_nonfood", "Select Event:", choices = events_vector, selected = "Kharif Crop Harvest", 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         
                                       ),
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Average Non-Food Consumption", plotOutput("nonfood_plot")),
                                           #tabPanel("Table", DT::DTOutput("nonfood_table"))
                                         )
                                       ),
                                       
                                       
                                     ),
                                     
                                     
                                     
                            ), 
                            
                            tabPanel("Income",
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Income"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Overview")),
                                                     p("We also report average weekly household income for households across villages over 52 weeks. 
                                                       There is a significant increase in households’ income across most villages in late March. This increase coincides 
                                                       with the largest harvest for farmers in the region. We will investigate the different variations (spikes and dips) 
                                                       to determine the correlation between environmental shocks or unexpected household incidents."),
                                                     p("Income: Over the twelve month period that the data was collected in, our team was able to track 
                                                       weekly household income and we were able to visualize it by breaking the income by each village.  
                                                       On average, the weekly income per household is 1395.61 INR and the median is 1341.82. Throughout 
                                                       the year there are many spikes which can be caused by different harvest seasons, influx in remittance, 
                                                       or other external factors. In early April we can see a bigger spike as this time period marks one of 
                                                       the biggest harvest seasons seen by the local people. "),
                                                     p("Male and Female Income: Although we know that males in the region attain more income than females, 
                                                       we wanted to see if there are certain households in any villages where the females in the household 
                                                       made more than the men."),
                                                     p("The importance of remittance income can be seen in this graph as all of the villages have similar 
                                                       weekly income before adding remittance. The village of Sagar has a weekly average income around 1954 
                                                       INR which is one of the higher weekly incomes in the region. Since many of these households work for 
                                                       wage either as an agriculture worker or casual laborer, the per week income is relatively same throughout 
                                                       the region. This can indicate why the weekly income is ranging consistently in-between 1000 INR and 2000 INR.")
                                                     )),
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         #tags$h2("Select/Deselect all"),
                                         pickerInput("village_inc", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         varSelectInput("Gender", "Select Gender:", malefemale_inc[,-(1:2)]),
                                         pickerInput("event_choose_inc", "Select Event:", choices = events_vector, selected = "Kharif Crop Harvest", 
                                                     multiple = T, options = list(`actions-box` = T)),
                                       ),
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Average Weekly Income",plotOutput("inc")),
                                           tabPanel("Male/Female Income", plotOutput("malefemaleinc")),
                                           #tabPanel("Full Income", plotOutput("fullinc")),
                                           tabPanel("Table", DT::DTOutput("inc_table"))
                                         )
                                       ),
                                     ),
                            ),
                            
                            
                            tabPanel("Borrowing",
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Borrowing"), align = "justify"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Overview")),
                                                     p("The first tab depicts amount borrowed by village throughout the year. There is a spike of amount borrowed between April and July. This is during the dry season, when the honey harvest occurs. This spike also happens at roughly the same time the Fani Cyclone hit this region.  Purba Dwarokapur, Shibpur and Sagar show major spikes in the amount borrowed during this time. With the amount reaching about 40000 INR."),
                                                     p("The “Count” tab depicts the number of Households borrowing. This data is relatively consistent throughout the year other than an early spike in Bijoynagar, with over 30 households borrowing before January of 2019. This spike occurs at the same time as the Kharif and Rabi crop harvests. If you deselect Bijoynagar, all other villages range from 17 to 0 households borrowing each week. The lowest number of households borrowing is between January and July, with no more than 10 households borrowing each week during these months.  A couple villages increase to above 10 households borrowing after July."),
                                                     p("The main purpose for borrowing is consumption, with over 2000 total occasions of borrowing. The next most common purposes are other expenses and payback of other loans. Borrowing is done both in cash and in kind. 54% are done in kind and 46% are in cash.")
                                                     
                                                     
                                                     
                                              )),
                                     
                                     # Show a plot of the generated plot
                                     mainPanel(
                                       tabsetPanel(
                                         tabPanel("Amount",plotOutput("bor"),
                                                  sidebarPanel(
                                                    pickerInput("village_bramt", "Select Village:", choices = village_vector, 
                                                                selected = village_vector, 
                                                                multiple = T, options = list(`actions-box` = T)),
                                                    pickerInput("event_choose_borr", "Select Event:", choices = events_vector, selected = "Kharif Crop Harvest", 
                                                                multiple = T, options = list(`actions-box` = T)),), 
                                         ),
                                         tabPanel("Count",plotOutput("borr"),
                                                  sidebarPanel(
                                                    pickerInput("village_borr", "Select Village:", choices = village_vector, 
                                                                selected = village_vector,
                                                                multiple = T, options = list(`actions-box` = T)),
                                                    pickerInput("event_choose_borr_count", "Select Event:", choices = events_vector, selected = "Kharif Crop Harvest", 
                                                                multiple = T, options = list(`actions-box` = T))),
                                         ),
                                         tabPanel("Purpose", 
                                                  plotOutput("purpplot", height = "500px")
                                         ),
                                         
                                         
                                         
                                         
                                       ))),
                            
                            
                            tabPanel("Remittances", value = "",
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h1(strong("Remittances"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Overview")),
                                                     p("In recent years, households have become more reliant on remittances as a significant source of income. 
                                                       As such, we examine temporal changes in remittances between October 2018 and November 2019. The villages in the Sundarbans
                                                       recieve consistent inputs of remittances throughout the data period. Notably, the Sundarbans region was affected by three severe cyclones during this period: 
                                                       Fani, Category 4 (April – May 2019), and Category 1, Bulbul and Matmo (October – November 2019). The Sundarbans also could have been negatively impacted by four 
                                                       cyclones that hit the Arabian Sea during this period: Vayu (Category 1, June 8-18), Hikaa (Category 1, September 20-26), Kyaar (Category 3, October 22 - November 3), and Maha (Category 4, October 28 - November 11). 
                                                       It is possible households are using remittances to cope with these cyclones and weather-related shocks."),
                                                     p("With climate change impacting coastal areas disproportionately	 compared to other environments, the Sundarban region is seeing the effects of this in one 
                                                       way through employment opportunities. Since farming and fishing are one of the biggest employment opportunities in the region, the effects of climate change 
                                                       on the population of fish or the amount of arable farming land has put a strain on the working population in the region. Due to this reason, many of the younger 
                                                       population (18-30) are seeking work in cities where the wage is higher and employment is easier to find.  Since this impacts the households in the Sundarbans greatly, 
                                                       the migrant workers send money back(Remittance Income) to their families.  As threats to climate continue and are only going to get worse, the Sundarban region is going 
                                                       to see an increase in lack of employment	opportunities which impact the demographics of the region since the younger population is moving away."),
                                                     p("Remittance impact on the livelihood of the Sundarban population can be seen as the data collected shows that the median 
                                                       weekly remittance income is 205.61 INR which is on average almost 800 INR. This significant portion of a households monthly 
                                                       income show that importance this income has on the families ability to function. The graph also does a good job at showing 
                                                       spikes in remittance income which can be either because of festivals, other celebrations, money sent because of health 
                                                       concerns, or other shocks."),
                                                     
                                                     
                                              ) ),
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              p("", style = "padding-top:10px;"),
                                              column(12, h4(strong("Remittances Sources and Usage")),
                                                     p("With migrant workers coming from different parts of West Bengal like Kolkata which is one the biggest cities in India, or from 
                                                       overseas in the Middle East or Southeast Asia, they use different methods to send money back home to the Sundarbans. This region 
                                                       also has limited access to internet services as well as cellular data making wire transfers, and other electronic banking unfeasible 
                                                       and also expensive due to high transfer rates. Due to this reason, the most common way money is sent back is in person when migrant 
                                                       workers come back home. The second most common method used to send money back is bank transfers. Within India, money can be 
                                                       transferred at the same banks in different locations which is often more convenient. Over the one year of weekly financial data, 
                                                       remittance is sent as a one time “lump sum” for expenses like tuition fees or needed capital for different shocks or unlikely 
                                                       circumstances. Remittance is also sent on monthly or bi-monthly instances to help with the consistent expenses. Most frequently 
                                                       the money sent from migrant workers are used to take care of consumption expenses like food or utility purchases. Least frequently 
                                                       this money is used to medical expenses, tuition, or big durable purchases. "),
                                                     br(""), #plotOutput("rmt_method", width = "70%")
                                                     
                                                     
                                              )),
                                     
                                     
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         #tags$h2("Select/Deselect all"),
                                         pickerInput("village_rmt", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         pickerInput("event_choose_rmt", "Select Event:", choices = events_vector, selected = "Kharif Crop Harvest", 
                                                     multiple = T, options = list(`actions-box` = T)),
                                         
                                       ),
                                       
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Average Weekly Remittances",plotOutput("rmt")),
                                           tabPanel("Table",DT:: DTOutput("rmt_table")),
                                           tabPanel("Method", plotOutput("rmt_method")),
                                           tabPanel("Purpose", plotOutput("rmt_purpose"))
                                         )
                                       ), 
                                       
                                       
                                     ),
                                     #fluidRow(style = "margin: 6px;", align = "justify",
                                      #        p("", style = "padding-top:10px;"),
                                       #       column(12, align = "center", h4(strong("Remittances Sources")),
                                        #             p("We also examine how households received remittances. We find that households primarily collected remittances 
                                         #              in person or through a bank suggesting these methods to be the most convenient. Although a money order is a 
                                          #             secure method of sending/receiving money, it requires additional fees, which may make it more expensive for 
                                           #            this poverty-stricken area. Moreover, households may be more concerned about receiving the remittance quickly 
                                            #           rather than safely. Also, using mobile apps can be difficult in regions where data usage is limited."),
                                             #        br(""), #plotOutput("rmt_method", width = "70%")
                                                     
                                                     
                                              #)),
                                     #plotOutput("rmt_method", width = "65%"),
                                     
                                     #fluidRow(style = "margin: 6px;", align = "justify",
                                      #        p("", style = "padding-top:10px;"),
                                       #       column(12, align = "center", h4(strong("Usage of Remmittances")),
                                        #             p("Remittances is primarily being used for food and utility purchases, which are 
                                         #              often the most essential items for households in underdeveloped regions."),
                                          #           br(""), #plotOutput("rmt_purpose", width = "70%")
                                                     
                                                     
                                           #   )),
                                     
                                     
                                     
                            ),
                            
                            
                            
                 ),
                 ## Shocks Tab --------------------------------------------
                 
                 navbarMenu("Shocks" , 
                            tabPanel("Shocks in the Sundarbans", value = "", align = "center",
                                     
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("The Sundarbans region is highly susceptible to climate change and extreme weather events, 
                                                                  especially in the last decade. These weather changes negatively impact the Sundarbans population 
                                                                  in terms of their economic activities and livelihoods. ")),
                                                     br("We present the frequency of different shocks households experience from 2009 to 2018.
                                                        Most households indicate experiencing home loss due to river erosion and cyclone. The 
                                                        loss of livestock is also a significant negative shock impacting families in this region. 
                                                        The least common occurring shock is the loss of business/shop. ")
                                              )
                                     ),
                                     
                                     # Show a plot of the generated plot
                                     tabPanel("All The Shocks", plotOutput("shocks_all", width = "65%"),
                                     ),
                                     
                                     fluidRow(style = "margin: 6px", align = "justify",
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("")),
                                                     br("Given the significance of the different shocks over the nine-year period, we 
                                                        examine whether there was a disproportionate impact of shocks by villages. On average, households 
                                                        in each village tend to experience 2 to 3 different shocks each year from 2009 to 2018. We 
                                                        suspect that this even distribution across villages is due to villages being relatively close. 
                                                        Thus, many of the natural shocks will impact all households in each village. ")
                                              )
                                     ),
                                     
                                     tabPanel("Shocks by Village", plotOutput("shocks_village", width = "65%")
                                     ),
                                     fluidRow(style = "margin: 6px", align = "justify",
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("")),
                                                     br("The Sundarbans area typically face tropical events such as cyclones. However, the frequency 
                                                     and intensity of cyclones have increased in the past decade. Specially, the most devasting cyclones 
                                                     in the region occurred in 2007, 2009, 2019, 2020, and 2021.  
                                                  The impact of the cyclone in 2009 (Alia) is still evident as the majority of households reported a shock 
                                                        in 2009, even though this interview was done in 2018. Moreover, many families reported experiencing 2 
                                                        to 3 shocks, with some reporting a high of 4 shocks in 2009. This graph highlights that 2009 was a devasting 
                                                        year for most households. ")
                                              )
                                     ),
                                     
                                     
                                     tabPanel("Shocks by the Year", plotOutput("shocks_by_year", width = "65%"),
                                              fluidRow(style = "margin: 6px;", align = "justify",
                                                       p("", style = "padding-top:10px;"),
                                                       column(12,h4(strong("")),
                                                              br("We further investigate the impact of the 2009 shock on household livelihood. 
                                                                 During this year, many families lost their homes due to the cyclone. Families also lost livestock, 
                                                                 vegetation, and crops. They were also forced to move due to flooding, which may be related to the 
                                                                 aftermath of the cyclone.")
                                                       )
                                              )
                                     ),
                                     
                                     tabPanel("Shocks in 2009", plotOutput("shocks_plot_2009", width = "65%"),
                                              fluidRow(style = "margin: 6px;", align = "justify",
                                                       p("", style = "padding-top:10px;"),
                                                       column(12,h4(strong("")),
                                                              br("After the many shocks in 2009, families in the Sundarbans region coped by taking steps such as 
                                                                 obtaining credit or pursuing other jobs. Notably, the most common coping method was unconditional 
                                                                 help from the government, followed by receiving support from friends or relatives. Often, families 
                                                                 did nothing and tried to “weather the storm” until better times. ")
                                                       )
                                              )
                                     ),
                                     
                                     tabPanel("Copes in 2009", plotOutput("cope_2009_plot", width = "65%"),
                                              fluidRow(style = "margin: 6px;", align = "justify",
                                                       p("", style = "padding-top:10px;"),
                                                       column(12,h4(strong("")),
                                                              br("Relocation is common after shocks occur in the region and often times households are relocated 
                                                                 for less than a month. Around 80 households don’t relocate after a shock and 75 households relocate 
                                                                 for more than a month as well. ")
                                                       )
                                              )
                                              
                                     ),
                                     
                                     tabPanel("Relocations after Shock in 2009", plotOutput("shock_relocation_2009_yn", width = "65%"),
                                              fluidRow(style = "margin: 6px;", align = "justify",
                                                       p("", style = "padding-top:10px;"),
                                                       column(12,h4(strong("")),
                                                              br("With a vast majority of households saying that they relocate for either less or more than a month, 
                                                                 many of these households relocate to a safer place in the same village. Less frequently do the households 
                                                                 relocate to Kolkata (the biggest city nearby) or other villages around the Sundarbans. ")
                                                       )
                                              )
                                     ),
                                     
                                     tabPanel("Where Relocation took place", plotOutput("shock_relocation_2009", width = "65%")
                                     ),
                                     
                            ),  
                            #the comma above separates the two sub-tabs in Shocks            
                            
                            
                            tabPanel("Yearly Shocks",
                                     fluidRow(style = "margin: 6px;", 
                                              h1(strong("2009 Shocks"), align = "center"),
                                              p("", style = "padding-top:10px;")
                                              
                                     ),
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         #tags$h2("Select/Deselect all"),
                                         pickerInput("village_selecter", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         
                                       ),
                                       
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Coping Strategies",plotOutput("cope_2009")),
                                           tabPanel("Relocation",plotOutput("relocation_2009_yn")),
                                           tabPanel("Relocation Area",plotOutput("relocation_2009")),
                                         )
                                       ),
                                       
                                     ),
                            ),
                 ),
                 
                 ## FGD tab --------------------------------------------
                 tabPanel("Focus Group Discussions",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong(""), align = "center"),
                                   p("", style = "padding-top:10px;")),
                          fluidRow(style = "margin: 6px;",
                                   p("", style = "padding-top:10px;"),
                                   column(12, align = "center",h4(strong("FGD")),
                                          p(""),
                                          br("")
                                          
                                          
                                   )),
                          
                 ),             
                 ## Tab Team --------------------------------------------
                 tabPanel("Team", 
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   align = "center",
                                   br(""),
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
                                          p(a(href = 'https://www.linkedin.com/in/samantha-rippley-58846119b/', 'Samantha Rippley', target = '_blank'), "(Virginia Tech, M.S in Agriculture and Applied Economics);"),
                                          br(), 
                                          img(src = "das.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/nandini-das-390577104/', 'Nandini Das', target = '_blank'), "(Virgina Tech, PHD in Economics)"),
                                          br(), 
                                          img(src = "Taj.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/taj-cole-83a738221', 'Taj Cole', target = '_blank'), "(Virginia Tech, Undergraduate in Environmental Economics, Management, and Policy, and Minoring in Data and Decisions)."),
                                          br(), 
                                          img(src = "Sid.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/siddarth-ravikanti-63374b207/', 'Siddarth Ravikanti', target = '_blank'), "(Virginia Tech, Undergraduate in Statistical and Data Science)"),
                                         p("", style = "padding-top:10px;") 
                                   ),
                                   column(6, align = "center",
                                          h4(strong("VT Faculty Team Members")),
                                          # img(src = "team-posadas.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "holmes.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://www.linkedin.com/in/chanita-holmes-385577234/", 'Dr. Chanita Holmes', target = '_blank'), "(Research Assistant Professor Department of Agriculture and Applied Economics Virginia Tech)"),
                                          
                                   h4(strong("Project Stakeholders")), 
                                   img(src = "anubhab.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                   p(a(href = 'https://www.linkedin.com/in/samantha-rippley-58846119b/', 'Dr. Anubhab Gupta', target = '_blank'), "(Assistant Professor Department of Agriculture and Applied Economics Virginia Tech);"),
                                   br(), 
                                   img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                   p(a(href = 'https://www.linkedin.com/in/nandini-das-390577104/', 'Dr. Aleksandr Michuda', target = '_blank'), "(UC Davis)"),
                                   br(), 
                                   img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                   p(a(href = 'https://www.linkedin.com/in/taj-cole-83a738221', 'Dr.Miki Doan', target = '_blank'), "(Cornell)."),
                                   br(), 
                                   img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                   p(a(href = 'https://www.linkedin.com/in/siddarth-ravikanti-63374b207/', 'Binoy Majumder', target = '_blank'), "(Sundarbans Field Team Lead)"),
                                   br(), 
                                   img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                   p(a(href = 'https://www.linkedin.com/in/siddarth-ravikanti-63374b207/', 'Heng Zhu', target = '_blank'), "(World Food Program)"),
                                    p("", style = "padding-top:10px;") 
                 ))),
                          
                 
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
    ggplot(filtered_bramt(), aes(x=week_num, y=br_amt, color = village, na.rm=TRUE)) +
      geom_line() +
      #labs(title ="Amount Borrowed by Village") + 
      xlab("Date") +
      ylab("Amount Borrowed (INR)")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) +
      scale_color_viridis_d() +
      theme(legend.position = "none")+
      geom_rect(data = filtered_event_borr(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
  })  
  
  # borrowing count 
  filtered_dbr <- reactive({
    dbr %>%
      filter(village %in% input$village_borr)
  })
  # Plot
  output$borr <- renderPlot({
    ggplot(filtered_dbr(), aes(x=week_num, y=d_br, color = village, na.rm=TRUE)) +
      geom_line() +
      #labs(title = "Number of Households Borrowing (Cash or in Kind)") + 
      xlab("Date") +
      ylab("Number of HH")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) +
      scale_color_viridis_d() +
      theme(legend.position = "none")+
      geom_rect(data = filtered_event_borr_count(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
  })  
  
  #borrowing purpose ----------------------
  
  output$purpplot <- renderPlot({
    ggplot(dfpurp, aes(x= A, y = B, fill = A)) + geom_col() + 
      coord_flip()+
      #labs(title = "Purpose of Borrowing") + 
      xlab("") +
      ylab("")+
      theme(legend.position = "none") 
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
  
  # Consumption by food group plots
  
  #output$food_plot <- renderPlot({
  #ggplot(filtered_cs_food(), aes(x = week, y = !!input$food_group, color = village))+
  #geom_line()
  #})
  
  output$cs_staple <- renderPlot({
    ggplot(filtered_cs_food_staple(), aes(x = week, y = `Staple Items`, color = village)) +
      geom_line()+
      theme_classic()+
      #ggtitle("Average Weekly Expenditure on Staple Items ")+
      labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 463.87  Median: 431.20")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_viridis_d()
    
  })
  
  output$cs_meats <- renderPlot({
    ggplot(filtered_cs_meats(), aes(x = week, y = `Meats`, color = village))+
      geom_line()+
      theme_classic()+
      #ggtitle("Average Weekly Expenditure on Meat")+
      labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 158.97  Median: 431.20")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_viridis_d()
  })
  
  output$cs_other <- renderPlot({
    ggplot(filtered_cs_other(), aes(x = week, y = `Other`, color = village))+
      geom_line() +
      theme_classic()+
      #ggtitle("Average Weekly Expenditure on 'Other' Items")+
      labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 113.75  Median: 111.94")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_viridis_d()
    
  })
  
  
  
  
  
  #sociodemo tabset -----------------------------------------------------
  ageVar <- reactive({
    input$agedrop
  })
  
  output$ageplot <- renderPlotly({
    if (ageVar() == "Mean Age for Head of Households") {
      
      fplot <-  ggplot(by_villagemore, aes(x = village, y = head_age, fill = village, width=0.5, srt = 45)) +
        geom_col(hoverinfo = "text", aes(text = paste("Age: ", round(head_age,2))), width = "5") +
        ylab("Age") + 
        xlab("")+
        theme(legend.position = "none") +
        rotate_x_text(angle = 33, size = rel(1)) 
      + scale_fill_viridis_d()
      ggplotly(fplot, tooltip = c("text"))
    }
    else if (ageVar() == "Mean Years of Education for Head of Households") {
      splot <- ggplot(by_villagemore, aes(x = "", y= head_edu, fill = village)) +
        geom_bar(width = 1, stat = "identity", hoverinfo = "text", aes(text = paste("Education: ", round(head_edu, 2), "<br>Village: ", village))) +
        facet_wrap(~village, ncol = 5) +
        labs(x = NULL, y = "Years of Education") +
        theme(legend.position="none", strip.text.x = element_text(size = 9)) + scale_fill_viridis_d()
      ggplotly(splot, tooltip = c("text"))
    }
    else if (ageVar() == "Households that Live Below Poverty Line (₹240) per week") {
      village_pl_count_plot <- ggplot(dat_pl, aes(x= Village, y = Households, fill = Key)) + 
        geom_col(position = 'stack', hoverinfo = "text", aes(text = paste("Village:", Village,"<br>Key: ", Key, "<br>Percentage:", percentage,"%" , "<br>Total Households: ", Households ))) + 
        labs(x= "", y = "Total Households", fill = "") + 
        theme_classic() + 
        coord_flip()
      ggplotly(village_pl_count_plot, tooltip = c("text"))
    }
    else if (ageVar() == "Household Heads Marital Status") {
      marplot <- ggplot(countmar, aes(x = head_married, y = n, fill = Gender)) +
        geom_col(hoverinfo = "text", aes(text = paste("Total:", n,"<br>Gender: ", Gender))) +
        labs(x = "Not Married                                         Married", y = "Total Household Head", fill = "") +
        scale_x_discrete() + theme(legend.title=element_blank())
      ggplotly(marplot, tooltip = c("text"))
    }
    else if (ageVar() == "Household Size by Village") {
      hh_size_plot <- ggplot(median_hhsize, aes(x = forcats::fct_rev(village), y = median, fill = village)) +
        geom_col( hoverinfo = "text", aes(text = paste("Village:", village,"<br>Median: ", median))) +
        labs( x = "", y = "Median Household Size")+
        coord_flip()+
        theme(legend.position="none") + scale_fill_viridis_d()
      ggplotly(hh_size_plot, tooltip = c("text"))
    }
    else if (ageVar() == "Total Children per Household") {
      chhoplot <- ggplot(avg_children, aes(x = village, y = avg_children, fill = village)) + 
        geom_col(hoverinfo = "text", aes(text = paste("Village:", village,"<br>Average Children: ", round(avg_children, digit = 2)))) + labs(x = "", y = "Average number of children" ,title = "", fill = "Village") + 
        theme(legend.position = "none") +
        rotate_x_text(angle = 33, size = rel(1)) + scale_fill_viridis_d()
      ggplotly(chhoplot, tooltip = c("text"))
    }
  })
  
  
  
  #livelihood tabset -----------------------------------------------------
  ocuVar <- reactive({
    input$ocudrop
  })
  
  output$ocuplot <- renderPlotly({
    if (ocuVar() == "Primary Occupation for Head of Households") {
      pocuplot <- ggplot(countv, aes(x = job, y = n, fill = village)) +
        geom_col(hoverinfo = "text", aes(text = paste("Village:", village, "<br>Total: ", n))) +
        scale_x_discrete(limits = factor(1:16), labels = c("1" = "Agricultural wage worker","2" =  "Livestock worker", "3" = "Farmer", "4" = "Casual labor","5" =  "Construction/brick labor","6" =  "Gleaning/foraging","7" =  "Fisherman","8" =  "Fishery worker", "9" = "Factory worker" , "10" = "Household help" ,"11" =  "Transport related work","12" =  "Own business", "13" = "Service Work (NGO, gov,etc.)", "14" = "NREGA","15" =  "Housewife","16" =  "Other")) +
        coord_flip() +
        theme_minimal () +
        labs(x = "", y = "Total Households", fill = "Select Village:") + scale_fill_viridis_d()
      ggplotly(pocuplot, tooltip = c("text"))
    } 
    else if (ocuVar() == "Secondary Occupation for Head of Households") {
      socplot <- ggplot(scountv, aes(x = job, y = n, fill = village)) +
        geom_col(hoverinfo = "text", aes(text = paste("Village:", village, "<br>Total: ", n))) +
        scale_x_discrete(limits = factor(1:16), labels = c("1" = "Agricultural wage worker","2" =  "Livestock worker", "3" = "Farmer", "4" = "Casual labor","5" =  "Construction/brick labor","6" =  "Gleaning/foraging","7" =  "Fisherman","8" =  "Fishery worker", "9" = "Factory worker" , "10" = "Household help" ,"11" =  "Transport related work","12" =  "Own business", "13" = "Service Work (NGO, gov,etc.)", "14" = "NREGA","15" =  "Housewife","16" =  "Other")) +
        coord_flip() +
        theme_minimal () +
        labs(x = "", y = "Total Households", fill = "Select Village:") + scale_fill_viridis_d()
      ggplotly(socplot, tooltip = c("text"))
    }
    else if (ocuVar() == "Proportion of Households Involved in Agricultural Farming") {
      agfaplot <- ggplot(grouped, aes(forcats::fct_rev(village),prop_farm*100, fill = village)) + 
        geom_col(hoverinfo = "text", aes(text = paste("Village:", village,"<br>Percentage: ", round(prop_farm*100, 3), "%"))) + 
        labs(x = "", y = "Percentage", title = "") + coord_flip() + theme(legend.position = "none") + scale_fill_viridis_d()
      ggplotly(agfaplot,tooltip = c("text"))
    }
    else if (ocuVar() == "Average Amount of Land Owned by Village") {
      mean_land_plot <- ggplot(land_stats, aes(x = forcats::fct_rev(villages), y = mean_land_value, fill = villages)) +
        geom_col(hoverinfo = "text", aes(text = paste("Village:", villages,"<br>Mean Land Owned: ", round(mean_land_value,3)))) +
        coord_flip() + theme(legend.position = "none") +
        labs(x = "", y = "Land Owned (Kathas)") + scale_fill_viridis_d()
      ggplotly(mean_land_plot, tooltip = c("text"))
      
    }
    else if (ocuVar() == "Proportion of Households Owning Assets") {
      assetplot <- ggplot(assets_long, aes(property, percentage, fill = property, text = paste(""))) + 
        geom_col(hoverinfo = "text", aes(text = paste("Property: ", property,
                                  "<br>Percentage: ", percentage))) + 
        labs(x = "Asset", y = "Percentage" ,title = "") + 
        theme(legend.position = "none") +
        rotate_x_text(angle = 33, size = rel(1)) +
        scale_fill_viridis_d()    
        
      ggplotly(assetplot, tooltip = c("text"))
    }
    else if (ocuVar() == "Average Amount of Land Fallowed by Village") {
      land_fallow_plot <- ggplot(land_fallow, aes(x = forcats::fct_rev(village), y = sum, fill = village)) +
        geom_col(hoverinfo = "text", aes(text = paste("Village:", villages,"<br>Land Fallowed: ", sum)))+
        theme(legend.position = "none") +
        labs(x = "", y = "Total Land Fallowed", caption = "*Note: For missing bars, villages did not have any land fallowed")+
        coord_flip() + scale_fill_viridis_d()
      ggplotly(land_fallow_plot, tooltip = c("text"))
    }
    else if (ocuVar() == "Average Job Duration for Head of Household") {
      job_duration_plot <- ggplot(job_duration_summary, aes(x = forcats::fct_rev(villages), y = job_duration_avg, fill = villages)) +
        geom_col(hoverinfo = "text", aes(text = paste("Village:", villages,"<br>Average Job Duration: ", round(job_duration_avg,2), "months"))) + 
        coord_flip()+
        labs(x= "", y = "Average Job Duration (Months)")+
        ggtitle("") +
        theme(legend.position = "none") + scale_fill_viridis_d()
      ggplotly(job_duration_plot, tooltip = c("text"))
    }
    
  })
  
  #financial  tabset -----------------------------------------------------
  finVar <- reactive({
    input$findrop
  })
  
  output$finplot <- renderPlotly({
    if (finVar() == "Number of Households that Own a Business") {
      village_bus_count_plot <- ggplot(dat_bus, aes(x= Village, y = households, fill = key)) + 
        geom_col(position = 'stack', hoverinfo = "text", aes(text = paste("Households: ", households, "<br>Percentage:",`percentage`,"%\n", "Key:", key))) + 
        labs( x= "", y = "Total Households", fill = "") + 
        theme_classic() + 
        #ggtitle("Households That Own a Business") +
        coord_flip()
      ggplotly(village_bus_count_plot, tooltip = c("text"))
      }
    
    else if (finVar() == "Income vs Remmitances (October 2018 - November 2019)") {
      rem_inc <- ggplot(baseline.summary, aes(rmt_total, full_inc, color= village)) +
        geom_point(data=baseline.summary, shape=17, size=3, hoverinfo = "text", 
                   aes(text = paste("Village: ", village, "<br>Total Remmitance: ", round(rmt_total,2), "<br>Total Income: ", round(full_inc, 2)))) +
        labs(x="Average Weekly Remmitances", y="Average Weekly Income", color="Villages") + 
        ggtitle("") + scale_color_viridis_d() +coord_flip() 
      ggplotly(rem_inc, tooltip = c("text"))
    }
    else if (finVar() == "Average Monthly Salary per Household by Village")  {
      salplot <- ggplot(m_salary, aes(village, round(avg_salary, digits = 2), fill = village)) + 
        geom_col(hoverinfo = "text", aes(text = paste("Average Salary:", round(avg_salary,2), "₹"))) + 
        labs(x = "", y = "Indian Rupees ₹" ,title = "", fill = "") +
        theme(legend.position = "none") + scale_fill_viridis_d() +
        rotate_x_text(angle = 33, size = rel(1))
      ggplotly(salplot, tooltip = c("text"))
    }
    else if (finVar() == "Number of Times Households Saved Money in Year Prior to Baseline Survey (October 2018 - November 2019)") {
      savplot <- ggplot(nbsavcount, aes(x = nb_put_saving, y = n, fill = "red")) +
        geom_point(hoverinfo = "text", aes(text = paste("Number of Households: ", n, "<br>Number of Times Household Saved: ", nb_put_saving))) +
        labs(x = "Total Households ", y = " Number of Times Household Saved") +
        theme_classic() +
        theme(legend.position="none")
      ggplotly(savplot, tooltip = c("text"))
    }
    
    else if (finVar() == "Percentage of Migrant Workers per Household") {
      migplot <- ggplot(migrant_prop, aes(forcats::fct_rev(village), migrant_proportion, fill = village)) + 
        geom_col(hoverinfo = "text", aes(text = paste("Percentage: ", round(migrant_proportion, 2), "%"))) + theme(legend.position = "none") + 
        labs(x = "", y = "Percentage", title = "", fill = "") + coord_flip()+
        scale_fill_viridis_d()
      ggplotly(migplot, tooltip = c("text"))
    }
    
    
  })
  
  # High Frequency Data Output------------------- 
  
  # rmt plot output
  # Filter by inputt
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
      labs(x = "Date", y = "Average Weekly Remittance (INR)", caption = "Mean: 205.61   Median: 107.14", color = "Villages") +
      #ggtitle("Average Weekly Household Remittance Income by Village")+ #(11/16/18 - 10/31/19)
      #scale_color_brewer(palette = "Spectral")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) + 
      scale_color_viridis_d()+
      theme(plot.caption = element_text(size = 12))+
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
  
  # Plot
  output$exp <- renderPlot({
    ggplot(filtered_exp(), aes(x=week_num, y=total_spending, color = village, na.rm=TRUE)) +
      geom_line() +
      labs(x="Date", y="Average Weekly Expenditure (INR)", caption = "Mean: 1982.77   Median: 1832.1", color = "Villages") +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) + 
      scale_color_viridis_d()+
      theme_classic()+
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_exp(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
    
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
      labs(x = "", y = "Income (INR)", color = "Village",
           caption = "Mean: 1395.61   Median: 1341.82") + 
      scale_color_viridis_d()+
      theme_classic()+
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_inc(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
  })
  
  filtered_malefemaleinc <- reactive({
    malefemale_inc  %>% 
      filter(village %in% input$village_inc)
  })
  
  output$malefemaleinc <- renderPlot({
    ggplot(filtered_malefemaleinc(), aes(x = week,y = !!input$Gender, color = village)) + geom_line() + 
      #geom_line(aes(y = !!input$gender, color = village), linetype = "twodash") +  
      labs(x = "", y = "Income (INR)", color = "Village") + 
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) + scale_color_viridis_d()+
      geom_rect(data = filtered_event_inc(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
  })
  
  # filtered_fullinc <- reactive({
  #    fullinc  %>% 
  #      filter(village %in% input$village_inc)
  #  })
  #  
  #  output$fullinc <- renderPlot({
  #    ggplot(filtered_fullinc(), aes(x=week_num, y=full_inc, color = village, na.rm=TRUE)) +
  #      geom_line() + labs(title ="Full Income by Village") + xlab("Date") + ylab("Full Income (INR)") +
  #      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)
  #  })
  
  filtered_totalinc <- reactive({
    totinc  %>% 
      filter(village %in% input$village_inc)
  })
  
  output$totalinc <- renderPlot({
    qplot(x=week_num, y=inc_total, color = village,
          data=filtered_totalinc(), na.rm=TRUE,
          #main="Total Income by Village",
          xlab="Date", ylab="Total Income (INR)", geom = "line") +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40)) + scale_color_viridis_d()+
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
      #ggtitle("Average Weekly Consumption Expenditure by Village")+
      labs(x = "", y = "Average Consumption Expenditure (INR)", caption = "Mean: 766.13  Median: 731.68", color = "Villages")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      theme(plot.caption = element_text(size = 12))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_viridis_d()
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
      #ggtitle("Average Consumption Items Bought a Week")+
      labs(x = "", y = "No. of Consumption Items Bought", color = "Villages", caption = "Mean: 7.2  Median: 7.2")+
      theme(plot.caption = element_text(size = 12))+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_viridis_d()
  })
  
  # Filtered consumption by group
  
  filtered_cs_food <- reactive({
    avg_cs_food %>% 
      filter(village %in% input$village_cs_food)
  })
  
  # Consumption by food group plots
  
  output$food_plot <- renderPlot({
    ggplot(filtered_cs_food(), aes(x = week, y = !!input$food_group, color = village))+
      geom_line()+
      theme_classic()+
      labs(x = "", y = "Average Weekly Expenditure", color = "Villages", caption = "Mean: 721.41  Median: 686.96")+
      #ggtitle("Average Consumption Expenditure on Food Items")+
      theme(plot.caption = element_text(size = 12))+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      geom_rect(data = filtered_event_cs_food(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
  })
  # cs table
#  output$cs_table <- DT::renderDT({
#    avg_cs_table
#  })
  
  filtered_non_food_cs <- reactive({
    non_food_cs %>% 
      filter(village %in% input$village_cs_nonfood)
  })
  
  output$nonfood_plot <- renderPlot({
    ggplot(filtered_non_food_cs(), aes(x = week, y = !!input$nonfood_group, color = village)) +
      geom_line()+
      theme_classic()+
      labs(x = "", y = "Average Weekly Expenditure", color = "Villages", caption = "Mean: 882.22  Median: 769.75")+
      theme(plot.caption = element_text(size = 12))+
      #ggtitle("Average Consumption Expenditure on Non-Food Items")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      geom_rect(data = filtered_event_cs_nonfood(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)+
      scale_color_viridis_d()
    
  })
  # Render non food table
  
 # output$nonfood_table <- DT::renderDT({
  #  nonfood_table
 # })
  
  #Event Filtered
  
  filtered_event <- reactive({
    event_periods %>% 
      filter(Events %in% input$event_choose)
  })
  
  ###Shock plot output  -----------------------------------------------------
  
  #shock_all plot output
  output$shocks_all <- renderPlot({
    shocks_all
  })
  
  #shock_village plot output
  output$shocks_village <- renderPlot({
    shocks_village
  })
  
  #shock_by_year plot output
  output$shocks_by_year <- renderPlot({
    shocks_by_year
  })
  
  #shock_2009 plot output
  output$shocks_plot_2009 <- renderPlot({
    shocks_plot_2009
  })
  
  #cope_2009 plot output
  output$cope_2009_plot <- renderPlot({
    cope_2009_plot
  })
  
  #shock_relocation_2009_yn plot output
  output$shock_relocation_2009_yn <- renderPlot({
    shock_relocation_2009_yn
  })
  
  #shock_relocation_2009 plot output
  output$shock_relocation_2009 <- renderPlot({
    shock_relocation_2009
  })
  
  #Cope_plot_2009 filter 
  
  filtered_cope <- reactive({
    shocks_cope %>%
      filter(village %in% input$village_selecter)
  })
  
  output$cope_2009 <- renderPlot({
    ggplot(filtered_cope(), aes(shk_2009_cope, fill = village)) + geom_histogram() + 
      labs(x = "", y = "" ,fill = "Village") + scale_fill_viridis_d() + 
      theme(axis.text = element_text(size = 5)) +
      scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), labels = str_wrap(cope_labels, width = 30), limits = c(0:20)) + 
      coord_flip() 
  })
  
  #Cope_plot_2009 filter 
  
  filtered_relocation_yn <- reactive({
    shock_relocation %>%
      filter(village %in% input$village_selecter)
  })
  
  output$relocation_2009_yn <- renderPlot({
    ggplot(filtered_relocation_yn(), aes(shk_2009_reloc_yn, fill = village)) + geom_bar() + 
      labs(x = "", y = "No. of Households" ,fill = "Village") + 
      scale_x_discrete(breaks = c(0,1,2), labels = str_wrap(relocation_labels, width = 30), limits = c(0:2)) + 
      scale_fill_viridis_d()
    
  })
  
  filtered_relocation <- reactive({
    shock_relocation_where %>%
      filter(village %in% input$village_selecter)
    
  })
  
  output$relocation_2009 <- renderPlot({
    
    ggplot(filtered_relocation(), aes(shk_2009_reloc1, fill = village)) + 
      geom_bar() + labs(x = "", y = "No. of Households" ,fill = "Village") + 
      scale_x_discrete(breaks = c(1,2,3,4,5,6), labels = str_wrap(relocation_where_labels, width = 20), limits = c(1:6)) + 
      scale_fill_viridis_d() + coord_flip() +  theme(axis.text = element_text(size = 8))
    
    
  })
  
  
  
}


shinyApp(ui = ui, server = server)