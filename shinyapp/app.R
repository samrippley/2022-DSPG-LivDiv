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
fd <- livdiv 
meals <- fd %>%
  select(c("hhid", "date", "fs_skipmeals", "fs_reducemeals", "village","hhid", "week_num"))
meals$date <- as_date(meals$date)
borrow <- fd %>%
  select(c("hhid", "date", "d_br", "d_br_cash","d_br_inkind", "br_amt","br_amtdue", "village","hhid", "week_num"))
borrow$date <- as_date(borrow$date)
purp <- fd %>%
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

purpose <- purp %>%
  select(c("br_purpose_cons", "br_purpose_exp", "br_purpose_fee", "br_purpose_loan", "br_purpose_asset", "br_purpose_ag", "village", "week_num")) %>%
  group_by(village) %>%
  summarize_at(c("br_purpose_cons", "br_purpose_exp", "br_purpose_fee", "br_purpose_loan", "br_purpose_asset", "br_purpose_ag"), sum, na.rm=TRUE) 
pamr <- purpose %>% 
  filter(village == "Amrabati") 
pamr <- t(pamr)
pamr <- as.data.frame(pamr)
pamr <- pamr %>% slice(-c(1))
pamr <- data.frame(A = c("Consumption", "Other Expenses", "Fees Due", "Payback Other Loan", "Asset Purchase", "Agriculture Purchases"),
                   B = c(pamr$V1))
purposenv <- purp %>%
  select(c("br_purpose_cons", "br_purpose_exp", "br_purpose_fee", "br_purpose_loan", "br_purpose_asset", "br_purpose_ag", "village", "week_num")) %>%
  summarize_at(c("br_purpose_cons", "br_purpose_exp", "br_purpose_fee", "br_purpose_loan", "br_purpose_asset", "br_purpose_ag"), sum, na.rm=TRUE) 
purposenv <- t(purposenv)
purposenv <- as.data.frame(purposenv)
df <- data.frame(A = c("Consumption", "Other Expenses", "Fees Due", "Payback Other Loan", "Asset Purchase", "Agriculture Purchases"),
                 B = c(purposenv$V1))
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
assets <- baseline %>% select(contains("asset")) %>% select(contains("num"))  %>% summarize(Stove = sum(asset_stove_num)/n(), Bike = sum(asset_bike_num)/n(), Car = sum(asset_car_num)/n(), Waterpump = sum(asset_waterpump_num)/n(), Generator = sum(asset_generator_num)/n(), Solarpanel = sum(asset_solarpanel_num)/n(), Bed = sum(asset_bed_num)/n(), Fridge = sum(asset_fridge_num)/n(), Almirah = sum(asset_almirah_num)/n(), PC = sum(asset_pc_num)/n(), TV = sum(asset_tv_num)/n(), Phone = sum(asset_mobile_num)/n(), Waterfilter = sum(asset_waterfilter_num)/n())

assets_long <- gather(assets, property, measurement, Stove:Waterfilter)
assets_long["measurement"] <- round(assets_long$measurement, digits = 2)


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

migrant_prop <- baseline %>% group_by(village) %>% summarize(migrant_proportion = mean(hh_migrant_prop))

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
  ggtitle("Method of Receiving Remittances")+
  geom_text(aes(label = method_values), size = 3) + scale_fill_viridis_d()


# leaflet data --------------------------------------------------------------------

require(rgdal)

ind <- st_read(dsn = paste0(getwd(), "/data"), "gadm36_IND_3", stringsAsFactors = TRUE)

sundarban <- subset(ind, NAME_2 %in% c('North 24 Parganas','South 24 Parganas'))
d.sundarban<-st_union(sundarban)
village_all <- st_read(dsn = paste0(getwd(), "/data"), "Village, GP coordinates", stringsAsFactors = TRUE)

village <- subset(village_all, Village.Na %in% c("Amrabati","Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar","Lakshmi Janardanpur","Parghumti","Purba Dwarokapur","Gangasagar","Shibpur"))

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "lightred"
)

map_leaflet <- leaflet(data = d.sundarban) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "green",
    stroke=TRUE,
    weight = 1,
    smoothFactor = 0.2,
    opacity = 1.0,
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(color = "white",
                                        weight = 2,
                                        bringToFront = TRUE)) %>%
  addAwesomeMarkers(~lon, ~lat, label = ~as.character(Village.Na), icon=icons, data=village)


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
  ggtitle("Purpose for Receiving Remittances")+
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
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40) + scale_color_viridis_d()
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
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10),labels = str_wrap(shock_labels, width = 25) ,limits = 0:10) + 
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
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10),labels = str_wrap(shock_labels_2009, width = 25) ,limits = 0:10) + 
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
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), labels = str_wrap(cope_labels, width = 30), limits = 0:20) + 
  coord_flip() 

## Relocation Status after 2009 Shock

shock_relocation <- baseline %>% select(village, shk_2009_reloc_yn)

relocation_labels <- c("No", "Yes, for under a month", "Yes, for over a month")

shock_relocation_2009_yn <- ggplot(shock_relocation, aes(shk_2009_reloc_yn, fill = village)) + geom_bar() + 
  labs(x = "", y = "No. of Households" ,title = "Relocation Status after Shock", fill = "Village") + 
  scale_x_discrete(breaks = c(0,1,2), labels = str_wrap(relocation_labels, width = 30), limits = 0:2) + 
  scale_fill_viridis_d()

## Where they Relocated after 2009 Shock

shock_relocation_where <- baseline %>% select(village, shk_2009_reloc1)

relocation_where_labels <- c("Within same village","Other village in Sundarbans",
                             "Village outside Sundarbans, within West Bengal", "Kolkata", 
                             "Other Urban area outside Sundarbans, within West Bengal","Urban area outside West Bengal")

shock_relocation_2009 <- ggplot(shock_relocation_where, aes(shk_2009_reloc1, fill = village)) + geom_bar() + 
  labs(x = "", y = "No. of Households" ,title = "Relocation Areas", fill = "Village") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6), labels = str_wrap(relocation_where_labels, width = 20), limits = 1:6) + 
  scale_fill_viridis_d() + coord_flip() +  theme(axis.text = element_text(size = 8))


##Male and Female Income

malefemale_inc <- livdiv %>% select(village, week, inc_female, inc_male) %>% 
  group_by(week, village) %>% 
  summarize(avg_male_inc = mean(inc_male, na.rm = TRUE), avg_female_inc = mean(inc_female, na.rm = TRUE)) 

ggplot(malefemale_inc, aes(x = week)) + geom_line(aes(y = avg_male_inc, color = village)) + 
  geom_line(aes(y = avg_female_inc, color = village)) + 
  labs(x = "", y = "Income (INR)", title = "Male and Female Income", color = "Village") + 
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40) 

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
  ggtitle("Average Weekly Consumption Expenditure by Village")+
  labs(x = "", y = "Average Consumption Expenditure (INR)", caption = "Mean: 766.13  Median: 731.68", color = "Villages")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)+
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
  ggtitle("Average Consumption Items Bought a Week")+
  labs(x = "", y = "No. of Consumption Items Bought", color = "Villages")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)

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
  ggtitle("Average Weekly Expenditure on Staple Items ")+
  labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 463.87  Median: 431.20",
       subtitle = "(Rice/Grains, Flour, Vegetables, Fruits, Tubers, Beans and Spices)")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)+
  theme(plot.caption = element_text(size = 10))

# Meat plot

cs_meat_plot <- ggplot(avg_cs_food, aes(x = week, y = `Average Meat Expenditure`, color = village))+
  geom_line()+
  theme_classic()+
  ggtitle("Average Weekly Expenditure on Meat")+
  labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 158.97  Median: 431.20",
       subtitle = "(Red Meat, Fish, and Poultry)")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)+
  theme(plot.caption = element_text(size = 10))

# Other consumption items plot

cs_other_plot <- ggplot(avg_cs_food, aes(x = week, y = `Average Other Expenditure`, color = village))+
  geom_line() +
  theme_classic()+
  ggtitle("Average Weekly Expenditure on 'Other' Items")+
  labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 113.75  Median: 111.94",
       subtitle = "(Eggs, Dairy, Packaged Foods, Tea, and Other Food Items)")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)+
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

filtered_non_food_cs <- reactive({
  non_food_cs %>% 
    filter(village %in% input$village_cs_nonfood)
})

# Events data -------------------------------------
Events <- c("Kharif Crop Harvest", "Rabi Crop Harvest","Honey Harvest", "Fani Cyclone", "Bulbul and Matmo Cyclone", "Vayu Cyclone", "Hikaa Cyclone",
            "Republic Day", "Rama Navami", "Eid Al-Fitr", "Indian Independence Day", "Dussehra", "Diwali")
start_week <- c(2, 0, 19, 22, 48, 30, 43, 10, 20, 28, 38, 46, 49)
end_week <- c(12, 14, 32, 24, 49, 31, 44, 10.2, 20.2, 28.2, 38.2, 46.2, 49.2)
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
                 tabPanel("Project Overview", value = "overview",
                          
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
                                          p("We examine the Sundarbans in West Bengal, India, which has faced increasing climate changes in recent years for this project. The Sundarbans region has experienced a disproportionate number of climate disasters such as flooding and cyclones over the past decade. Residents who primarily engage in small-scale agriculture are forced to diversify their likelihood strategies using out-migration and reduced farming to cope with the increasing environmental changes."),
                                          p("The overall goal of this project is to evaluate livelihood-diversification strategies using weekly financial data for approximately 300 households from 10 representative villages in the region. The team aims to create a public-facing dashboard to describe and visualize households' livelihood diversification strategies, including changes in income, expenditure, and consumption patterns. The insights from this dashboard are important for designing effective and targeted poverty-reducing strategies and aiding those affected by shocks such as natural disasters and climate change."),
                                         img(src='sunphoto2.png', align = "center", width = "95%")
                                          )
                          ),
                          
                          #fluidRow(align = "center",
                          # p(tags$small(em('Last updated: August 2021'))))
                          
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
                                          p("Gupta et al. (2021) use financial diaries to capture high-frequency data on household income, expenditure, and consumption behavior. As such, we have weekly financial and economic activities for approximately 300 households for an entire year (November 2018 to October 2019)."),
                                          p("Household members were trained and provided instructions to independently record their financial activities in diaries (see image below, insert screenshot of image) before data collection. These diaries include information on weekly income, remittances, borrowing, lending, expenditure on consumption, and non-consumption items.")
                                          
                                          
                                   ),
                                   #fluidRow(align = "center",
                                   #    p(tags$small(em('Last updated: August 2021'))))
                          ),
                          fluidRow(align = "center",
                            img(src='Picture2.png', width = "75%"),
                          )), 
                 ## Sundarbans Region--------------------------------------------
                 navbarMenu("Sundarbans Region" ,
                          tabPanel("Villages", 
                                   
                                   fluidRow(style = "margin: 2px;",
                                            align = "center",
                                            h1(strong("Representative Villages in the Sundarbans"),
                                             
                                            )),
                                   
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              column(4, 
                                                     h4(strong("Sampled Villages")),
                                                     p("The Sundarbans are a cluster of islands located in the Bay of Bengal that spans across India and Bangladesh. Gupta et al. (2021) collected household-level data from a representative sample of rural households in the Sundarbans region. Our villages are located on the Indian side of the Sundarbans in West Bengal, India across the South 24 Parganas and North 24 Parganas districts."),
                                                     p("Gupta et al. (2021) randomly chose a set of ten representative villages from five administrative blocks in the Sundarbans. While looking at the map, it is clear to see how the villages could be separated into five blocks based on location. One village is within 15 km of one other village. The representative villages are paired of as follows: Pargumti and Haridaskati Samsernagar, Bijoynagar and Birajnagar, Purba Dwarokapur and Lakshmi Janardanpur,  Amrabati  and Shibpur, and  Beguakhali and Sagar."),
                                                     p("They collected information from approximately 300 households in the 10 villages from November 2018 to October 2019. During this period, the region was struck by four different cylones. The Bengal Bay was hit by a category 4 cyclone named Fani in April as well as a category 1 cyclone named Bulbul and Matmo in October. The Arabian Sea also was hit by two category 1 cyclones during while the data was being collected.  Vayu in June and Hikaa in September."),
                                                     p("This sundarbans have different crop seasons due to varying weather patterns trhoughout the year. The Kharif crop season of Winter paddy Aman is sown during monsoon season (June-August) and harvested in winter (December – January). This is a highly water consuming crop. Additionally, the Rabi crop season for paddy is sown in winter (November – February) and harvested from March to June. Fishing occurs year-round and honey is seasonally harvested from April to June. Our representative population also celebrated festivals and holidays throughout the data collection period including- Republic day, Rama Navami, Eid al-Fitr, Indian Independence Day, Dussehra, Diwali, Mawlid and Christmas.")
                                              ),
                                              column(8, leafletOutput("map_leaflet", width = "100%"),
                                                     
                                                     
                                              )
                                     
                            )),
                            tabPanel("Timelapse", 
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center", h4(strong("Timelapse Showing Coastal Degradation")),
                                                     p(""),
                                                     br(""), tags$video(type = "video/mp4",src = "sundarbansv2.mp4", width = "80%", align = "center",controls = "controls")
                                              ), 
                                     )
                                     
                            ),
                            tabPanel("Gallery",
                            fluidRow(style = "margin: 6px;", 
                                     column(12,
                                            h2(strong("Images"))
                                            
                                            
                                     ),   
                                     mainPanel( 
                                       actionButton("previous", "Previous"),
                                       actionButton("next", "Next"),
                                       imageOutput("image")
                                       
                                     ))),
                 ),
                 
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
                                                     p("A household with only one means of support has nothing to fall back on if that livelihood is destroyed by a disaster. Due to the increasing threats in the Sundarbans region, most of the population depends on multiple sources of income. This is a common practice for climate vulnerable regions, to manage and reduce risks."),
                                                     p("We began our evaluation of livelihood behavior by looking at the head of households’ primary occupation, primary occupation duration, and secondary occupation. The most common primary and secondary occupations are Farming followed by Casual labor. These graphs show that there are differences in occupation by village. For example, the village of Amrabati has the highest proportion of workers in jobs related to fishing. The head of households’ occupations are distributed relatively similarly from primary to secondary. However, the total number of household heads in each occupation is lower in secondary occupation. The total number of household heads with a primary occupation is 252 while 149 has a secondary occupation."),
                                                     p("The average job duration by village for head of households is 6 to 8 months. This implies that on average, they work in their primary occupation for ½ to ¾ of the year. This is evidence that the population is relying on different, multiple activities within a year."),
                                                     h4(strong("Agriculture")),
                                                     p("Our data set contained an agricultural crop variable where participants gave a yes/no answer to the question “Did your household cultivate any agriculture crops in the last 12 months?” On average, across all the villages, 63.9% of the households participate in farming. When broken down by village, Amrabati had the lowest percentage of households that had cultivated crops in the given year, at 7%. Haridaskati Samsernaga had the highest percentage of households that had cultivated crops in the given year, at 86%. This was closely followed by Pargumti at 85%. Given that on average, across all the villages, 63.9% of the households participate in farming it is evident that a very shows that a large proportion of our households participated in some sort of crop cultivation."), 
                                                     p("We next evaluated Land Holding and Land Fallow by village. Land holding is the amount of land owned in Kathas. A Katha is unit of area mostly used for land measure in India and Bangladesh. One unit of katha is equivalent to 720 square feet. Pargumti and Bijoynagar have the highest average amount of land owned, with households owning over 60 kathas of land. Amrabati and Sagar have the lowest amount of land owned. Land fallow is agriculture land that needs to be left to rest and regenerate. This land is typically not in good enough condition to sustain crops. The two villages that had the lowest amount of land owned, have no land fallow. This is evidence that these villages that these villages are less involved in agriculture. ")
                                                     
                                                     
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
                                                     p("Households business: Owning a business in the Sundarbans is often too expensive and financially 
                                                       unfeasible. A majority of 88% of households do not own a business and only 12% do own a business, 
                                                       making it very uncommon. "
                                                     ),
                                                     p("Salary: On average, per month, The data shows that the monthly salaries for the households range 
                                                       anywhere between 2500 INR to 4600 INR per month. On average, Amrabati receives the highest income. 
                                                       This can indicate that the fishing industry in Amrabati is more lucrative than other jobs in other villages. 
                                                       On the other hand, the village of Sagar has the lowest monthly salary on average."
                                                     ),
                                                     p("Income/Remmitance: For the villages that are plotted on the middle/top right of the graph, 
                                                       remittances are highly impactful on the household's income. Remittances are essentially any income a 
                                                       household receives from someone working away from the household. By taking a look at the remittances we 
                                                       were able to see the impact it had on the total household income."
                                                     ),
                                                     p("Savings: On average, many households are not able to save, but sometimes some households are able to 
                                                       save once or twice during the 12-month span. It is unlikely to see many households save more than 20 times 
                                                       like some did. It is expected to see low amounts of saving because of the high poverty level in the region."
                                                     )
                                              ) ,
                                              column(8, h4(strong("Head of Household Demographics")),
                                                     selectInput("findrop", "Select Characteristic:", width = "100%", choices = c( 

                                                       "Household Business" = "Number of Households that Own a Business",
                                                       "Salary" = "Average Monthly Salary per Household by Village",
                                                       "Migrant Workers" = "mig",
                                                       "Income/Remmitances" = "Income vs Remmitances (November 2019 - October 2018)",
                                                       "Savings" = "Number of Times Households Saved Money in Year Prior to Baseline Survey (November 2018 - November 2019)"
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
                                              column(12,h4(strong("Overview")),
                                                     p("We present average weekly expenditure from Nov 2018 - Oct 2019 to examine the spending behaviors of households in the region. 
                                                       This will provide information on the changing nature of spending in the Sundarbans region due to events such as festivals, 
                                                       harvest seasons, and weather-related shocks."),
                                                     p("Expenditure is defined as spending on consumption (e.g., food) and non-consumption (e.g., rent) items. 
                                                       It appears that the largest expense for households during this period include house repairs and festival-related costs. 
                                                       The most common expenditures are food purchases."),
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
                                           tabPanel("Plot",plotOutput("exp")),
                                           tabPanel("Table", DT::DTOutput("exp_table"))
                                         )
                                       ),
                                       
                                     ),
                                     
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              h4(strong("Consumption on Food Items"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Overview")),
                                                     p("We present the average weekly expenditure on consumption items from November 2018 - October 2019. Consumption expenditure includes purchases by 
                                                     households on goods and services, excluding housing. By visualizing consumption expenditures over time, we can gain information about household spending behavior, 
                                                     identifying changes in spending, as well as which consumption items are bought most frequently. Within the data period, the Sundrabans region spent
                                                       an average of 766.13 Rupees per week on consumption items; they also had bought an average of seven consumption items per week."),
                                                     p("Additionally, we present a time series of expenditures on staple, meat, and other consumable items."),
                                                     p("- Staple Items: Rice/Grains, Flour, Vegetables, Fruits, Tubers, Beans and Spices"),
                                                     p("- Meats: Red Meat, Fish, and Poultry"),
                                                     p("- Other: Eggs, Dairy, Packaged Foods, Tea, and Sinful Items"),
                                                     p("We identified that most of consumption expenditure is being used for staple food items, followed by meats. We observed
                                                       a siginficant spike in 'Other' items, in Shibpur, in late April due to a large increase in expenditure on sinful items (tea,
                                                       cigarettes, betel leaves, bidil, etc.). These items are often deemed to be harmful to society, but provide certain satisfaction to
                                                       consumers. Therefore, this increase in consumption could suggest a communal need to celebrate or cope from a certain event."),
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
                                           tabPanel("Average Food Expenditure",plotOutput("cs_exp")),
                                           tabPanel("No. of Food Items", plotOutput("cs_item")),
                                           tabPanel("Staple Items", plotOutput("cs_staple")),
                                           tabPanel("Meats", plotOutput("cs_meats")),
                                           tabPanel("Other", plotOutput("cs_other")),
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
                                              h4(strong("Non-Food Consumption"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Overview")),
                                                     p("Furthermore, we examined consumption expenditure on non-food items, inluding: clothes, 
                                                       books and tuition, utilities, toiletries, health, home repairs, transportation, livestock,
                                                       agriculture, labor, and other non-food items."), 
                                                     p("Consumption expenditure on health, home repairs, and books/tuition
                                                       made up the largest but least frequent expenses, while utilities, toilitries, and transportation made up the most frequent
                                                       purchases. Considering farmers make up the largest proportion of occupation in the Sundarbans, it is predictable to also see 
                                                       frequent consumption expenditures on agriculture, livestock, and labor."),
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
                                           tabPanel("Plot", plotOutput("nonfood_plot"))                                         
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
                                                       we wanted to see if there are certain households in any villages where …"),
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
                                           tabPanel("Plot",plotOutput("inc")),
                                           tabPanel("Male/Female Income", plotOutput("malefemaleinc")),
                                           #tabPanel("Full Income", plotOutput("fullinc")),
                                           tabPanel("Total Income(w/o remmitance)", plotOutput("totalinc")),
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
                                                     p("The first tab depicts amount borrowed by village throughout the year. There is a spike of amount borrowed between April and July. Purba Dwarokapur, Shibpur and Sagar show major spikes in the amount borrowed during this time. With the amount reaching about 40000 INR."),
                                                     p("The “Count” tab depicts the number of Households borrowing. This data is relatively consistent throughout the year other than an early spike in Bijoynagar, with over 30 households borrowing before January of 2019.  If you deselect Bijoynagar, all other villages range from 17 to 0 households borrowing each week. The lowest number of households borrowing is between January and July, with no more than 10 households borrowing each week during these months.  A couple villages increase to above 10 households borrowing after July."),
                                                     p("The purpose of borrowing is mostly consumption. This is followed by “Other Expenses” and “Payback of Other Loans.")
                                                     
                                                     
                                                     
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
                                                  plotlyOutput("purpplot", height = "500px")
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
                                                       Fani, Category 4 (April – May 2019), and Category 1, Bulbul and Matmo (October – November 2019). The Sundarbans also could have been negatively impacted by two 
                                                       cyclones that hit the Arabian Sea during this period: Vayu (Category 1, June 8-18) and Hikaa (Category 1, September 20-26). 
                                                       It is possible households are using remittances to cope with these cyclones and weather-related shocks."),
                                                     p("Remittance impact on the livelihood of the Sundarban population can be seen as the data collected shows that the median 
                                                       weekly remittance income is 205.61 INR which is on average almost 800 INR. This significant portion of a households monthly 
                                                       income show that importance this income has on the families ability to function. The graph also does a good job at showing 
                                                       spikes in remittance income which can be either because of festivals, other celebrations, money sent because of health 
                                                       concerns, or other shocks.")
                                                     
                                                     
                                              ) ),
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
                                           tabPanel("Plot",plotOutput("rmt")),
                                           tabPanel("Table",DT:: DTOutput("rmt_table"))#,
                                           #tabPanel("Method", plotOutput("rmt_method")),
                                           #tabPanel("Purpose", plotOutput("rmt_purpose"))
                                         )
                                       ), 
                                       
                                       
                                     ),
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center", h4(strong("Remittances Sources")),
                                                     p("We also examine how households received remittances. We find that households primarily collected remittances 
                                                       in person or through a bank suggesting these methods to be the most convenient. Although a money order is a 
                                                       secure method of sending/receiving money, it requires additional fees, which may make it more expensive for 
                                                       this poverty-stricken area. Moreover, households may be more concerned about receiving the remittance quickly 
                                                       rather than safely. Also, using mobile apps can be difficult in regions where data usage is limited."),
                                                     br(""), plotOutput("rmt_method", width = "70%")
                                                     
                                                     
                                              )),
                                     #plotOutput("rmt_method", width = "65%"),
                                     
                                     fluidRow(style = "margin: 6px;", align = "justify",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center", h4(strong("Usage of Remmittances")),
                                                     p("Remittances is primarily being used for food and utility purchases, which are 
                                                       often the most essential items for households in underdeveloped regions."),
                                                     br(""), plotOutput("rmt_purpose", width = "70%")
                                                     
                                                     
                                              )),
                                     
                                     
                                     
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
                                     "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical
                                social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how 
                                information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply,
                                and our annual symposium, please visit", 
                                     a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Members")),
                                          img(src = "team-esha.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-julie.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          br(), 
                                          img(src = "team-ryan.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-john.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/samantha-rippley-58846119b/', 'Samantha Rippley', target = '_blank'), "(Virginia Tech, M.S in Agriculture and Applied Economics);",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/julie-rebstock', 'Nandini Das', target = '_blank'), "(Virgina Tech, PHD in Economics);",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/taj-cole-83a738221', 'Taj Cole', target = '_blank'), "(Virginia Tech, Undergraduate in Environmental Economics, Management, and Policy, and Minoring in Data and Decisions).",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/john-wright-9a13621a0/', 'Siddarth Ravikanti', target = '_blank'), "(Virginia Tech, Undergraduate in Statistical and Data Science)."),
                                          p("", style = "padding-top:10px;") 
                                   ),
                                   column(6, align = "center",
                                          h4(strong("VT Faculty Team Members")),
                                          # img(src = "team-posadas.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-sarah.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://www.linkedin.com/in/chanita-holmes-385577234/", 'Dr. Chanita Holmes', target = '_blank'), "(Research Assistant Professor Department of Agriculture and Applied Economics Virginia Tech)",
                                            #  br(), 
                                            #  a(href = '', 'Dr. Chanita Holmes', target = '_blank'), "(Associate Professor Department of Biology Virginia State University)."),
                                            # p("", style = "padding-top:10px;")
                                          ) ), 
                          ),
                          fluidRow(tyle = "margin-left: 100px; margin-right: 100px;",
                                   align = "center",
                                   h4(strong("Project Stakeholders")),
                                   img(src = "stake-dawn.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                   img(src = "stake-john.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                   p(a(href = '', 'Dawn Barnes', target = '_blank'), "(Virginia Cooperative Extension, Floyd County at Virginia Tech);",
                                     br(), 
                                     a(href = '', 'Jon Vest', target = '_blank'), "(Virginia Cooperative Extension, Floyd County at Virginia Tech)."),
                                   p("", style = "padding-top:10px;")
                          )
                 ),
                 
                 
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
    ggplot(df, aes(x= A, y = B, fill = A)) + geom_col() + 
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
      ggtitle("Average Weekly Expenditure on Staple Items ")+
      labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 463.87  Median: 431.20",
           subtitle = "(Rice/Grains, Flour, Vegetables, Fruits, Tubers, Beans and Spices)")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)+
      theme(plot.caption = element_text(size = 10))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
  })
  
  output$cs_meats <- renderPlot({
    ggplot(filtered_cs_meats(), aes(x = week, y = `Meats`, color = village))+
      geom_line()+
      theme_classic()+
      ggtitle("Average Weekly Expenditure on Meat")+
      labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 158.97  Median: 431.20",
           subtitle = "(Red Meat, Fish, and Poultry)")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)+
      theme(plot.caption = element_text(size = 10))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
  })
  
  output$cs_other <- renderPlot({
    ggplot(filtered_cs_other(), aes(x = week, y = `Other`, color = village))+
      geom_line() +
      theme_classic()+
      ggtitle("Average Weekly Expenditure on 'Other' Items")+
      labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 113.75  Median: 111.94",
           subtitle = "(Eggs, Dairy, Packaged Foods, Tea, and Other Food Items)")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)+
      theme(plot.caption = element_text(size = 10))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
      
  })

  
  
  
  
  #sociodemo tabset -----------------------------------------------------
  ageVar <- reactive({
    input$agedrop
  })
  
  output$ageplot <- renderPlotly({
    if (ageVar() == "Mean Age for Head of Households") {
      
      fplot <-  ggplot(by_villagemore, aes(x = village, y = head_age, fill = village, width=0.5, srt = 45)) +
        geom_col(hoverinfo = "text", aes(text = paste("Age: ", head_age)), width = "5") +
        ylab("Age") + 
        xlab("")+
        #ggtitle("Mean age for Head of Households ")  +
        theme(legend.position = "none") +
        rotate_x_text(angle = 33, size = rel(1)) + scale_fill_viridis_d()
      ggplotly(fplot, tooltip = c("text"))
    }
    else if (ageVar() == "Mean Years of Education for Head of Households") {
      splot <- ggplot(by_villagemore, aes(x = "", y= head_edu, fill = village)) +
        geom_bar(width = 1, stat = "identity") +
        facet_wrap(~village, ncol = 5) +
        geom_text(aes(label = sub), position = position_stack(vjust=1.1)) +
        labs(x = NULL, y = "Years of Education") +
        theme(legend.position="none", strip.text.x = element_text(size = 9)) + scale_fill_viridis_d()
      splot
    }
    else if (ageVar() == "Households that Live Below Poverty Line (₹240) per week") {
      village_pl_count_plot <- ggplot(dat_pl, aes(x= `Village`, y = `Households`, fill = `Key`)) + 
        geom_col(position = 'stack', hoverinfo = "text", aes(text = paste("Percentage:",Percentage,"%\n"))) + 
        labs(x= "", y = "Total Households", fill = "") + 
        theme_classic() + 
        coord_flip()
    }
    else if (ageVar() == "Household Heads Marital Status") {
      marplot <- ggplot(countmar, aes(x = head_married, y = n, fill = Gender)) +
        geom_col() +
        labs(x = "Not Married                                         Married", y = "Total Household Head", fill = "") +
        scale_x_discrete() + theme(legend.title=element_blank()) 
      marplot
    }
    else if (ageVar() == "Household Size by Village") {
      hh_size_plot <- ggplot(median_hhsize, aes(x = village, y = median, fill = village)) +
        geom_col() +
        labs( x = "", y = "Median Household Size")+
        coord_flip()+
        #ggtitle("Household Size by Village") +
        theme(legend.position="none") + scale_fill_viridis_d()
      hh_size_plot
    }
    else if (ageVar() == "Total Children per Household") {
      chhoplot <- ggplot(avg_children, aes(village, avg_children, fill = village)) + 
        geom_col(hoverinfo = "text", aes(), width = "5") + labs(x = "", y = "Average number of children" ,title = "Total Children per Household", fill = "Village") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + scale_fill_viridis_d()
      chhoplot
    }
  })
  
  

  #livelihood tabset -----------------------------------------------------
  ocuVar <- reactive({
    input$ocudrop
  })
  
  output$ocuplot <- renderPlotly({
    if (ocuVar() == "Primary Occupation for Head of Households") {
      pocuplot <- ggplot(countv, aes(x = job, y = n, fill = village)) +
        geom_col() +
        scale_x_discrete(limits = factor(1:16), labels = c("1" = "Agricultural wage worker","2" =  "Livestock worker", "3" = "Farmer", "4" = "Casual labor","5" =  "Construction/brick labor","6" =  "Gleaning/foraging","7" =  "Fisherman","8" =  "Fishery worker", "9" = "Factory worker" , "10" = "Household help" ,"11" =  "Transport related work","12" =  "Own business", "13" = "Service Work (NGO, gov,etc.)", "14" = "NREGA","15" =  "Housewife","16" =  "Other")) +
        coord_flip() +
        theme_minimal () +
        labs(x = "", y = "Total Households", fill = "") + scale_fill_viridis_d()
      pocuplot
    } 
    else if (ocuVar() == "Secondary Occupation for Head of Households") {
      socplot <- ggplot(scountv, aes(x = job, y = n, fill = village)) +
        geom_col() +
        scale_x_discrete(limits = factor(1:16), labels = c("1" = "Agricultural wage worker","2" =  "Livestock worker", "3" = "Farmer", "4" = "Casual labor","5" =  "Construction/brick labor","6" =  "Gleaning/foraging","7" =  "Fisherman","8" =  "Fishery worker", "9" = "Factory worker" , "10" = "Household help" ,"11" =  "Transport related work","12" =  "Own business", "13" = "Service Work (NGO, gov,etc.)", "14" = "NREGA","15" =  "Housewife","16" =  "Other")) +
        coord_flip() +
        theme_minimal () +
        labs(x = "", y = "Total Households", fill = "") + scale_fill_viridis_d()
      socplot
    }
    else if (ocuVar() == "Proportion of Households Involved in Agricultural Farming") {
      agfaplot <- ggplot(grouped, aes(village,prop_farm)) + geom_col(fill = "navy blue") + labs(x = "", y = "Proportion", title = "") + coord_flip() + theme_classic()
      agfaplot
    }
    else if (ocuVar() == "Average Amount of Land Owned by Village") {
      mean_land_plot <- ggplot(land_stats, aes(x = villages, y = mean_land_value, fill = villages)) +
        geom_col() +
        coord_flip() +
        #ggtitle("Average Amount of Land Owned in Each Village") +
        labs(x = "", y = "Land Owned (Kathas)") + scale_fill_viridis_d()
      mean_land_plot
   # }
    #else if (ocuVar() == "Proportion of Households that Cultivated Crops") {
     # croplot <- ggplot(grouped, aes(village,prop_farm)) + geom_col(fill = "navy blue") + labs(x = "", y = "Proportion", title = "") + coord_flip() + theme_classic()
      #croplot
    }
    else if (ocuVar() == "Proportion of Households Owning Assets") {
      assetplot <- ggplot(assets_long, aes(property, measurement, fill = property)) + geom_col() + 
        labs(x = "", y = "Proportion" , fill  = "Asset") + 
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
        geom_text(aes(label = measurement), size = 3, nudge_y = .05) + coord_flip() + scale_fill_viridis_d()
      assetplot
    }
    else if (ocuVar() == "Average Amount of Land Fallowed by Village") {
      land_fallow_plot <- ggplot(land_fallow, aes(x = forcats::fct_rev(village), y = sum, fill = village)) +
        geom_col(fill = plasma(10, alpha = 1, begin = 0, end = 1, direction = 1))+
        theme_classic() +
        labs(x = "", y = "Total Land Fallowed", caption = "*Note: For missing bars, villages did not have any land fallowed", tags = "*Note: For missing bars, villages did not have any land fallowed" )+
        coord_flip()
    }
    else if (ocuVar() == "Average Job Duration for Head of Household") {
      job_duration_plot <- ggplot(job_duration_summary, aes(x = forcats::fct_rev(villages), y = job_duration_avg, fill = villages)) +
        geom_col( fill = plasma(10, alpha = 1, begin = 0, end = 1, direction = 1)) + 
        coord_flip()+
        labs(x= "", y = "Average Job Duration (Months)")+
        ggtitle("") +
        theme_classic() + scale_fill_viridis_d()
      job_duration_plot
    }
    
  })
  
  #financial  tabset -----------------------------------------------------
  finVar <- reactive({
    input$findrop
  })
  
  output$finplot <- renderPlotly({
    if (finVar() == "Number of Households that Own a Business") {
      village_bus_count_plot <- ggplot(dat_bus, aes(x= Village, y = `households`, fill = key)) + 
        geom_col(position = 'stack', hoverinfo = "text", aes(text = paste("Percentage:",`percentage`,"%\n"))) + 
        labs( x= "", y = "Total Households", fill = "") + 
        theme_classic() + 
        #ggtitle("Households That Own a Business") +
        coord_flip()
    }

  else if (finVar() == "Income vs Remmitances (November 2019 - October 2018)") {
      ggplot(baseline.summary, aes(rmt_total, full_inc, color= village))+
        geom_point(data=baseline.summary, shape=17, size=3) +
        labs(x="Average Weekly Remmitances", y="Average Weekly Income", color="Villages") + 
        ggtitle("") + scale_color_viridis_d() +coord_flip() 

    }
else if (finVar() == "Average Monthly Salary per Household by Village")  {
      salplot <- ggplot(m_salary, aes(village, avg_salary, fill = village)) + geom_col() + 
        labs(x = "Villages", y = "Indian Rupees ₹" ,title = "", fill = "") +
        theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + scale_fill_viridis_d()
      salplot
    }
    else if (finVar() == "Number of Times Households Saved Money in Year Prior to Baseline Survey (November 2018 - November 2019)") {
      savplot <- ggplot(nbsavcount, aes(x = nb_put_saving, y = n, fill = "red")) +
        geom_point() +
        labs(x = "Total Households ", y = " Number of Times Household Saved") +
        theme_classic() +
        theme(legend.position="none")
      savplot
    }
    
    else if (finVar() == "mig") {
      migplot <- ggplot(migrant_prop, aes(village, migrant_proportion, fill = village)) + 
        geom_col() + theme_classic() + 
        labs(x = "", y = "Proportion", title = "", fill = "") + coord_flip()

      migplot
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
      labs(x = "Date", y = "Average Remittance Income (INR)", caption = "Mean: 205.61   Median: 107.14", color = "Villages") +
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
      labs(x="Date", y="Average Weekly Expenditure (INR)", caption = "Mean: 1982.77   Median: 1832.1") +
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
      theme(plot.caption = element_text(size = 10))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
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
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      geom_rect(data = filtered_event_cs(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
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
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = c(10:40))+
      geom_rect(data = filtered_event_cs_food(), inherit.aes = F, aes(xmin= start_week, xmax= end_week, ymin=0, ymax= Inf, fill = Events), alpha=0.25)
    
  })
  
  filtered_non_food_cs <- reactive({
    non_food_cs %>% 
      filter(village %in% input$village_cs_nonfood)
  })
  
  output$nonfood_plot <- renderPlot({
    ggplot(filtered_non_food_cs(), aes(x = week, y = !!input$nonfood_group, color = village)) +
      geom_line()+
      theme_classic()+
      labs(x = "", y = "Average Weekly Expenditure", color = "Villages", caption = "Mean: 882.22  Median: 769.75")+
      #ggtitle("Average Consumption Expenditure on Non-Food Items")+
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





