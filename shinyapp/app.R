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

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
village_vector = c("Amrabati","Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar","Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur")


# data -----------------------------------------------------------

load("data/livdivdata.RData")

baseline <- livdiv %>%
  slice(1:306,)


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

purp <- fd %>%
  select(c("date", "br_purpose_cons", "br_purpose_exp", "br_purpose_fee", "br_purpose_loan", "br_purpose_asset", "br_purpose_ag", "village","hhid", "week_num"))
purpose <- purp %>%
  select(c("br_purpose_cons", "br_purpose_exp", "br_purpose_fee", "br_purpose_loan", "br_purpose_asset", "br_purpose_ag", "village", "week_num")) %>%
  group_by(village) %>%
  summarize_at(c("br_purpose_cons", "br_purpose_exp", "br_purpose_fee", "br_purpose_loan", "br_purpose_asset", "br_purpose_ag"), sum, na.rm=TRUE) 
names <- c("village", "Consumption", "Other Expenses", "Fees Due", "Payback Other Loan", "Asset Purchase", "Agriculture Purchases")

purpose <- rbind(names, purpose)
names(purpose) <- purpose[1,]
purpose <- t(purpose)
purpose <- purpose[,-1]
purpose <- t(purpose)
purpose <- as.data.frame(purpose)

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

baseline.summary <- baseline %>% select(village, exp_total, full_inc, rmt_total) %>% group_by(village) %>%summarise_all(mean) 

#land owned data 
villages <- c("Amrabati","Beguakhali","Bijoynagar","Birajnagar","Haridaskati Samsernagar","Lakshmi Janardanpur","Pargumti","Purba Dwarokapur","Sagar","Shibpur") 
land_owners <- c(2, 18, 41, 14, 25, 21, 24, 21, 10, 24)
max_land_value <- c(16, 160, 510, 80, 120, 200, 250, 70, 80, 180 )
min_land_value <- c(10, 10, 8, 10, 1, 10, 3, 7, 2, 3)
mean_land_value <- c(13, 45.44, 60.95, 42.14, 45.44, 50.9, 64, 36.35, 23.7, 41.58)
sum_value <- c(26, 818, 2499, 590, 1136, 1069, 1537, 763.5, 237, 998)
land_stats <- data.frame(villages, land_owners, max_land_value, min_land_value, mean_land_value, sum_value)

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

# Business counts
villages_2 <- c(rep("Amrabati", 2), rep("Beguakhali", 2), rep("Bijoynagar", 2), rep("Birajnagar", 2), rep("Haridaskati Samsernagar", 2), rep("Lakshmi Janardanpur",2), rep("Pargumti",2),rep("Purba Dwarokapur", 2), rep("Sagar", 2), rep("Shibpur",2))
Key <- rep(c("No", "Yes"), 2)
values_bus <-c(26,2,27,3,48,2,24,2,27,3,25,3,24,4,21,7,21,7,25,3)

#prop_bus_values <- c(0.93,0.07, 0.9,0.1,0.96,0.04,0.86,0.14,0.9,0.1,0.9,0.1,0.86,0.14,0.75,0.25,0.75,0.25,0.9,0.1)
prop_bus_values <- c("93%","7%","90%","10%","96%","4%","86%","14%","90%","10%","90%","10%","86%","14%","75%","25%","75%","25%","90%","10%")
dat_bus <-  data.frame(villages_2, Key, values_bus, prop_bus_values)
dat_bus

# Poverty line counts
values_pl <- c(17,11,20,10,32,18,19,9,14,16,17,11,18,10,23,5,21,6,21,7)
prop_pl_values <- c("60%", "40%", "67%", "33%", "64%","36%","68%","32%","53%","                47%","60%","40%","64%","36%","82%","18%","77%","23%","75%","25%" )
dat_pl <- data.frame(villages_2, Key, values_pl, prop_pl_values)
dat_pl

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
  ggtitle("Average Remittance Income by Village") + #(11/16/18 - 10/31/19)
  #scale_color_brewer(palette = "Spectral")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40) + scale_color_viridis_d()
#annotate(geom = "text", aes(x = unlist(months)))
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
  theme(plot.caption = element_text(size = 10))+
  geom_vline(xintercept = c(4,19,22, 29, 44, 48), linetype = "longdash")+
  geom_text(aes(x=4, label="Kharif Crop Harvest", y=1500), colour="black", angle = 90, vjust = -1, size = 3)+
  geom_text(aes(x=19, label="Rabi Crop Harvest", y=1500), colour="black", angle = 90, vjust = -1, size = 3)+
  geom_text(aes(x=22, label="Fani", y=1500), colour="black", angle = 90, vjust = -1, size = 3)+
  geom_text(aes(x=29, label="Vayu", y=1500), colour="black", angle = 90, vjust = -1, size = 3)+
  geom_text(aes(x=44, label="Hikaa", y=1500), colour="black", angle = 90, vjust = -1, size = 3)+
  geom_text(aes(x=48, label="Bulbul & Matmo", y=1500),colour = "black", angle = 90, vjust = -1, size = 3)

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
  summarise("Average Staple Expenditure" = mean(na.omit(cs_ricegrains + cs_wheatflour + cs_veg +
                                                          cs_fruit +cs_tubers + cs_pulsespice)), 
            "Average Meat Expenditure" = mean(na.omit(cs_redmeat +cs_fishshrimp + cs_poultry)), 
            "Average Other Expenditure" = mean(na.omit(cs_eggs + cs_dairy +cs_packaged + cs_sinful)))

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
                                   h1(strong("Livelihood Diversification Using High-Frequency Data"),
                                      h2(strong("Sundarbans")),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Polytechnic Institute and State University"),
                                      #h4("[updat this]"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;", align = "justify",
                                   column(4,
                                          h2(strong("The Setting")),
                                          
                                          p("The Sundarbans is a cluster of low-lying islands in the Bay of Bengal spans across India and Bangladesh. The Sundarbans area hosts the largest mangrove forests in the world, supporting an exceptionally rich diversity of flora and endangered fauna such as the Bengal tiger, Estuarine crocodile, Indian python, and Irawadi dolphins."),
                                          p("The vast delta is formed by the connection of the Ganga, Brahmaputra, and Meghna rivers. It also has a complex network of tidal waterways, creeks, and mudflats. The area's unique boundaries act as a shelter belt from natural disasters such as cyclones, tidal surges, and seawater seepage. Despite this natural protective system and being a World Heritage Site with conservation requirements, the Sundarbans is considered endangered under the ICUN Red List of Ecosystems due to increasing climate, population and agricultural farming."),
                                          p("The Sundarbans supplies sustainable livelihoods for 4 million people living in small villages near the mangrove forests. Most residents work in various agricultural occupations, including farmers, woodcutters, fishers, and honey gatherers. Farmers, primarily landless laborers, commonly farm a single crop (Aman paddy) in the rainy season and sell food to intermediaries or traders. The woodcutters obtain traditional forest produce like timber, fuelwood, and pulpwood. A large-scale harvest of shrimps, fish, crustaceans, and honey from the forests are also typical. However, with the ongoing climate and population changes, forest conservation efforts have placed a cap on harvesting. For example, in 2022, authorities began issuing three-month honey passes to collect wax from beehives.")
                                   ),
                                   column(4,
                                          h2(strong("Project Background")),
                                          
                                          p("The Sundarbans faces an increasing threat to its ecological system due to several manmade and natural causes. First, cyclones, common to this area, are getting more frequent and more serve. From 1961 to 2022, 15 cyclones hit this area, with at least one occurring yearly in the past four years. This has led to the forest incurring severe damages, gradually causing the area to shrink. Second, there is an increase in deforestation due to increasing population and commercial uses. There is also a decrease in aquatic animals due to increased fishing. Additionally, the biological makeup of the forest, such as salinity, soil pH, and reduced freshwater, are being altered due to climate change leading to more fallow land."),
                                          p("Agricultural-dependent families bear the brunt of these increasing threats to the Sundarbans. This is evident by the growing out-migration of the working population to cities and towns as a coping mechanism. Remittance income from this domestic migration has become one of the significant sources of income to protect residents' livelihood.")
                                   ),
                                   
                                   column(4,
                                          h2(strong("Project Goals")),
                                          p("Climate change is a global issue; however, its impact is not felt equally across all regions. Developing countries, especially areas with widespread poverty and poor infrastructure, are more ill-equipped to cope with these environmental threats. The worsening of extreme weather patterns such as high temperatures, droughts, floods, and rising sea levels are especially problematic for countries with large coastal areas and populations that primarily depend on agriculture for their livelihood."),
                                          p("We examine the Sundarbans in West Bengal, India, which has faced increasing climate changes in recent years for this project. The Sundarbans region has experienced a disproportionate number of climate disasters such as flooding and cyclones over the past decade. Residents who primarily engage in small-scale agriculture are forced to diversify their likelihood strategies using out-migration and reduced farming to cope with the increasing environmental changes."),
                                          p("The overall goal of this project is to evaluate livelihood-diversification strategies using weekly financial data for approximately 300 households from 10 representative villages in the region. The team aims to create a public-facing dashboard to describe and visualize households' livelihood diversification strategies, including changes in income, expenditure, and consumption patterns. The insights from this dashboard are important for designing effective and targeted poverty-reducing strategies and aiding those affected by shocks such as natural disasters and climate change.")
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
                                          p("We acquire weekly household financial and consumption data for this project from Gupta et.al (2021).  Data was collected from about 300 households in 10 representative village in the Sundarbans region from November 2018 to October 2019. ")
                                   ),
                                   
                                   column(4,
                                          h2(strong("Initial/Baseline")),
                                          p("The initial or baseline survey was conducted in November 2018. This data is the foundation of our data, allowing the team to understand this region's demographic and socio-economic characteristics. The baseline survey collected information on household demographics, economic activities, assets and landholding, shock history, migration, and agricultural behaviors.")
                                          
                                          
                                   ),
                                   column(4,
                                          h2(strong("Financial Diaries")),
                                          p("Gupta et al. (2021) use financial diaries to capture high-frequency data on household income, expenditure, and consumption behavior. As such, we have weekly financial and economic activities for approximately 300 households for an entire year (November 2018 to October 2019)."),
                                          p("Household members were trained and provided instructions to independently record their financial activities in diaries (see image below, insert screenshot of image) before data collection. These diaries include information on weekly income, remittances, borrowing, lending, expenditure on consumption, and non-consumption items.")
                                          
                                          
                                   ),
                                   #fluidRow(align = "center",
                                   #    p(tags$small(em('Last updated: August 2021'))))
                          ),
                          mainPanel(
                            img(src='Picture1.png', align = "right", width = "75%"),
                          )), 
                 ## Sundarbans Region--------------------------------------------
                 navbarMenu("Sundarbans Region" ,
                            tabPanel("Sundarbans Villages", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong(""), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center",h4(strong("Sundarbans Villages")),
                                                     p(""),
                                                     br("")
                                                     
                                                     
                                              )),
                                     fluidPage(
                                       leafletOutput("map_leaflet", width = "100%"),
                                       #p(),
                                       #actionButton("recalc", "New Points")
                                     ),
                            ),
                            tabPanel("Timelapse", 
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"),
                                              column(12, align = "center", h4(strong("Timelapse Showing Coastal Degradation")),
                                                     p(""),
                                                     br(""), tags$video(type = "video/mp4",src = "sundarbansv2.mp4", width = "80%", align = "center",controls = "controls")
                                              ), 
                                     )
                                     
                            )),
                 
                 ## Tab Demographics --------------------------------------------
                 navbarMenu("Demographics" , 
                            tabPanel("Socioeconomic", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Socioeconomic Characteristics"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Description")),
                                                     p("")
                                              ) ,
                                              column(8, 
                                                     h4(strong("Head of Household Demographics")),
                                                     selectInput("agedrop", "Select Varibiable:", width = "100%", choices = c(
                                                       "Age" = "age",
                                                       "Education" = "edu", 
                                                       "Poverty" = "pov", 
                                                       "Marital Status" = "mar",
                                                       "Household Size" = "hosi", 
                                                       "Number of Children in Household" = "chho"
                                                     ),
                                                     
                                                     ), 
                                                     withSpinner(plotOutput("ageplot", height = "500px", width = "100%")),
                                                     
                                              ),
                                              # column(12, 
                                              #     h4("References: "), 
                                              #   p(tags$small("[1] Groundwater: Groundwater sustainability. (2021). Retrieved July 27, 2021, from https://www.ngwa.org/what-is-groundwater/groundwater-issues/groundwater-sustainability")) ,
                                              #  p("", style = "padding-top:10px;")) 
                                     )), 
                            tabPanel("Livelihood", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Livelihood Behavior"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Description")),
                                                     p("")
                                              ) ,
                                              column(8, 
                                                     h4(strong("Livelihood - October 2018")),
                                                     selectInput("ocudrop", "Select Varibiable:", width = "100%", choices = c(
                                                       "Primary Occupation" = "pocu",
                                                       "Secondary Occupation" ="socu", 
                                                       "Job Duration" = "jodu",
                                                       "Agriculture Farming" = "agfa",
                                                       "Land Holding" = "laho",
                                                       "Crops" = "cro",
                                                       "Household Assets" = "hoas",
                                                       "Land Fallow" = "lafa"
                                                     ),
                                                     ), 
                                                     withSpinner(plotOutput("ocuplot", height = "500px")),
                                                     
                                              )
                                              # column(12, 
                                              #       h4("References: "), 
                                              #       p(tags$small("[1] Groundwater: Groundwater sustainability. (2021). Retrieved July 27, 2021, from https://www.ngwa.org/what-is-groundwater/groundwater-issues/groundwater-sustainability")) ,
                                              #      p("", style = "padding-top:10px;")) 
                                     )), 
                            
                            tabPanel("Financial", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Financial Practices"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Analysis")),
                                                     p("")
                                              ) ,
                                              column(8, 
                                                     h4(strong("Financial - October 2018")),
                                                     selectInput("findrop", "Select Varibiable:", width = "100%", choices = c(
                                                       "Household Business" = "hobu",
                                                       "Salary" = "sal",
                                                       "Income/Remmitances" = "inc",
                                                       "Savings" = "sav"
                                                     ),
                                                     ), 
                                                     withSpinner(plotOutput("finplot", height = "500px")),
                                                     
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
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Expenditure"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Overview")),
                                                     p("We present average weekly expenditure from Nov 2018 - Oct 2019 to examine the spending behaviors of households in the region. 
                                                       This will provide information on the changing nature of spending in the Sundarbans region due to events such as festivals, 
                                                       harvest seasons, and weather-related shocks.  "),
                                                     p("Expenditure is defined as total weekly consumption (e.g., food) and non-consumption (e.g., rent) items. 
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
                                         
                                         
                                       ),
                                       
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Plot",plotOutput("exp")),
                                           tabPanel("Table", DT::DTOutput("exp_table"))
                                         )
                                       ),
                                       
                                     )
                                     
                                     
                            ), 
                            
                            tabPanel("Income",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Income"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Overview")),
                                                     p("We also report average weekly household income for households across villages over 52 weeks. 
                                                       There is a significant increase in households’ income across most villages in late March. This increase coincides 
                                                       with the largest harvest for farmers in the region. We will investigate the different variations (spikes and dips) 
                                                       to determine the correlation between environmental shocks or unexpected household incidents."),
                                                     br("")
                                                     
                                              )),
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         #tags$h2("Select/Deselect all"),
                                         pickerInput("village_inc", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         
                                       ),
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Plot",plotOutput("inc")),
                                           tabPanel("Table", DT::DTOutput("inc_table"))
                                         )
                                       ),
                                     ),
                            ),
                            
                            tabPanel("Consumption",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Consumption"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Overview")),
                                                     p(""),
                                                     br("")
                                                     
                                              )),
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         pickerInput("village_cs_food", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
                                                     multiple = T, options = list(`actions-box` = T)),
                                         varSelectInput("food_group", "Select Food Group:", avg_cs_food[,-(1:2)])
                                         
                                       ),
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Plot", plotOutput("food_plot")),
                                           tabPanel("Staple Items",plotOutput("cs_staple")),
                                           tabPanel("Meat", plotOutput("cs_meat")),
                                           tabPanel("Other", plotOutput("cs_other")),
                                         )
                                       ),
                                       
                                       
                                     ),
                                     fluidRow(style = "margin: 6px;",
                                              h4(strong("Non-food consumption"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Overview")),
                                                     p("Expedniture on non food consumption items"),
                                                     br("")
                                                     
                                              )),
                                    
                            ),
                            
                            tabPanel("Borrowing",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Borrowing"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Overview")),
                                                     p("Households borrow"),
                                                     br("")
                                                     
                                                     
                                              ) ),
                                  
                                       # Show a plot of the generated plot
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Purpose", 
                                                    selectInput("purpdrop", "Select Varibiable:", width = "100%", choices = c(
                                                      "All Villages" = "alvi",
                                                      "Amrabati" = "ampu"
                                                    ),
                                                    ),
                                                    withSpinner(plotOutput("purpplot", height = "500px")),
                                           ),
                                           tabPanel("Amount",plotOutput("bor"),
                                           sidebarPanel(
                                             pickerInput("village_bramt", "Select Village:", choices = village_vector, 
                                                         selected = village_vector, 
                                                         multiple = T, options = list(`actions-box` = T))), 
                                                ),
                                           tabPanel("Count",plotOutput("borr"),
                                             sidebarPanel(
                                               pickerInput("village_borr", "Select Village:", choices = village_vector, 
                                                           selected = village_vector,
                                                           multiple = T, options = list(`actions-box` = T))),
                                           ),
                                           tabPanel("Purpose"
                                           )
                                                      
                                     
                            ))),
                                     
                              
                            
                            tabPanel("Remittances", value = "",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Remittances"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(12,h4(strong("Overview")),
                                                     p("In recent years, households have become more reliant on remittances as a significant source of income. 
                                                       As such, we examine temporal changes in remittances between October 2018 and November 2019. There is a substantial increase 
                                                       In late March (what year). This increase continues to occur frequently throughout the data period. Notably, 
                                                       the Sundarbans region was affected by three severe cyclones during this period: Fani, Category 4 (April – May 2019), 
                                                       and Category 1, Bulbul and Matmo (October – November 2019). The Sundarbans also could have been negatively impacted by two 
                                                       cyclones that hit the Arabian Sea during this period: Vayu (Category 1, June 8-18) and Hikaa (Category 1, September 20-26). 
                                                       It is possible households are using remittances to cope with these cyclones and weather-related shocks."),
                                                     br("")
                                                     
                                                     
                                              ) ),
                                     # Sidebar with a select input for village
                                     sidebarLayout(
                                       sidebarPanel(
                                         #tags$h2("Select/Deselect all"),
                                         pickerInput("village_rmt", "Select Village:", choices = village_vector, 
                                                     selected = village_vector,
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
                                     fluidRow(style = "margin: 6px;",
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
                                     
                                     fluidRow(style = "margin: 6px;",
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
                                            a(href = 'https://www.linkedin.com/in/ryan-jacobs-bb5727174/', 'Taj Cole', target = '_blank'), "(Virginia Tech, Undergraduate in Environmental Economics, Management, and Policy, and Minoring in Industrial Design).",
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
  
  
 #borrowing tab-------------------------
  #borrowing amount

  filtered_bramt <- reactive({
    bramt %>%
      filter(village %in% input$village_bramt)
  })
  # Plot
  output$bor <- renderPlot({
    ggplot(filtered_bramt(), aes(x=week_num, y=br_amt, color = village, na.rm=TRUE)) +
      geom_line() +
      labs(title ="Amount Borrowed by Village") + 
      xlab("Date") +
      ylab("Amount Borrowed (INR)")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40) +
      scale_color_viridis_d() +
      theme(legend.position = "none") 
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
             labs(title = "Number of Households Borrowing (Cash or in Kind)") + 
             xlab("Date") +
             ylab("Number of HH")+
             scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40) +
             scale_color_viridis_d() +
             theme(legend.position = "none")
  })  

  #borrowing purpose ----------------------

  
  
  
  
  #sociodemo tabset -----------------------------------------------------
  ageVar <- reactive({
    input$agedrop
  })
  
  output$ageplot <- renderPlot({
    if (ageVar() == "age") {
      
      fplot <-  ggplot(by_villagemore, aes(x = village, y = head_age, fill = village, width=0.5, srt = 45)) +
        geom_col(width = "5") +
        ylab("Age") + 
        xlab("")+
        ggtitle("Household Heads Average Age") +
        labs(subtitle = "by Village") +
        theme(axis.line = element_line(size = 3, colour = "grey80")) +
        theme(legend.position = "none") +
        rotate_x_text(angle = 33, size = rel(1)) + scale_fill_viridis_d()
      fplot
    }
    else if (ageVar() == "edu") {
      splot <- ggplot(by_villagemore, aes(x = "", y= head_edu, fill = village)) +
        geom_bar(width = 1, stat = "identity") +
        facet_wrap(~village, ncol = 5) +
        geom_text(aes(label = sub), position = position_stack(vjust=1.1)) +
        labs(title = "Mean Years of Education for Head of Households", x = NULL, y = "Years of Education") +
        theme(legend.position="none") + scale_fill_viridis_d()
      splot
    }
    else if (ageVar() == "pov") {
      village_pl_count_plot <- ggplot(dat_pl, aes(x= villages_2, y = values_pl, fill = Key)) + 
        geom_col(position = 'stack') + 
        labs( x= "", y = "Total Number of Households") + 
        theme_classic() + 
        ggtitle("Households That Live Below the Poverty Line") +
        coord_flip()+
        geom_text(aes(label = prop_pl_values), size = 2.5, nudge_y = -1) + scale_fill_viridis_d()
      village_pl_count_plot
    }
    else if (ageVar() == "mar") {
      marplot <- ggplot(countmar, aes(x = head_married, y = n, fill = Gender)) +
        geom_col() +
        labs(title = "Household Heads' Marital Status", x = "Not Married                       Married", y = "Number of Household Heads") +
        scale_x_discrete() + theme_classic() + scale_fill_viridis_d()
      marplot
    }
    else if (ageVar() == "hosi") {
      hh_size_plot <- ggplot(median_hhsize, aes(x = village, y = median, fill = village)) +
        geom_col() +
        labs( x = "", y = "Median Household Size")+
        coord_flip()+
        ggtitle("Household Size by Village") +
        theme(legend.position="none") +
        theme_classic() + scale_fill_viridis_d()
      hh_size_plot
    }
    else if (ageVar() == "chho") {
      chhoplot <- ggplot(avg_children, aes(village, avg_children, fill = village)) + geom_col() + labs(x = "", y = "Number of Children" ,title = "Number of Children per Household", fill = "Village") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + scale_fill_viridis_d()
      chhoplot
    }
  })
  
  
  #livelihood tabset -----------------------------------------------------
  ocuVar <- reactive({
    input$ocudrop
  })
  
  output$ocuplot <- renderPlot({
    if (ocuVar() == "pocu") {
      pocuplot <- ggplot(countv, aes(x = job, y = n, fill = village)) +
        geom_col() +
        scale_x_discrete(limits = factor(1:16), labels = c("1" = "Agricultural wage worker","2" =  "Livestock worker", "3" = "Farmer", "4" = "Casual labor","5" =  "Construction/brick labor","6" =  "Gleaning/foraging","7" =  "Fisherman","8" =  "Fishery worker", "9" = "Factory worker" , "10" = "Household help" ,"11" =  "Transport related work","12" =  "Own business", "13" = "Service Work (NGO, gov,etc.)", "14" = "NREGA","15" =  "Housewife","16" =  "Other")) +
        coord_flip() +
        theme_minimal () +
        labs(title = "Primary Occupation of Household Heads", x = "", y = "") + scale_fill_viridis_d()
      pocuplot
    } 
    else if (ocuVar() == "socu") {
      socplot <- ggplot(scountv, aes(x = job, y = n, fill = village)) +
        geom_col() +
        scale_x_discrete(limits = factor(1:16), labels = c("1" = "Agricultural wage worker","2" =  "Livestock worker", "3" = "Farmer", "4" = "Casual labor","5" =  "Construction/brick labor","6" =  "Gleaning/foraging","7" =  "Fisherman","8" =  "Fishery worker", "9" = "Factory worker" , "10" = "Household help" ,"11" =  "Transport related work","12" =  "Own business", "13" = "Service Work (NGO, gov,etc.)", "14" = "NREGA","15" =  "Housewife","16" =  "Other")) +
        coord_flip() +
        theme_minimal () +
        labs(title = "Secondary Occupation of Household Heads", x = "", y = "") + scale_fill_viridis_d()
      socplot
    }
    else if (ocuVar() == "agfa") {
      agfaplot <- ggplot(grouped, aes(village,prop_farm)) + geom_col(fill = "navy blue") + labs(x = "", y = "Proportion", title = "Proportion of Households Involved in Agricultural Farming") + coord_flip() + theme_classic()
      agfaplot
    }
    else if (ocuVar() == "laho") {
      mean_land_plot <- ggplot(land_stats, aes(x = villages, y = mean_land_value, fill = villages)) +
        geom_col(fill = plasma(10, alpha = 1, begin = 0, end = 1, direction = 1)) +
        coord_flip() +
        ggtitle("Average Amount of Land Owned in Each Village") +
        labs(x = "", y = "Land Owned [Kathas]") + scale_fill_viridis_d()
      mean_land_plot
    }
    else if (ocuVar() == "cro") {
      croplot <- ggplot(grouped, aes(village,prop_farm)) + geom_col(fill = "navy blue") + labs(x = "", y = "Proportion", title = "Proportion of Households that cultivated crops") + coord_flip() + theme_classic()
      croplot
    }
    else if (ocuVar() == "hoas") {
      assetplot <- ggplot(assets_long, aes(property, measurement, fill = property)) + geom_col() + 
        labs(x = "", y = "Proportion" ,title = "Proportion of Households Owning Assets", fill  = "Asset") + 
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
        geom_text(aes(label = measurement), size = 3, nudge_y = .05) + coord_flip() + scale_fill_viridis_d()
      assetplot
    }
    else if (ocuVar() == "lafa") {
      land_fallow_plot <- ggplot(land_fallow, aes(x = village, y = sum, fill = village)) +
        geom_col(fill = plasma(10, alpha = 1, begin = 0, end = 1, direction = 1))+
        theme_classic() +
        labs(x = "", y = "Total Land fallowed")+
        ggtitle("Total Land Followed by Village") +
        coord_flip() + scale_fill_viridis_d()
      land_fallow_plot
    }
    else if (ocuVar() == "jodu") {
      job_duration_plot <- ggplot(job_duration_summary, aes(x = villages, y = job_duration_avg, fill = villages)) +
        geom_col( fill = plasma(10, alpha = 1, begin = 0, end = 1, direction = 1)) + 
        coord_flip()+
        labs(x= "", y = "Average Job Duration [Months]")+
        ggtitle("Average Job Duration for the Head of the Household") +
        theme_classic() + scale_fill_viridis_d()
      job_duration_plot
    }
    
  })
  
  #financial  tabset -----------------------------------------------------
  finVar <- reactive({
    input$findrop
  })
  
  output$finplot <- renderPlot({
    if (finVar() == "hobu") {
      village_bus_count_plot <- ggplot(dat_bus, aes(x= villages_2, y = values_bus, fill = Key)) + 
        geom_col(position = 'stack') + 
        labs( x= "", y = "Total Number of Households") + 
        theme_classic() + 
        ggtitle("Households That Own a Business") +
        coord_flip()+
        geom_text(aes(label = prop_bus_values), size = 2.5, nudge_y = -1) + scale_fill_viridis_d()
      village_bus_count_plot
    }
    else if (finVar() == "inc") {
      figure_inc_spending <- ggplot(baseline.summary, aes(rmt_total, full_inc, color= village))+geom_point(data=baseline.summary, shape=17, size=3) +labs(x="Total mean weekly remittances", y="Total mean weekly income", color="Villages") + ggtitle("Average total income vs average total remittances in Baseline week") + scale_color_viridis_d()
      p<-figure_inc_spending +coord_flip() #+ scale_x_continuous(trans='log2') 
      p
    }
    else if (finVar() == "sal") {
      salplot <- ggplot(m_salary, aes(village, avg_salary, fill = village)) + geom_col() + labs(x = "", y = "INR" ,title = "Average Monthly Salary per Household", fill = "Village") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + scale_color_viridis_d()
      salplot
    }
    else if (finVar() == "sav") {
      savplot <- ggplot(nbsavcount, aes(x = nb_put_saving, y = n, fill = "red")) +
        geom_col() +
        labs(title="Number of Times Households Saved Money in a Year",
             x = "Numer of Times Able to Save", y = "Number of Household Heads") +
        theme_classic() +
        theme(legend.position="none")
      savplot
    }
    
    
  })
  
  
  
  # rmt plot output
  # Filter by inputt
  filtered_rmt <- reactive({
    rmt_data_mean_weeks %>%
      filter(Villages %in% input$village_rmt)
  })
  # Plot
  output$rmt <- renderPlot({
    ggplot(filtered_rmt(), aes(x = weeks
                               , y = mean_rmt_per_week, color = Villages)) + 
      geom_line() +
      theme_classic() +
      labs(x = "Date", y = "Average Remittance Income (INR)", caption = "Mean: 2622.5   Median: 1731.66") +
      ggtitle("Average Weekly Household Remittance Income by Village")+ #(11/16/18 - 10/31/19)
      #scale_color_brewer(palette = "Spectral")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40) + 
      scale_color_viridis_d()+
      theme(plot.caption = element_text(size = 12))
    
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
      labs(title="Average Weekly Household Expenditure by Village",
           
           x="Date", y="Average Weekly Expenditure (INR)", caption = "Mean: 1982.77   Median: 1832.1") +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40) + 
      scale_color_viridis_d()+
      theme_classic()+
      theme(plot.caption = element_text(size = 12))
    
    
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
    ggplot(filtered_inc(), aes(date, avg_inc, color = village)) + 
      geom_line() + 
      labs(x = "", y = "Income (INR)", title = "Average Weekly Household Income by Village", color = "Village",
           caption = "Mean: 1395.61   Median: 1341.82") + 
      scale_color_viridis_d()+
      theme_classic()+
      theme(plot.caption = element_text(size = 12))
    
  })
  #Render inc table
  output$inc_table <- DT::renderDT({
    avg_inc_table
  })
  

 # Consumption ----------------------- 


  output$cs_item <- renderPlot({
    ggplot(filtered_cs_avg_items(), aes(x = week, y = avg_item, color = village))+
      geom_line() +
      theme_classic()+
      ggtitle("Average Consumption Items Bought a Week")+
      labs(x = "", y = "No. of Consumption Items Bought", color = "Villages")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)
  })
  
  # Filtered consumption by group
  
  filtered_cs_food <- reactive({
    avg_cs_food %>% 
      filter(village %in% input$village_cs_food)
  })
  
  # Consumption by food group plots
  
  output$food_plot <- renderPlot({
    ggplot(filtered_cs_food(), aes(x = week, y = !!input$food_group, color = village))+
      geom_line()
  
  })
  
  # Staple items plot
  
  output$cs_staple <- renderPlot({
    ggplot(filtered_cs_food(), aes(x = week, y = `Average Staple Expenditure`, color = village)) +
      geom_line()+
      theme_classic()+
      ggtitle("Average Weekly Expenditure on Staple Items ")+
      labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 463.87  Median: 431.20",
           subtitle = "(Rice/Grains, Flour, Vegetables, Fruits, Tubers, Beans and Spices)")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)+
      theme(plot.caption = element_text(size = 10))
  })
  
  # Meat plot
  
  output$cs_meat <- renderPlot({
    ggplot(filtered_cs_food(), aes(x = week, y = `Average Meat Expenditure`, color = village))+
      geom_line()+
      theme_classic()+
      ggtitle("Average Weekly Expenditure on Meat")+
      labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 158.97  Median: 431.20",
           subtitle = "(Red Meat, Fish, and Poultry)")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)+
      theme(plot.caption = element_text(size = 10))
  })
  
  # Other cs plot
  
  output$cs_other <- renderPlot({
    ggplot(filtered_cs_food(), aes(x = week, y = `Average Other Expenditure`, color = village))+
      geom_line() +
      theme_classic()+
      ggtitle("Average Weekly Expenditure on 'Other' Items")+
      labs(x = "", y = "Average Weekly Expenditure (INR)", color = "Villages", caption = "Mean: 113.75  Median: 111.94",
           subtitle = "(Eggs, Dairy, Packaged Foods, Tea, and Other Food Items)")+
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)+
      theme(plot.caption = element_text(size = 10))
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
      labs(x = "", y = "" ,title = "Type of cope after 2009 shocks", fill = "Village") + scale_fill_viridis_d() + 
      theme(axis.text = element_text(size = 5)) +
      scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), labels = str_wrap(cope_labels, width = 30), limits = 0:20) + 
      coord_flip() 
  })
  
  #Cope_plot_2009 filter 
  
  filtered_relocation_yn <- reactive({
    shock_relocation %>%
      filter(village %in% input$village_selecter)
  })
  
  output$relocation_2009_yn <- renderPlot({
    ggplot(filtered_relocation_yn(), aes(shk_2009_reloc_yn, fill = village)) + geom_bar() + 
      labs(x = "", y = "No. of Households" ,title = "Relocation Status after Shock", fill = "Village") + 
      scale_x_discrete(breaks = c(0,1,2), labels = str_wrap(relocation_labels, width = 30), limits = 0:2) + 
      scale_fill_viridis_d()
    
  })
  
  
  
  filtered_relocation <- reactive({
    shock_relocation_where %>%
      filter(village %in% input$village_selecter)
    
  })
  
  
  output$relocation_2009 <- renderPlot({
    
    ggplot(filtered_relocation(), aes(shk_2009_reloc1, fill = village)) + 
      geom_bar() + labs(x = "", y = "No. of Households" ,title = "Relocation Areas", fill = "Village") + 
      scale_x_discrete(breaks = c(1,2,3,4,5,6), labels = str_wrap(relocation_where_labels, width = 20), limits = 1:6) + 
      scale_fill_viridis_d() + coord_flip() +  theme(axis.text = element_text(size = 8))
    
    
  })
}

shinyApp(ui = ui, server = server)





