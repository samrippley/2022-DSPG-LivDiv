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

#load("~/Virginia Tech/Internship 2022/2022-DSPG-LivDiv-/data/livdivdata.RData")

baseline <- livdiv %>%
  slice(1:307,)

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
  ggtitle("Average Remittance Income Per Village") + #(11/16/18 - 10/31/19)
  #scale_color_brewer(palette = "Spectral")+
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)
#annotate(geom = "text", aes(x = unlist(months)))
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
  scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)

#Shocks Data ------------------------------------------------------------------- 
## Frequency of each shock (Total baseline)
shocks <- baseline %>% select(village,shk1,shk2,shk3,shk4,shk5,shk6,shk7)
shocks <- data.frame(y=unlist(shocks))
colnames(shocks) <- c('shock_nmb')
shock_labels <- c('None', 'Crop Loss', 'Loss of vegetation', 'Damage caused by saline water encroachment','Forced to move due to Flooding',
                  'Loss of agricultural land by river erosion','Loss of home by river erosion/cyclone', 'Loss of livestock',
                  'Loss of business/shop/etc. due to rising water levels', 'Unexpected death or health consequence in Household', 'Other')

shocks_all <- ggplot(shocks, aes(shock_nmb)) + geom_bar() + labs(x = "", y = "Occurances" ,title = "Frequency of Each Shock") + 
  theme(axis.text = element_text(size = 7)) + 
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10),labels = str_wrap(shock_labels, width = 25) ,limits = 0:10) + coord_flip()

## Average Shocks by Village
shocks2 <- baseline %>% select(village, shk_count) %>% 
  group_by(village) %>% summarize(avg_count = sum(shk_count, na.rm = TRUE)/n())

shocks_village <- ggplot(shocks2, aes(village, avg_count, fill = village)) + geom_col() + 
  labs(x = "", y = "Average number of Shocks" ,title = "Average Shocks by Village", fill = "Village") + 
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
  labs(x = "", y = "Count" ,title = "Total Shocks by Year") + theme(axis.ticks.x=element_blank(), legend.position="none") + 
  scale_fill_viridis_d() 

## Frequency of each shocks in 2009

shocks_2009 <- baseline %>% select(shk_2009_type1, shk_2009_type2, shk_2009_type3, shk_2009_type4, shk_2009_type5, shk_2009_type6)
shock_labels_2009 <- c('None', 'Crop Loss', 'Loss of vegetation', 'Damage caused by saline water encroachment',
                       'Forced to move due to Flooding', 'Loss of agricultural land by river erosion',
                       'Loss of home by river erosion/cyclone', 'Loss of livestock', 'Loss of business/shop/etc. due to rising water levels', 
                       'Unexpected death or health consequence in Household', 'Other')
shocks_2009 <- data.frame(y=unlist(shocks_2009))
colnames(shocks_2009) <- c('shk')

shocks_plot_2009 <- ggplot(shocks_2009, aes(shk)) + geom_histogram() + 
  labs(x = "", y = "Occurances" ,title = "Frequency of Each Shock in 2009") + theme(axis.text = element_text(size = 7)) + 
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10),labels = str_wrap(shock_labels_2009, width = 25) ,limits = 0:10) + coord_flip()

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

cope_2009_plot <- ggplot(shocks_cope, aes(shk_2009_cope, fill = village)) + geom_histogram() + 
  labs(x = "", y = "" ,title = "Type of cope after 2009 shocks", fill = "Village") + scale_fill_viridis_d() + 
  theme(axis.text = element_text(size = 5)) +
  scale_x_discrete(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), labels = str_wrap(cope_labels, width = 30), limits = 0:20) + 
  coord_flip() 

## Relocation Status after 2009 Shock

shock_relocation <- baseline %>% select(village, shk_2009_reloc_yn)

relocation_labels <- c("No", "Yes, for under a month", "Yes, for over a month")

shock_relocation_2009_yn <- ggplot(shock_relocation, aes(shk_2009_reloc_yn, fill = village)) + geom_histogram() + 
  labs(x = "", y = "Count" ,title = "Relocation Status after Shock", fill = "Village") + 
  scale_x_discrete(breaks = c(0,1,2), labels = str_wrap(relocation_labels, width = 30), limits = 0:2) + scale_fill_viridis_d()

## Where they Relocated after 2009 Shock

shock_relocation_where <- baseline %>% select(village, shk_2009_reloc1)

relocation_where_labels <- c("Within same village","Other village in Sundarbans","Village outside Sundarbans, within West Bengal", 
                             "Kolkata", "Other Urban area outside Sundarbans, within West Bengal",
                             "Urban area outside West Bengal","Rural area outside West Bengal")

shock_relocation_2009 <- ggplot(shock_relocation_where, aes(shk_2009_reloc1, fill = village)) + geom_histogram() + 
  labs(x = "", y = "Count" ,title = "Relocation Status after Shock", fill = "Village") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7), labels = str_wrap(relocation_where_labels, width = 20), limits = 1:7) + 
  scale_fill_viridis_d() + coord_flip() + theme(plot.title = element_text(hjust = 2))

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
                 navbarMenu("Demographics" , 
                            tabPanel("Static", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Static Demographics"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Education")),
                                                     p("These are demographics"),
                                              ) ,
                                              column(8, 
                                                     h4(strong("Demographics")),
                                                     selectInput("agedrop", "Select Varibiable:", width = "100%", choices = c(
                                                       "Age" = "age",
                                                       "Education" = "edu", 
                                                       "Primary Occupation" = "pocu",
                                                       "Secondary Occupation" ="socu",
                                                       "Poverty" = "pov", 
                                                       "Marital Status" = "mar"),
                                                    ), 
                                                     withSpinner(plotOutput("ageplot", height = "500px")),
                                                     
                                              ),
                                              column(12, 
                                                     h4("References: "), 
                                                     p(tags$small("[1] Groundwater: Groundwater sustainability. (2021). Retrieved July 27, 2021, from https://www.ngwa.org/what-is-groundwater/groundwater-issues/groundwater-sustainability")) ,
                                                     p("", style = "padding-top:10px;")) 
                                     )), 
                            tabPanel("Dynamic", 
                                     style = "margin: 6px;",
                                     h1(strong("Dynamic"), align = "center"),
                                     p("", style = "padding-top:10px;"), 
                                     column(4, 
                                            h4(strong("Dynamic")), 
                                            p("Differences by Village."), 
                                            p("Location is important. Need water to fish. Transportation, etc. Graphed differences by village.")),
                                     column(8,
                                                    h4(strong("Household Demographic Characteristics by Village")),
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
               
                navbarMenu("High Frequency Data" , 
                           tabPanel("Income",
                                    fluidRow(style = "margin: 6px;",
                                             h1(strong("Shocking"), align = "center"),
                                             p("", style = "padding-top:10px;")
                                             
                                    ) 
                           ),            
                           
                           tabPanel("Expenditure",
                                    fluidRow(style = "margin: 6px;",
                                             h1(strong("Shocking"), align = "center"),
                                             p("", style = "padding-top:10px;")
                                             
                                    ),
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
                                          tabPanel("Plot",plotOutput("exp"))
                                        )
                                      ),
                                      
                                    )
                                    
                                    
                           ), 
                           
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
                           
                           
                           
                ),
              
               
                ## Shocks Tab --------------------------------------------
                navbarMenu("Shocks" , 
                tabPanel("Shocks in the Sundarbans", value = "",
                         fluidRow(style = "margin: 6px;",
                                  h1(strong("Shocks"), align = "center"),
                                  p("", style = "padding-top:10px;"),
                                  column(12,h4(strong("Overview")),
                                         p("Over time"),
                                         br("")
                                         
                                         
                                  ) ),
                           # Show a plot of the generated plot
                               tabPanel("All The Shocks", plotOutput("shocks_all")),
                               tabPanel("Shocks by Village", plotOutput("shocks_village")),
                               tabPanel("Shocks by the Year", plotOutput("shocks_by_year")),
                               tabPanel("Shocks in 2009", plotOutput("shocks_plot_2009")),
                               tabPanel("Copes in 2009", plotOutput("cope_2009_plot")),
                               tabPanel("Relocations after Shock in 2009", plotOutput("shock_relocation_2009_yn")),
                               tabPanel("Where Relocation took place", plotOutput("shock_relocation_2009")),
                             
                           
                           
                         
                ),  
                
                
                
                tabPanel("Dynamic Plot",
                         fluidRow(style = "margin: 6px;",
                                  h1(strong("Dynamic Shocks"), align = "center"),
                                  p("", style = "padding-top:10px;")
                                  
                         ) 
                ), 

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
server <- function(input, output, session) {
# Run JavaScript Code
 runjs(jscode)
                  

  #age tabset -----------------------------------------------------
  ageVar <- reactive({
    input$agedrop
  })
  
  output$ageplot <- renderPlot({
    if (ageVar() == "age") {
      
    fplot <- ggplot(baseline, aes(x = head_age)) +
      geom_histogram(fill = "cornflowerblue", 
                     color = "white", bins = 20
      ) + 
      labs(title="Age of Household Heads",
           y = "Number of Household Heads") +
      theme_classic() +
      scale_x_continuous(breaks = c(20, 30, 40, 50, 60, 70, 80), name="Age", limits=c(20, 80))
    fplot
    }
    else if (ageVar() == "edu") {
    splot <- ggplot(by_villagemore, aes(x = "", y= head_edu, fill = village)) +
      geom_bar(width = 1, stat = "identity") +
      facet_wrap(~village, ncol = 5) +
      geom_text(aes(label = sub), position = position_stack(vjust=1.1)) +
      labs(title = "Mean Years of Education for Head of Households", x = NULL, y = "Years of Education") +
      theme(legend.position="none") 
    splot
    }
  
    else if (ageVar() == "pocu") {
     pocplot <- ggplot(countv, aes(x = job, y = n, fill = village)) +
        geom_col() +
        scale_x_discrete(limits = factor(1:16), labels = c("1" = "Agricultural wage worker","2" =  "Livestock worker", "3" = "Farmer", "4" = "Casual labor","5" =  "Construction/brick labor","6" =  "Gleaning/foraging","7" =  "Fisherman","8" =  "Fishery worker", "9" = "Factory worker" , "10" = "Household help" ,"11" =  "Transport related work","12" =  "Own business", "13" = "Service Work (NGO, gov,etc.)", "14" = "NREGA","15" =  "Housewife","16" =  "Other")) +
        coord_flip() +
        theme_minimal () +
        labs(title = "Primary Occupation of Household Heads", x = "", y = "") +
        scale_fill_brewer(palette="Spectral")
     pocplot
    }
    
    else if (ageVar() == "socu") {
      socplot <- ggplot(scountv, aes(x = job, y = n, fill = village)) +
        geom_col() +
        scale_x_discrete(limits = factor(1:16), labels = c("1" = "Agricultural wage worker","2" =  "Livestock worker", "3" = "Farmer", "4" = "Casual labor","5" =  "Construction/brick labor","6" =  "Gleaning/foraging","7" =  "Fisherman","8" =  "Fishery worker", "9" = "Factory worker" , "10" = "Household help" ,"11" =  "Transport related work","12" =  "Own business", "13" = "Service Work (NGO, gov,etc.)", "14" = "NREGA","15" =  "Housewife","16" =  "Other")) +
        coord_flip() +
        theme_minimal () +
        labs(title = "Secondary Occupation of Household Heads", x = "", y = "") +
        scale_fill_brewer(palette="Spectral")
      socplot
    }
    
    else if (ageVar() == "pov") {
      village_pl_count_plot <- ggplot(dat_pl, aes(x= villages_2, y = values_pl, fill = Key)) + 
        geom_col(position = 'stack') + 
        labs( x= "", y = "Total Number of Households") + 
        theme_classic() + 
        ggtitle("Households That Live Below the Poverty Line") +
        coord_flip()+
        geom_text(aes(label = prop_pl_values), size = 2.5, nudge_y = -1)
      village_pl_count_plot
    }
 else if (ageVar() == "mar") {
      marplot <- ggplot(countmar, aes(x = head_married, y = n, fill = Gender)) +
        geom_col() +
        labs(title = "Household Heads' Marital Status", x = "Not Married                       Married", y = "Number of Household Heads") +
        scale_x_discrete() + theme_classic()
      marplot
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
  # exp plot output
  filtered_exp <- reactive({
    exbyvil %>% 
      filter(village %in% input$village_exp)
  })
  output$exp <- renderPlot({
    ggplot(filtered_exp(), aes(x=week_num, y=total_spending, color = village, na.rm=TRUE)) +
      geom_line() +
      labs(title="Average Weekly Expenditure by Village",
           x="Date", y="Average Weekly Expenditure (INR)") +
      scale_x_discrete(breaks = c(10,20,30,40), labels = c("January 2019", "April 2019", "July 2019", "October 2019"), limits = 10:40)
    
    
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

  


}

shinyApp(ui = ui, server = server)




