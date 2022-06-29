# Load packages
library(dplyr)
library(ggplot2)

# Baseline data
baseline <- livdiv %>%
  slice(1:306)

# Select variables of interest
selected_bl <- baseline %>% select(hh_ownfarm_prop, hh_ownfarm, nb_hhmem, land_own, business_yn, below_204inr_bl, village, today, hhid)
selected_bl

# Selecting only the below poverty line and own a business count from each of the 10 villgaes:
#Villages : Amrabati, Beguakhali, Bijoynagar, Haridaskati Samsernagar, Lakshmi Janardanpur, Pargumti, Purba Dwarokapur, Sagar, Shibpur

## Amrabati
Amrabati <- selected_bl %>% 
  filter(village == "Amrabati") %>% 
  select(business_yn, below_204inr_bl)
# Amrabati business count
Amrabati_bus_count <- Amrabati %>% 
  count(business_yn)
# Amrabati pl count
Amrabati_pl_count <- Amrabati %>% 
  count(below_204inr_bl)

## Beguakhali 
Beguakhali <- selected_bl %>% 
  filter(village == "Beguakhali") %>% 
  select(business_yn, below_204inr_bl)
#Beguakhali business count
Beguakhali_bus_count <-  Beguakhali %>% 
  count(business_yn)
#Beguakhali pl count
Beguakhali_pl_count <- Beguakhali %>% 
  count(below_204inr_bl)

## Bijoynagar 
Bijoynagar <- selected_bl %>% 
  filter(village == "Bijoynagar") %>% 
  select(business_yn, below_204inr_bl)
#Bijoynagar business count
Bijoynagar_bus_count <- Bijoynagar %>% 
  count(business_yn)
# Bijoynagar pl count 
Bijoynagar_pl_count <- Bijoynagar %>% 
  count(below_204inr_bl)

# Birajnagar

Birajnagar <- selected_bl %>% 
  filter(village == "Birajnagar") %>% 
  select(business_yn, below_204inr_bl)
# Birajnagar business count
Birajnagar_bus_count <- Birajnagar %>% 
  count(business_yn)
# Birajnagar pl count
Birajnagar_pl_count <- Birajnagar %>% 
  count(below_204inr_bl)

## Haridaskati Samsernagar 
Haridaskati_Samsernagar <- selected_bl %>% 
  filter(village == "Haridaskati Samsernagar") %>% 
  select(business_yn, below_204inr_bl)
# Haridaskati Samsernagar business count
Hsam_bus_count <-  Haridaskati_Samsernagar %>% 
  count(business_yn)
# Haridaskati Samsernagar pl count 
Hsam_pl_count <- Haridaskati_Samsernagar %>% 
  count(below_204inr_bl)

## Lakshmi Janardanpur
Lakshmi_Janardanpur <- selected_bl %>% 
  filter(village == "Lakshmi Janardanpur") %>% 
  select(business_yn, below_204inr_bl)
# Lakshmi Janardanpur business count
Ljan_bus_count <- Lakshmi_Janardanpur %>% 
  count(business_yn)
# Lakshmi Janardanpur pl count
Ljan_pl_count <- Lakshmi_Janardanpur %>% 
  count(below_204inr_bl)

## Pargumti
Pargumti <- selected_bl %>% 
  filter(village == "Pargumti") %>% 
  select(business_yn, below_204inr_bl)
# Pargumti business count
Pargumti_bus_count <-  Pargumti %>% 
  count(business_yn)
# Pargumti pl count
Pargumti_pl_count <- Pargumti %>% 
  count(below_204inr_bl)

## Purba Dwarokapur
Purba_Dwarokapur <- selected_bl %>% 
  filter(village == "Purba Dwarokapur") %>% 
  select(business_yn, below_204inr_bl)
# Purba Dwarokapur business count
Pdwar_bus_count <-  Purba_Dwarokapur %>% 
  count(business_yn)
# Purba Dwarokapur pl count
Pdwar_pl_count <- Purba_Dwarokapur %>% 
  count(below_204inr_bl)

## Sagar
Sagar <- selected_bl %>% 
  filter(village == "Sagar") %>% 
  select(business_yn, below_204inr_bl)
# Sagar business count 
Sagar_bus_count <- Sagar %>% 
  count(business_yn)
# Sagar pl count
Sagar_pl_count <- Sagar %>% 
  count(below_204inr_bl)

## Shibpur
Shibpur <- selected_bl %>% 
  filter(village == "Shibpur") %>% 
  select(business_yn, below_204inr_bl)
# Shibpur business count
Shibpur_bus_count <-Shibpur %>% 
  count(business_yn)
# Shibpur pl count
Shibpur_pl_count <- Shibpur %>% 
  count(below_204inr_bl)








