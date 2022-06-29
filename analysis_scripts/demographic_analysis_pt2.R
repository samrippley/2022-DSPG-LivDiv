

library(dplyr)
library(ggplot2)
library(plotrix)
# Set baseline and select variables
baseline <- livdiv %>%
  slice(1:306)
business_bl <- baseline %>% 
  select(hhid, village, business_yn, business_type, business_asset, business_mth, business_inc)

business_type_count <-business_bl %>% 
  count(business_type)

# Pie chart of business type
slices_bt <- c(2,2,1,1,1,11,2,12,1,2,1,1,1)
lbls_bt <- c("      Barber/Saloon", "    Clothier", "     Decorator", "Electronic Shop", "Fish Monger",
             "Grocery/Corner Shop", "Hotel/Guest House", "Petty Trader", "Poultry Seller",
             "          Seashell Seller", "Studio", "             Sugar Seller", "    Tea Supplier")
pct_bt <- round(slices_bt/sum(slices_bt)*100)
lbls_bt <- paste(lbls_bt, pct_bt) # add percents to labels
lbls_bt <- paste(lbls_bt,"%        ",sep="") # add % to labels
business_type_pie <- pie3D(slices_bt,labels = lbls_bt, col=rainbow(length(lbls_bt)),
                      main="Pie Chart of Type of Business (All Villages)", 
                      labelcex = 0.5)


# Average household size by village
hhsize <- baseline %>% 
  select(village, hhid, nb_hhmem) 


avg_hhsize <- hhsize %>% 
  group_by(village) %>% 
  summarise("Avg" = mean(nb_hhmem))
  
median_hhsize <- hhsize %>% 
  group_by(village) %>% 
  summarise("median" = median(nb_hhmem))

hh_size_plot <- ggplot(median_hhsize, aes(x = village, y = median, fill = village)) +
                         geom_col() +
  labs( x = "", y = "Median Household Size")+
  coord_flip()+
  theme_classic()
hh_size_plot


#Average business Income level 

# how much income does your business earn in a typical month
bus_inc <- business_bl %>% 
  select(village, business_inc) %>% 
  group_by(village) %>% 
  summarise("avg" = mean(na.omit(business_inc)))

avg_bus_inc <- ggplot(bus_inc, aes(x = village, y = avg, fill = village)) +
  geom_col()+
  theme_classic()+
  coord_flip()+
  labs(x = "", y = "Average Business Income [Rupee]") +
  ggtitle('Average Business Inocme in a Typical Month')
avg_bus_inc



# Land fallow

# fallow: (of farmland) plowed and harrowed but left unsown for a period in order to restore its fertility 
# as part of a crop rotation or to avoid surplus production
land_fallow <- baseline %>% 
  select(village, land_fallow)
land_fallow <- land_fallow %>% 
  group_by(village) %>% 
  summarise("avg" = mean(na.omit(land_fallow)), "sum" = sum(na.omit(land_fallow)))

land_fallow_plot <- ggplot(land_fallow, aes(x = village, y = sum, fill = village)) +
  geom_col()+
  theme_classic() +
  labs(x = "", y = "Total Land fallowed")+
  coord_flip()
land_fallow_plot


# Job Duration

job_duration <- baseline %>% 
  select(village, relationship1, relationship2, relationship3, relationship4,
         job1_duration1, job1_duration2, job1_duration3,
         job1_duration4, job1_duration5, job1_duration6, 
         job1_duration7, job1_duration9, job1_duration12)

# Average Job duration for the primary occupation of the head of household by village
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

job_duration_plot <- ggplot(job_duration_summary, aes(x = villages, y = job_duration_avg, fill = villages)) +
  geom_col()
job_duration_plot










