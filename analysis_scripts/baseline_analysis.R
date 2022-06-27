# Load packages
library(dplyr)
library(ggplot2)

#load liv div data

# Baseline data
#baseline <- livdiv %>%
 # filter(week_num == 0:4) %>%
 #arrange(date)

baseline <- livdiv %>%
  slice(1:306)


# Select variables of interest
#selected_bl <- baseline %>% select(hh_ownfarm_prop, hh_ownfarm, nb_hhmem, land_own, business_yn, below_204inr_bl, village, today, hhid)
#selected_bl

selected_bl <- baseline %>% select(hh_ownfarm_prop, hh_ownfarm, nb_hhmem, land_own, business_yn, below_204inr_bl, village, today, hhid)
selected_bl

# ----------------------------------------------------------

## Land owned
# Bar plot of Kathas owned 
own_land <- ggplot(selected_bl, aes(x = land_own)) + 
  geom_bar(position = position_dodge(2), color = "dark orange") +
  labs(x = "'Kathas' of Land Owned", y = " Number of Households") + 
  theme_classic() +
  facet_wrap(~village)
own_land
# Count of Kathas owned 
own_land_count <- selected_bl %>% 
  count(land_own)
own_land_count
# Avg, max, min
mean(na.omit(selected_bl$land_own))
max(na.omit(selected_bl$land_own))
min(na.omit(selected_bl$land_own))
# A katha is about 1,361 sq ft. For reference, 1 acre is about 32 kathas 
# Households in the baseline own an average of 48.36 kathas of land
# The max owned is 510 kathas (1 person in Bijoynagar) 
# The min owned is 1 kathas (4 people)
# 106 NAs were reported and removed from histogram
# (Only asked households that reported to own a farm)
# Most households do not own farm land, and if they do, its often not much.
# Most hosueholds provide labor to farm land, rather than owning their own farm land 

#----------------------------------------------------------

## Total count pie charts across all 10 villages

# Pie chart have Households that own a business (aggregate)
slices <- c(268, 38)
lbls <- c("No", "Yes")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
business_pie <- pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Households that own a Business (All 10 villages)")
# 88% of households across the 10 villages reported that they do not own a business
# 12% do own a business

#----------------------------------------------------------

#Pie chart of households below the poverty line (aggregate)
slices_pl <- c(202, 103)
lbls_pl <- c("No", "Yes")
pct_pl <- round(slices_pl/sum(slices_pl)*100)
lbls_pl <- paste(lbls_pl, pct_pl) # add percents to labels
lbls_pl <- paste(lbls_pl,"%",sep="") # ad % to labels
pie(slices_pl,labels = lbls_pl, col=rainbow(length(lbls_pl)),
    main="Pie Chart of Households that live below the poverty line (All 10 villages)")
# 34% of households, across all 10 villages, live below the poverty line
# (66% dont)
# Living below the poverty line means that you make below 204 rupee per week per person
# This pie chart shows that a sgnificant amount of the sunderbans region lives in financial poverty 

#----------------------------------------------------------

## Create new data frames with Village counts

# Business data frame and stacked bar plot
villages_2 <- c(rep("Amrabati", 2), rep("Beguakhali", 2), rep("Bijoynagar", 2), rep("Birajnagar", 2), rep("Haridaskati Samsernagar", 2), rep("Lakshmi Janardanpur",2), rep("Pargumti",2),rep("Purba Dwarokapur", 2), rep("Sagar", 2), rep("Shibpur",2))
Answer <- rep(c("No", "Yes"), 2)
values_bus <-c(26,2,27,3,48,2,24,2,27,3,25,3,24,4,21,7,21,7,25,3) 
dat_bus <-  data.frame(villages_2, Answer, values_bus)
village_bus_count_plot <- ggplot(dat_bus, aes(x= villages_2, y = values_bus, fill = Answer)) + 
  geom_col(position = 'stack') + 
  labs( x= "", y = "Total Number of Households") + 
  theme_classic() + 
  ggtitle("Count of Households That Own a Business") +
  coord_flip()
village_bus_count_plot
# Across all 10 villages, very few households own a business. Purba Dwarokapur and Sagar had the most, with 7 housholds reporting to own a business.
# Its not surprising to see that most hosueholds do not own a business; since the region is largely in poverty, it would not be feasible for most households, as operating a business often requires frequent financial injections as well as absorbing financial risk
# Rather than owning a business, most members of the household provide farm labor



# Below poverty line data frame and stacked bar plot
values_pl <- c(17,11,20,10,32,18,19,9,14,16,17,11,18,10,23,5,21,6,21,7)
dat_pl <- data.frame(villages_2, Answer, values_pl)
village_pl_count_plot <- ggplot(dat_pl, aes(x= villages_2, y = values_pl, fill = Answer)) + 
  geom_col(position = 'stack') + 
  labs( x= "", y = "Total Number of Households") + 
  theme_classic() + 
  ggtitle("Count of Households That Live Below the Poverty Line") +
  coord_flip()
village_pl_count_plot

# ---------------------------------------------------------

# data frames of village counts
Village_bus_data <- data.frame(village = c("Amrabati", "Beguakhali", "Bijoynagar","Birajnagar", "Haridaskati Samsernagar", "Lakshmi Janardanpur", "Pargumti", "Purba Dwarokapur", "Sagar", "Shibpur"),
                               No = c(26, 27, 48, 24, 27, 25, 24, 21, 21, 25),
                               Yes = c(2, 3, 2, 4, 3, 3, 4, 7, 7, 3))

Village_pl_data <- data.frame(village = c("Amrabati", "Beguakhali", "Bijoynagar","Birajnagar", "Haridaskati Samsernagar", "Lakshmi Janardanpur", "Pargumti", "Purba Dwarokapur", "Sagar", "Shibpur"),
                              No = c(17, 20, 32, 19, 14, 17, 18, 23, 21, 21),
                              Yes = c(11, 10, 18, 9, 16, 11, 10, 5, 6, 7))

# ----------------------------------------------------------



## Below the poverty line
# Histogram of households below the poverty line
below_pov_line <- ggplot(selected_bl, aes(x = below_204inr_bl)) + 
  geom_histogram(color = "blue", fill = " light blue", binwidth = 0.5) +
  theme_classic() +
  labs(x = "0 = 'No'  1 = 'Yes'", y = "Number of Households Below the Poverty Line")
below_pov_line
# Count of households below the poverty line
below_pl_count <- selected_bl %>% 
  count(below_204inr_bl)
below_pl_count
# Living below the poverty line means that you make below 204 rupee per week per person
# 92 of the 230 hh live below the poverty line (~40%)
# 137 reported that they do not live under the poverty line (~60%)
# One NA reported and removed from histogram
#Haridaskati is the only village that more households report to be below the poverty line than not 

# ----------------------------------------------------------

## Households that own a business
# Histogram of households that own a business
have_business <- ggplot(selected_bl, aes(x = unclass(business_yn))) + 
  geom_histogram(color = "green", fill = "dark green", binwidth = 0.5) +
  theme_classic() + 
  labs(x = "0 = 'No'  1 = 'Yes'", y = "Number of households That Own a Business")
have_business
# Count of households that own a business
have_business_count <- selected_bl %>% 
  count(business_yn)
have_business_count
# 30 of the 230 hh reported owning a business (~13%)
# 199 hh do not own a business (~86.5%)
# One NA reported and removed from histogram


own_land_boxplot <- ggplot(selected_bl, aes(x = land_own)) + 
  geom_boxplot(position = position_dodge(2), color = "dark orange") +
  labs(x = "'Kathas' of Land Owned", y = " Number of Households") + 
  theme_classic() +
  facet_wrap(~village)
own_land_boxplot





















