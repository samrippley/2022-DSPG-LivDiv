



Village_bus_data <- data.frame(village = c("Amrabati", "Beguakhali", "Bijoynagar","Birajnagar", "Haridaskati Samsernagar", "Lakshmi Janardanpur", "Pargumti", "Purba Dwarokapur", "Sagar", "Shibpur"),
                               No = c(26, 27, 48, 24, 27, 25, 24, 21, 21, 25),
                               Yes = c(2, 3, 2, 4, 3, 3, 4, 7, 7, 3))

Village_pl_data <- data.frame(village = c("Amrabati", "Beguakhali", "Bijoynagar","Birajnagar", "Haridaskati Samsernagar", "Lakshmi Janardanpur", "Pargumti", "Purba Dwarokapur", "Sagar", "Shibpur"),
                              No = c(17, 20, 32, 19, 14, 17, 18, 23, 21, 21),
                              Yes = c(11, 10, 18, 9, 16, 11, 10, 5, 6, 7))


# Create new data frames with Village counts (business and below poverty line)
# Stacked bar plots 
villages_2 <- c(rep("Amrabati", 2), rep("Beguakhali", 2), rep("Bijoynagar", 2), rep("Birajnagar", 2), rep("Haridaskati Samsernagar", 2), rep("Lakshmi Janardanpur",2), rep("Pargumti",2),rep("Purba Dwarokapur", 2), rep("Sagar", 2), rep("Shibpur",2))
Answer <- rep(c("No", "Yes"), 2)
values_bus <-c(26,2,27,3,48,2,24,2,27,3,25,3,24,4,21,7,21,7,25,3)
#prop_bus_values <- c(0.93,0.07, 0.9,0.1,0.96,0.04,0.86,0.14,0.9,0.1,0.9,0.1,0.86,0.14,0.75,0.25,0.75,0.25,0.9,0.1)
prop_bus_values <- c("93%","7%","90%","10%","96%","4%","86%","14%","90%","10%","90%","10%","86%","14%","75%","25%","75%","25%","90%","10%")

dat_bus <-  data.frame(villages_2, Answer, values_bus, prop_bus_values)
village_bus_count_plot <- ggplot(dat_bus, aes(x= villages_2, y = values_bus, fill = Answer)) + 
  geom_col(position = 'stack') + 
  labs( x= "", y = "Total Number of Households") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Count of Households That Own a Business")+
  geom_text(aes(label = prop_bus_values))
village_bus_count_plot


values_pl <- c(17,11,20,10,32,18,19,9,14,16,17,11,18,10,23,5,21,6,21,7)
prop_pl_values <- c("60%", "40%", "67%", "33%", "64%","36%","68%","32%","47%","53%","60%","40%","64%","36%","82%","18%","77%","23%","75%","25%" )
dat_pl <- data.frame(villages_2, Answer, values_pl, prop_pl_values)
village_pl_count_plot <- ggplot(dat_pl, aes(x= villages_2, y = values_pl, fill = Answer)) + 
  geom_col(position = 'stack') + 
  labs( x= "", y = "Total Number of Households") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Count of Households That Live Below the Poverty Line")+
  geom_text(aes(label = prop_pl_values), size = 2.5)
village_pl_count_plot


grouped_bus <- selected_bl %>% group_by(village) %>% summarize(prop_bus_yes = sum(business_yn)/n(), prop_bus_no = 1 - prop_bus_yes)
grouped_pl <- selected_bl %>% group_by(village) %>% summarize(prop_pl_yes = sum(below_204inr_bl)/n(), prop_pl_no = 1 - prop_pl_yes)



