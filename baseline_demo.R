library(dplyr)

baseline <- livdiv %>%
  filter(week_num == 0:4) %>%
  arrange(date) %>%

baseline %>%
  summarise("farm_yn")

baseline %>%
summarise_at(c("farm_yn", "head_edu", "head_age", "head_married", "head_female", "nb_children", "hh_dependents_prop"), mean, na.rm = TRUE)

sumstats <- baseline %>%
summarise_at(c("farm_yn", "head_edu", "head_age", "head_married", "head_female", "nb_children", "hh_dependents_prop"), mean, na.rm = TRUE)

sumstats2 <- baseline %>%
  summarise_at(c("farm_yn", "head_edu", "head_age", "head_married", "head_female", "nb_children", "hh_dependents_prop"), range, na.rm = TRUE)

sumstats <- rbind(sumstats, sumstats2)

rownames <- c("Mean", "Min", "Max")
row.names(sumstats) <- rownames

villagecount <- baseline %>% 
  count(village) 

baseline %>% 
  group_by(village) 
  
  


         