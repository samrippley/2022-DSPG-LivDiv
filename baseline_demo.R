library("dplyr")

load("~/Virginia Tech/Internship 2022/2022-DSPG-LivDiv-/data/livdivdata.RData")

baseline <- livdiv %>%
  filter(week_num == 0:4) %>%
  arrange(date)

baseline %>%
  summarise("farm_yn")

baseline %>%
summarise_at(c("farm_yn", "head_edu", "head_age", "head_married", "head_female", "nb_children", "hh_dependents_prop"), mean, na.rm = TRUE)

sumstats <- baseline %>%
summarise_at(c("farm_yn", "head_edu", "head_age", "head_married", "head_female", "nb_children", "hh_dependents_prop"), mean, na.rm = TRUE)

sumstats2 <- baseline %>%
  summarise_at(c("farm_yn", "head_edu", "head_age", "head_married", "head_female", "nb_children", "hh_dependents_prop"), min, na.rm = TRUE)

sumstats3 <- baseline %>%
  summarise_at(c("farm_yn", "head_edu", "head_age", "head_married", "head_female", "nb_children", "hh_dependents_prop"), max, na.rm = TRUE)

sumstats <- rbind(sumstats, sumstats2, sumstats3)

rownames <- c("Mean", "Min", "Max")
row.names(sumstats) <- rownames

villagecount <- baseline %>% 
  count(village) 

groupvillage <- baseline %>% 
  group_by(village) %>% 
  summarize(meanage = mean(head_age), meanedu = mean(head_edu), meanmarried = mean(head_married), meanfem = mean(head_female))

countfem <- baseline %>%
  group_by(village) %>%
  count(head_female) 
  
countmar <- baseline %>%
  group_by(village) %>%
  count(head_married) 



baseline %>%
  group_by(head_female) %>%
  count(village) 

baseline %>%
  group_by(head_married) %>%
  count(village) 

groupmar <- baseline %>% 
  group_by(head_married) %>% 
  summarize(meanage = mean(head_age), meanedu = mean(head_edu), meanfem = mean(head_female))


groupfem <- baseline %>% 
  group_by(head_female) %>% 
  summarize(meanage = mean(head_age), meanedu = mean(head_edu), meanmar = mean(head_married))


         