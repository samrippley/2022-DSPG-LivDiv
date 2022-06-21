library("dplyr")
library(tidyverse)
library(dyplr)

load("~/Virginia Tech/Internship 2022/2022-DSPG-LivDiv-/data/livdivdata.RData")

baseline <- livdiv %>%
  slice(1:307,)

baseline %>%
  summarise("farm_yn")

baseline %>%
summarise_at(c("head_age", "head_edu", "head_female", "nb_hhmem", "nb_children", "hh_migrant_prop"), mean, na.rm = TRUE)

sumstats <- baseline %>%
summarise_at(c("head_age", "head_edu", "head_female", "nb_hhmem", "nb_children", "hh_migrant_prop"), mean, na.rm = TRUE)

sumstats2 <- baseline %>%
  summarise_at(c("head_age", "head_edu", "head_female", "nb_hhmem", "nb_children", "hh_migrant_prop"), min, na.rm = TRUE)

sumstats3 <- baseline %>%
  summarise_at(c("head_age", "head_edu", "head_female", "nb_hhmem", "nb_children", "hh_migrant_prop"), max, na.rm = TRUE)

sumstats <- rbind(sumstats, sumstats2, sumstats3)

rownames <- c("Mean", "Min", "Max")
row.names(sumstats) <- rownames

villagecount <- baseline %>% 
  count(village) 

sum(livdiv(week))


baseline %>%
  group_by(head_female) %>%
  count(village) 

baseline %>%
  group_by(head_married) %>%
  count(village) 

groupmar <- baseline %>% 
  group_by(head_married) %>% 
  summarize(meanage = mean(head_age), meanedu = mean(head_edu), meanfem = mean(head_female))

ggplot(group, aes(x = "village", y = "meanmar")) +
         geom_col()



library(ggplot2)
countmar <- baseline %>%
  count(head_married) 

sum(villagecount$n)

# Create a bar plot of age by village 
ggplot(baseline, aes(x = village, y = head_age)) +
  geom_col()

# Create a bar plot of head female by village 
ggplot(baseline, aes(x = village, y = head_female)) +
  geom_col()



group <- baseline %>% 
  group_by(head_married) %>%
  mutate(meanfem = mean(head_female))  

ggplot(group, aes(x = head_married, y = meanfem)) +
  geom_col()

by_village <- baseline %>% 
  group_by(village ) %>% 
  summarize(meanage = mean(head_age))

# bar plot showing meanage by village 
ggplot(by_village, aes(x = village, y = meanage)) +
  geom_col()

by_villagefem <- baseline %>% 
  group_by(village) %>% 
  filter(head_female == 1) %>%
  summarize(meanage = mean(head_age))

# bar plot showing meanage by village for female lead hh 
ggplot(by_villagefem, aes(x = village, y = meanage)) +
  geom_col()


by_villagemore <- baseline %>% 
  group_by(village ) %>% 
  summarize(meanage = mean(head_age), meanedu = mean(head_edu), meanmar = mean(head_married))

by_villagemore %>% 
  mutate(across(, as.double))

class(by_villagemore$meanmar)
class(by_villagemore$meanage)
library(tidyverse)
library(dyplr)
as.numeric(by_villagemore$meanmar)

#by village mean stats 

by_villagemore <- baseline %>% 
  group_by(village ) %>% 
  summarize(meanage = mean(head_age), meanedu = mean(head_edu), meanmar = mean(head_married))

ggplot(by_villagemore, aes(x = village, y = meanmar)) +
  geom_col()

ggplot(by_villagemore, aes(x = village, y = meanage)) +
  geom_col()

ggplot(by_villagemore, aes(x = village, y = meanedu)) +
  geom_col()


#by village mean stats for female headed hh



femmore <- baseline %>% 
  group_by(head_female) %>%
  summarize(meanage = mean(head_age), meanedu = mean(head_edu), meanmar = mean(head_married))

ggplot(femmore, aes(x = head_female, y = meanmar)) +
  geom_col()

ggplot(femmore, aes(x = village, y = meanage)) +
  geom_col()

ggplot(femmore, aes(x = village, y = meanedu)) +
  geom_col()


count <- baseline %>%
  group_by(village) %>%
  summarize(fem = sum(head_female), mar = sum(head_married))

villagecount <- baseline %>% 
  count(village) 

count <- baseline %>%
  group_by(village) %>%
  summarize(fem = sum(head_female), mar = sum(head_married))

counted <- cbind(count, villagecount)

as.numeric(counted$fem)
as.numeric(counted$mar)
as.numeric(counted$n)
 



baseline %>%
  summarizecount(head_married) 

countfem <- baseline %>%
  group_by(village) %>%
  count(head_female) 

countmar <- baseline %>%
  group_by(village) %>%
  count(head_married) 

         