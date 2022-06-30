
fd <- livdiv %>%
  select(-(4:967))

rmt_dat <- fd %>% 
  select(village, date, week, rmt_total) %>% 
  arrange(week, village) %>% 
  group_by(week)
rmt_dat$date <- as_date(rmt_dat$date)  
avg_rmt <- rmt_dat %>% 
  group_by(date, village) %>% 
  summarize("Average Remmitances" = mean(rmt_total, na.rm = T))
      
