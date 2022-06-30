
mean_rmt <- rmt %>% 
  group_by(village, week) %>% 
  summarise(mean_weekly_rmt = mean(rmt_total))
mean_rmt[is.na(mean_rmt)] <- 0
