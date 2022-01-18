library(nflfastR)
library(tidyverse)
sch <- fast_scraper_schedules(2020)
write_csv(sch, "nfl2020.csv")
pbp <- nflfastR::build_nflfastR_pbp(game_ids = '2018_11_KC_LA')
pbp %>%
  group_by(play_type) %>%
  summarize(mean(epa)) %>%
  arrange(-`mean(epa)`) %>%
  filter(play_type %in% c("pass", "run", "sack", "field_goal", "punt"))


