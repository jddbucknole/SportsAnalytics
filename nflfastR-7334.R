library(tidyverse)
library(nflfastR)
install.packages("tictoc")
library(tictoc)


tic()
games2020 <- nflfastR::fast_scraper_schedules(seasons = 2020)
nfl2020 <- nflfastR::fast_scraper(game_ids = games2020$game_id)
toc()
#260 sec

tic()
nfl2020 <- readRDS(
  url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds")
)
toc()
#2 sec

nfl2000 <- readRDS(
  url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2000.rds")
)


nfl2020 %>%
  distinct(play_type)

nfl2020 %>%
  select(posteam, down, play_type) %>%
  filter(down == 1, play_type %in% c('pass','run')) %>%
  group_by(posteam) %>%
  summarize(pct_1st_down_pass = mean(play_type == 'pass')) %>%
  arrange(-pct_1st_down_pass)



nfl2020 <- nfl2020 %>%
  mutate(season = "Y2020")

nfl2000 %>%
  mutate(season = "Y2000") %>%
  bind_rows(nfl2020) %>%
  select(posteam, down, play_type, season) %>%
  filter(down == 1, play_type %in% c('pass','run')) %>%
  group_by(posteam, season) %>%
  summarize(pct_1st_down_pass = mean(play_type == 'pass')) %>%
  pivot_wider(names_from = season, values_from = pct_1st_down_pass) %>%
  ggplot(aes(x = Y2000, y = Y2020, label = posteam)) + geom_text() + geom_abline(intercept = 0, slope = 1)
  
