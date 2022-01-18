#devtools::install_github(repo = "saiemgilani/wehoop")

library(wehoop)
library(tidyverse)
dat <-wnba_espn_scoreboard(2020)
dat %>%
  select(home_team_full, home_score, away_team_full, away_score) %>%
  write_csv('wnba_2020_scores.csv')
