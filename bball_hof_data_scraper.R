devtools::install_github("abresler/nbastatR")
install.packages('rvest')
library(nbastatR)
library(rvest)
library(tidyverse)

bball_hof <- read_html('https://en.wikipedia.org/wiki/List_of_players_in_the_Naismith_Memorial_Basketball_Hall_of_Fame')
hof <- bball_hof %>%
  html_table(fill = T) %>%
  .[[1]]  
#all <- nbastatR::bref_players_stats(seasons = "2000")


hof_players <- hof$Inductees
hof_data <- nbastatR::players_careers(players = hof_players, modes = c("PerGame","Totals"))


