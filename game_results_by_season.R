library(tidyverse)
#devtools::install_github(repo = "saiemgilani/cfbfastR")


#College Football
cfb19 <-cfbfastR::cfbd_game_info(year = 2019) %>%
  select(season, week, neutral_site,home_team, home_conference,home_points,
         away_team, away_conference, away_points)

write_csv(cfb19,'cfb_games_2019.csv')


#NFL
library(nflfastR)
nfl2020<- nflfastR::fast_scraper_schedules(seasons = 2020) 

write_csv(nfl2020,'nfl2020.csv')


#NBA
library(rvest)
library(lubridate)
library(tidyverse)

bballscorescraper <- function(month, year){
  
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                "_games-", month, ".html")
  webpage <- read_html(url)
  col_names <- webpage %>% 
    html_nodes("table#schedule > thead > tr > th") %>% 
    html_attr("data-stat")    
  col_names <- c("game_id", col_names)
  
  dates <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > th") %>% 
    html_text()
  dates <- dates[dates != "Playoffs"]
  
  game_id <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > th") %>%
    html_attr("csk")
  game_id <- game_id[!is.na(game_id)]
  
  data <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > td") %>% 
    html_text() %>%
    matrix(ncol = length(col_names) - 2, byrow = TRUE)
  
  month_df <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
  names(month_df) <- col_names
  month_df
}

months_to_scrape <-c("december","january", "february", "march","april")
full <- map2(bballscorescraper, .x = months_to_scrape, .y = 2021) %>%
  bind_rows(.id = "column_label")
# change columns to the correct types
full$visitor_pts <- as.numeric(full$visitor_pts)
full$home_pts    <- as.numeric(full$home_pts)
full$attendance  <- as.numeric(gsub(",", "", full$attendance))
full$date_game   <- mdy(full$date_game)
full <- filter(full, !is.na(visitor_pts))

# drop boxscore column
full$box_score_text <- NULL
write_csv(full, "nba_scores_2021.csv")



###Baseball
team_results <-function(Tm, year) {

  url <- paste0("https://www.baseball-reference.com/teams/",Tm,"/",year,"-schedule-scores.shtml")


data <- read_html(url) %>%
  html_nodes("table")

data <- data[[length(data)]] %>%
  html_table() %>%
  .[-3]


col_names <- c('Gm','Date','Tm','H_A','Opp','Result','R','RA','Inn','Record','Rank',
               'GB','Win','Loss','Save','Time','D/N','Attendance', 'cLI', 'Streak', 'Orig_Scheduled')

names(data) <- col_names

data$H_A <- ifelse(grepl("@", data$H_A, fixed = TRUE), "A", "H")

data$Attendance <- gsub(",", "", data$Attendance)

data$Streak <- ifelse(grepl("-", data$Streak, fixed = TRUE), nchar(data$Streak) * -1, nchar(data$Streak) * 1)

# for (i in c("R", "Rank", "Attendance")) {
#   i <- "R"
#   if(!is.numeric(data[, i])) {
#     data[,i] <- suppressWarnings(as.numeric(data[,i]))
#   }
# }

data$Year <- year
data <- data[, 1:ncol(data)]
data <- data %>%
  dplyr::filter(!grepl("Gm#", Gm))

return(data)
}
teams1 <- team_results("CLE",2021)%>% 
  select(Opp) %>%
  unique() 
teams2 <-  team_results("CIN",2021)%>% 
  select(Opp) %>%
  unique()
teams <- teams1 %>% bind_rows(teams2) %>% unique()
teams <- as.list(teams)
teams<- unlist(teams)
test <- map(teams, team_results, year = 2021)

test <- bind_rows(test, .id = "column_label")


scores2021 <-test %>% filter(H_A == "H") %>% distinct(Date, Win, .keep_all= T)

write_csv(scores2021,"bballscores_2021.csv")
