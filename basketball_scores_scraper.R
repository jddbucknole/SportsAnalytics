library(rvest)
library(lubridate)

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
full <- map2(bballscorescraper, .x = c("october","november","december","january", "february", "march","april"), .y = 2020) %>%
  bind_rows(.id = "column_label")
# change columns to the correct types
full$visitor_pts <- as.numeric(full$visitor_pts)
full$home_pts    <- as.numeric(full$home_pts)
full$attendance  <- as.numeric(gsub(",", "", full$attendance))
full$date_game   <- mdy(full$date_game)
full <- filter(full, !is.na(visitor_pts))

# drop boxscore column
df$box_score_text <- NULL

