library(tidyverse)
library(rvest)
library(xml2)
# team <- "GSW"
# yearID <- "2019"
# playoffs <- "N"
# offset <- 100
# 
# url <- paste0("https://www.basketball-reference.com/play-index/lineup_finder.cgi?request=1&match=single&player_id=&lineup_type=5-man&output=total&year_id=",yearID,"&is_playoffs=",playoffs,"&team_id=",team, "&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=&c1comp=&c1val=&c2stat=&c2comp=&c2val=&c3stat=&c3comp=&c3val=&c4stat=&c4comp=&c4val=&order_by=diff_pts&order_by_asc=&offset=",offset)
# 
# test <- url3 %>%
#   read_html() %>%
#   html_nodes(xpath = '//*[@id="div_sched_ks_1889_1"]') %>%
#   html_table()
url <- "https://fbref.com/en/comps/9/schedule/Premier-League-Stats"
test <- url %>%
   read_html() %>%
   html_nodes(xpath = '//*[@id="sched_ks_3232_1"]') %>%
   html_table() %>%
  as.data.frame() %>%
  filter(!is.na(Wk)) %>%
  separate(col = Score,into = c("HomeScore","AwayScore"), sep= "–") %>%
  mutate(HomeScore = as.numeric(HomeScore),AwayScore = as.numeric(AwayScore), Attendance = as.numeric(gsub(",","",Attendance)))%>%
  select(-Match.Report,-Notes) 

write_csv(test,"epl_2019-20.csv")



