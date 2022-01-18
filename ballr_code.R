library(tidyverse)
library(jsonlite)
library(httr)
players_url = "https://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=2020-21&IsOnlyCurrentSeason=0"

request_headers = c(
  "Accept" = "application/json, text/plain, */*",
  "Accept-Language" = "en-US,en;q=0.8",
  "Cache-Control" = "no-cache",
  "Connection" = "keep-alive",
  "Host" = "stats.nba.com",
  "Pragma" = "no-cache",
  "Referer" = "https://www.nba.com/",
  "Upgrade-Insecure-Requests" = "1",
  "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9"
)

request = GET(players_url, add_headers(request_headers))
players_data = fromJSON(content(request, as = "text"))
players = as_tibble(data.frame(players_data$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
names(players) = tolower(players_data$resultSets$headers[[1]])
names(players) = tolower(players_data$resultSets$headers[[1]])

players = mutate(players,
                 person_id = as.numeric(person_id),
                 rosterstatus = as.logical(as.numeric(rosterstatus)),
                 from_year = as.numeric(from_year),
                 to_year = as.numeric(to_year),
                 team_id = as.numeric(team_id)
)


fetch_shots_by_player_id_and_season = function(player_id, season, season_type = "Regular Season") {
  #req(player_id, season, season_type)
  
  request = GET(
    "https://stats.nba.com/stats/shotchartdetail",
    query = list(
      PlayerID = player_id,
      Season = season,
      SeasonType = season_type,
      PlayerPosition = "",
      ContextMeasure = "FGA",
      DateFrom = "",
      DateTo = "",
      GameID = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      Period = 0,
      Position = "",
      RookieYear = "",
      SeasonSegment = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers(request_headers)
  )
  
  stop_for_status(request)
  
  data = content(request)
  
  raw_shots_data = data$resultSets[[1]]$rowSet
  col_names = tolower(as.character(data$resultSets[[1]]$headers))
  
  if (length(raw_shots_data) == 0) {
    shots = data.frame(
      matrix(nrow = 0, ncol = length(col_names))
    )
  } else {
    shots = data.frame(
      matrix(
        unlist(raw_shots_data),
        ncol = length(col_names),
        byrow = TRUE
      )
    )
  }
  
  shots = as_tibble(shots)
  names(shots) = col_names
  
  shots = mutate(shots,
                 loc_x = -as.numeric(as.character(loc_x)) / 10,
                 loc_y = as.numeric(as.character(loc_y)) / 10 + hoop_center_y,
                 shot_distance = as.numeric(as.character(shot_distance)),
                 shot_made_numeric = as.numeric(as.character(shot_made_flag)),
                 shot_made_flag = factor(shot_made_flag, levels = c("1", "0"), labels = c("made", "missed")),
                 shot_attempted_flag = as.numeric(as.character(shot_attempted_flag)),
                 shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2),
                 game_date = as.Date(game_date, format = "%Y%m%d")
  )
  
  raw_league_avg_data = data$resultSets[[2]]$rowSet
  league_avg_names = tolower(as.character(data$resultSets[[2]]$headers))
  league_averages = as_tibble(data.frame(
    matrix(unlist(raw_league_avg_data), ncol = length(league_avg_names), byrow = TRUE)
  ))
  names(league_averages) = league_avg_names
  league_averages = mutate(league_averages,
                           fga = as.numeric(as.character(fga)),
                           fgm = as.numeric(as.character(fgm)),
                           fg_pct = as.numeric(as.character(fg_pct)),
                           shot_value = ifelse(shot_zone_basic %in% c("Above the Break 3", "Backcourt", "Left Corner 3", "Right Corner 3"), 3, 2)
  )
  
  return(list(player = shots, league_averages = league_averages))
}
if (Sys.Date() <= as.Date("2017-10-20")) {
  players = mutate(players, to_year = pmin(to_year, 2016))
}

players$name = sapply(players$display_last_comma_first, function(s) {
  paste(rev(strsplit(s, ", ")[[1]]), collapse = " ")
})

first_year_of_data = 1996
last_year_of_data = max(players$to_year)
season_strings = paste(first_year_of_data:last_year_of_data,
                       substr(first_year_of_data:last_year_of_data + 1, 3, 4),
                       sep = "-")
names(season_strings) = first_year_of_data:last_year_of_data

available_players = filter(players, to_year >= first_year_of_data)

names_table = table(available_players$name)
dupe_names = names(names_table[which(names_table > 1)])

available_players$name[available_players$name %in% dupe_names] = paste(
  available_players$name[available_players$name %in% dupe_names],
  available_players$person_id[available_players$name %in% dupe_names]
)

available_players$lower_name = tolower(available_players$name)
available_players = arrange(available_players, lower_name)

find_player_by_name = function(n) {
  filter(available_players, lower_name == tolower(n))
}

find_player_id_by_name = function(n) {
  find_player_by_name(n)$person_id
}

# default_shots = fetch_shots_by_player_id_and_season(
#   default_player$person_id,
#   default_season,
#   default_season_type
# )

lebron = find_player_by_name("LeBron James")
seasons = season_strings[as.character(2003:2015)]
court_theme = court_themes$dark

all_shots = bind_rows(lapply(seasons, function(season) {
  fetch_shots_by_player_id_and_season(lebron$person_id, season)$player %>%
    mutate(season = season)
}))




circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(tibble(x = center[1] + radius * cos(angles),
                y = center[2] + radius * sin(angles)))
}

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

plot_court = function(court_theme = court_themes$dark, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius = 22
    three_point_side_height = 0
  }
  
  court_points = tibble(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , tibble(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , tibble(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , tibble(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = tibble(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  court_points <<- court_points
  
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    coord_fixed(ylim = c(0, 35), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}
