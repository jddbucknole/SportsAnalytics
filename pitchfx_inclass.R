#install.packages("devtools")
#devtools::install_github("BillPetti/baseballr")



library(tidyverse)
library(baseballr)


View(playerid_lookup(last_name = "Chapman", first_name = "Aroldis"))

bauer <- baseballr::scrape_statcast_savant_pitcher(start_date = "2016-04-06", end_date = "2016-04-15", 
                                                   pitcherid=545333)

chap <- scrape_statcast_savant_pitcher(start_date = "2010-04-30", end_date = "2020-09-20", 
                                       pitcherid = 547973)

#Jim Albert's Workshop
# bit.ly/Rbaseball

source("http://bit.ly/add_zone")
d <- read_csv("http://bit.ly/4pitchers")
