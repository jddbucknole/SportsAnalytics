options(java.parameters = "-Xmx4g" )
library(rvest)
library(xlsx)

#Create Excel Workbook to export data to
scheduleData <- createWorkbook(type = "xlsx")

for (i in 2007:2018) {
  #Specifying the url for desired website to be scrapped
  if (i != 2012) { #Lockout season started in December 
    oct_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-october.html', sep = "")
    nov_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-november.html', sep = "")
  }
  dec_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-december.html', sep = "")
  jan_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-january.html', sep = "")
  feb_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-february.html', sep = "")
  mar_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-march.html', sep = "")
  apr_url <- paste('https://www.basketball-reference.com/leagues/NBA_', i, '_games-april.html', sep = "")
 
   #Reading the HTML code from the website
  if (i != 2012) { #Lockout season started in December
    oct_webpage <- read_html(oct_url)
    nov_webpage <- read_html(nov_url)
  }
  dec_webpage <- read_html(dec_url)
  jan_webpage <- read_html(jan_url)
  feb_webpage <- read_html(feb_url)
  mar_webpage <- read_html(mar_url)
  apr_webpage <- read_html(apr_url)
  
  #Turn HTML data into data frames for each season month
  if (i != 2012) { #Lockout season started in December
    oct_schedule <- as.data.frame(html_table(html_nodes(oct_webpage,'table'))[[1]])
    nov_schedule <- as.data.frame(html_table(html_nodes(nov_webpage,'table'))[[1]])
  }
  dec_schedule <- as.data.frame(html_table(html_nodes(dec_webpage,'table'))[[1]])
  jan_schedule <- as.data.frame(html_table(html_nodes(jan_webpage,'table'))[[1]])
  feb_schedule <- as.data.frame(html_table(html_nodes(feb_webpage,'table'))[[1]])
  mar_schedule <- as.data.frame(html_table(html_nodes(mar_webpage,'table'))[[1]])
  apr_schedule <- as.data.frame(html_table(html_nodes(apr_webpage,'table'))[[1]])
  
  #Combine all data frames into one data frame for each season
  if(i != 2012) { #Lockout season started in December
    schedule <- rbind(oct_schedule,nov_schedule,dec_schedule,jan_schedule,feb_schedule,mar_schedule,apr_schedule)
  } else {
    schedule <- rbind(dec_schedule,jan_schedule,feb_schedule,mar_schedule,apr_schedule)
  }
  
  #Convert points to numbers for later calculations
  schedule$PTS <- as.numeric(as.character(schedule$PTS))
  schedule$PTS.1 <- as.numeric(as.character(schedule$PTS.1))
  
  #Use only Regular Season
  if (i != 2018) { #Playoffs haven't started yet so don't need this
    schedule <- schedule[-c(which(schedule$`Visitor/Neutral` == "Playoffs"):nrow(schedule)),]
  }
  
  #Export to Excel
  scheduleYear <- createSheet(scheduleData, sheetName = toString(i))
  addDataFrame(schedule, scheduleYear, row.names = FALSE)
  saveWorkbook(scheduleData, "Schedule Data.xlsx")
  
  rm(list = ls(all = TRUE))
}
