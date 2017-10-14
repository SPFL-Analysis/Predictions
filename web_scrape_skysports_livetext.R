#####load the librarys

library(rvest)
library(stringr)
library(stringi)
library(RODBC)
library(data.table)
library(dplyr)


#############################################################################################
# clean up the links data table
#############################################################################################

SPFL_LeagueCup <- read.csv("C:/Users/David/Downloads/LeagueCup_QF_25th_Sept_2017.csv")

SPFL_Prem <- read.csv("C:/Users/David/Downloads/SPFL_Prem_1st_Oct_2017.csv")
SPFL_Champ <- read.csv("C:/Users/David/Downloads/SPFL_Champ_1st_Oct_2017.csv")
SPFL_L1 <- read.csv("C:/Users/David/Downloads/SPFL_L1_1st_Oct_2017.csv")
SPFL_L2 <- read.csv("C:/Users/David/Downloads/SPFL_L2_1st_Oct_2017.csv")



filter(SPFL_Prem, grepl("http://www.skysports.com/football", Link, fixed = TRUE))[1:6,] %>%
  select(Link) %>%
  getSkyLiveText(., 'SPFL_live_text', '2017-2018', 'Premiership', '2017')


filter(SPFL_Champ, grepl("http://www.skysports.com/football", Link, fixed = TRUE))[1:5,] %>%
  select(Link) %>%
  getSkyLiveText(., 'SPFL_live_text', '2017-2018', 'Championship', '2017')

filter(SPFL_L1, grepl("http://www.skysports.com/football", Link, fixed = TRUE))[1:5,] %>%
  select(Link) %>%
  getSkyLiveText(., 'SPFL_live_text', '2017-2018', 'League 1', '2017')

filter(SPFL_L2, grepl("http://www.skysports.com/football", Link, fixed = TRUE))[1:5,] %>%
  select(Link) %>%
  getSkyLiveText(., 'SPFL_live_text', '2017-2018', 'League 2', '2017')




#############################################################################################
# create function
# loop round all the links and get the live text from them
# insert the live text into a SQL tbl
#############################################################################################

getSkyLiveText <- function(links, SQL_tbl , season, comp, year) {

  for (i in 1:nrow(links)) {
  
  locateStr <- stri_locate_last_fixed(toString(links[i,1]), "/") #locate the last / in the link
  splitStr <- as.numeric(locateStr[1,1]) #stroe the index of this 
  string1 <- str_sub(toString(links[i,1]), 1, splitStr) #split the link in 2 from index 1 to the located index
  string2 <- str_sub(toString(links[i,1]), splitStr,str_length(toString(links[i,1]))) #second split from located index to the end
  newlink <- paste(string1, "live", string2, sep ="") #combine the 2 strings and add in the required 'live'
  
  #scrape the live text from the link
  html <- read_html(newlink)
  livetextnodes <- html_nodes(html, ".live-text__text")
  livetext <- html_text(livetextnodes)
  
  
  # scrpae for the teams and the date
  teamNodes <- html_nodes(html, ".swap-text--bp10")
  teams <- html_text(teamNodes)
  home = str_replace(str_trim(str_replace(teams[1],"\n                        ", ""), side = "both"),"'","")
  away = str_replace(str_trim(str_replace(teams[2],"\n                        ", ""), side = "both"),"'","")
  
  Datenodes <- html_nodes(html, ".match-header__detail-item:nth-child(2)")
  livetextDate <- html_text(Datenodes)
  matchDate = str_replace(str_trim(str_replace(livetextDate[1],"\n                        ", ""), side = "both"),"'","")
  matchDate = paste(matchDate, year, sep = ' ')
  
  #open SQl channel
  channel <- odbcConnect("SPFL")
  
  #new loop to insert every row of live text
  
if(length(livetext) > 0)  {

  for (j in 1:length(livetext)) {
    
      newlivetext <- str_trim(str_replace(livetext[j],"\n                        ", ""), side = "left") #tidy up the live text
      newlivetext <- str_replace_all(newlivetext,"'", "") #replace ' with blank for SQL insert
      qry <- paste("insert into ", SQL_tbl, " select '"
                   , season, "', '"
                   , comp, "', '"
                   , home, "', '"
                   , away, "', '"
                   , newlink, "', '"
                   , newlivetext, "', "
                   , as.numeric(j), ", '"
                   , matchDate, "'"
                   , sep="") # create SQL qry
      
      sqlQuery(channel, qry) #run query
  }
  
} else {
  
      qry <- paste("insert into ", SQL_tbl, " select '"
                   , season, "', '"
                   , comp, "', '"
                   , home, "', '"
                   , away, "', '"
                   , newlink, "', 'No live text with the CSS available', 1"
                   , sep="") # create SQL qry
      sqlQuery(channel, qry) #run query
}
  
  close(channel)
  
  }
  }


##################################################################################################