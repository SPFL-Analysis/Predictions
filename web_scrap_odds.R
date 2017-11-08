
########################################################################################################
# load the librarys
########################################################################################################

library(rvest)
library(stringr)
library(stringi)
library(RODBC)
library(data.table)

########################################################################################################
# Get the table points totals
########################################################################################################

tbl_Prem <- getTable("https://spfl.co.uk/premiership/table/")

tbl_Champ <- getTable("https://spfl.co.uk/championship/table/")

tbl_L1 <- getTable("https://spfl.co.uk/league-one/table/")

tbl_L2 <- getTable("https://spfl.co.uk/league-two/table/")

season <- "2017-2018"
date <- "2017-11-01"

writeTableSQL(season, 'Premiership', date, tbl_Prem)
writeTableSQL(season, 'Championship', date, tbl_Champ)
writeTableSQL(season, 'League 1', date, tbl_L1)
writeTableSQL(season, 'League 2', date, tbl_L2)


########################################################################################################
# run functions -----------------------------------------------------------------------------------------------------
########################################################################################################

# archive odds from earlier in the week ------------------------------------------------------------------------------

update_qry <- 
  "update market_odds 
set archive_odds_date = '2017-09-30' 
where [date] = '2017-09-30' and odds_date = '2017-09-25'"

sqlQuery(channel,update_qry)

#######################################

season <- '2017-2018'
date <- '2017-11-01'
game_date <- '2017-11-04'


data <- getOddsChecker('https://www.oddschecker.com/football/scottish/premiership')
writeOddsSQL(season, 'Premiership', date, game_date, data$teams, data$OddsFrac)

data <- getOddsChecker("https://www.oddschecker.com/football/scottish/championship")
writeOddsSQL(season, 'Championship', date, game_date, data$teams, data$OddsFrac)

data <- getOddsChecker('https://www.oddschecker.com/football/scottish/league-1')
writeOddsSQL(season, 'League 1', date, game_date, data$teams, data$OddsFrac)

data <- getOddsChecker('https://www.oddschecker.com/football/scottish/league-2')
writeOddsSQL(season, 'League 2', date, game_date, data$teams, data$OddsFrac)


#########################################################################################################
# function to write the matches and odds to the SQL table ----------------------------------------------
#########################################################################################################

getOddsChecker <- function(link) {
  html <- read_html(link)
  Team_nodes <- html_nodes(html, ".fixtures-bet-name")
  Odds_nodes <- html_nodes(html, ".odds")
  
  teams <- html_text(Team_nodes)
  Odds <- html_text(Odds_nodes)
  
  Odds <- Odds[1:length(teams)]
  
  Odds <- str_replace_all(Odds, '\\(', '')
  Odds <- str_replace_all(Odds, "\\)", "")
  Odds <- str_replace_all(Odds, " ", "")
  numerator <-
    as.numeric(stri_sub(Odds, 1, stri_locate_first_coll(Odds, '/')[, 1] - 1))
  denom <-
    as.numeric(stri_sub(
      Odds,
      stri_locate_first_coll(Odds, '/')[, 1] + 1,
      stri_length(Odds)
    ))
  OddsFrac <- 1.000 + numerator / denom
  
  return(list(teams = teams, OddsFrac = OddsFrac))
}

#########################################################################################################
# function to write the matches and odds to the SQL table
#########################################################################################################

writeOddsSQL <- function(season, comp, date, game_date, teams, OddsFrac) {
  
  channel <- odbcConnect("SPFL")
  
  for(i in 1:(length(teams)/3)) {
    
    if(i==1) {
      j <- 1
    } else {
      j <- j + 3
    }
    
    qry <- paste("insert into market_odds select '"
                 , season, "', '"
                 , comp, "', '"
                 , game_date, "', '"
                 , teams[j], "', '"
                 , teams[j+2], "', "
                 , OddsFrac[j], ", "
                 , OddsFrac[j+1], ", "
                 , OddsFrac[j+2], ", '"
                 , date, "', null"
                 , sep="") # create SQL qry
    
    sqlQuery(channel, qry) #run query
    
  }
  
  close(channel)
}

#########################################################################################################
# function to write the table to the SQL table
#########################################################################################################

writeTableSQL <- function(season, comp, date, df) {
  
  channel <- odbcConnect("SPFL")
  
  for (i in 1:length(df$team)) {
    
    qry <- paste("insert into SPFLtables select '"
                 , season, "', '"
                 , comp, "', '"
                 , date, "', '"
                 , df[i,1], "', "
                 , df[i,2], ", "
                 , df[i,3]
                 , sep="") # create SQL qry
    
    sqlQuery(channel, qry) #run query
    
  }
  
  close(channel)
}


#########################################################################################################
# function to web scrape tables
#########################################################################################################

getTable <- function(webLink) {
  
  html <- read_html(webLink)
  nodes <- html_nodes(html, ".team_name, .pld, .pts")
  text <- html_text(nodes)
  
  team <- NULL
  played <- NULL
  points <- NULL
  
  for (i in 1:length(text)) {
    
    if(i == 1) j <- 4 else j <- j + 3
    
    team <- rbind(team,text[j])
    played <- rbind(played,text[j+1])
    points <- rbind(points,text[j+2])
    
    table <- na.omit(data.frame(team = team, played = played, points = points))
  }
  
  table
}