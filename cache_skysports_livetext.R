#####load the librarys
library(purrr)
library(rvest)
library(stringr)
library(stringi)
library(data.table)
library(dplyr)

source("functions.R")
purrr::walk(list.files("R", full.names = T), ~ source(.))
yearGames <- '2020'
season <- '2019-2020'

#############################################################################################
# clean up the links data table
#############################################################################################

league_cup <-
  getLinks("http://www.skysports.com/scottish-league-cup-results")

SPFL_Prem <- 
  getLinks("http://www.skysports.com/scottish-premier-results")

SPFL_Champ <- 
  getLinks("http://www.skysports.com/football/competitions/scottish-championship/results")

SPFL_L1 <-  
  getLinks("http://www.skysports.com/football/competitions/scottish-league-one/results")

SPFL_L2 <- 
  getLinks("http://www.skysports.com/football/competitions/scottish-league-two/results")

nprem <-6
nchamp <- 4
nl1 <- 5
nl2 <- 5

prem_live_text <- NULL
if(nprem > 0) {
  prem_live_text <-
    createSkyLiveTextDf(SPFL_Prem[1:nprem,], 
                        'SPFL.dbo.SPFL_live_text', 
                        season, 
                        'Premiership', 
                        yearGames)
}

championship_live_text <- NULL
if(nchamp > 0) {
championship_live_text <-
  createSkyLiveTextDf(SPFL_Champ[1:nchamp,], 
                      'SPFL.dbo.SPFL_live_text', 
                      season, 
                      'Championship', 
                      yearGames)
}

league_1_live_text <- NULL
if(nl1 > 0) {
league_1_live_text <-
  createSkyLiveTextDf(SPFL_L1[1:nl1,], 
                      'SPFL.dbo.SPFL_live_text', 
                      season, 
                      'League 1', 
                      yearGames)
}

league_2_live_text <- NULL
if(nl2 > 0) {
league_2_live_text <-
  createSkyLiveTextDf(SPFL_L2[1:nl2,], 
                      'SPFL.dbo.SPFL_live_text', 
                      season, 
                      'League 2', 
                      yearGames)
}

# load rsd ####
currentDf <- readRDS(file.path(getwd(), "data", "spfl_live_text.RDS")) %>% 
  dplyr::mutate(season = as.character(season),
                competition = as.character(competition),
                home = as.character(home),
                away = as.character(away),
                link = as.character(link),
                live_text = as.character(live_text),
                matchDate = sky_asDate(matchDate)
                )

updated_live_text_df <-
  dplyr::bind_rows(
    currentDf,
    prem_live_text,
    championship_live_text,
    league_1_live_text,
    league_2_live_text
  )

if(exists("bbc_live_text_df")) {
  updated_live_text_df <- dplyr::bind_rows(updated_live_text_df, bbc_live_text_df)
}

team_map <- readRDS("data/team_map.RDS") %>% 
  dplyr::mutate(
    long_name = as.character(long_name),
    short_name = as.character(short_name)
  )

xG_BBC_chance <- readRDS(file.path(getwd(), "data", "xG_BBC_chance.RDS"))

gameweek_dates <- c("2020-01-26")
check <- get_gameweek_results(updated_live_text_df, xG_BBC_chance, team_map, gameweek_dates)
View(check)

#save if things all look fine
saveRDS(updated_live_text_df,
        file.path(getwd(), "data", "spfl_live_text.RDS"))

