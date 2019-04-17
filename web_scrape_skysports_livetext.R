#####load the librarys
library(tidyverse)
library(rvest)
library(stringr)
library(stringi)
library(RODBC)
library(data.table)
library(dplyr)

source("functions.R")
yearGames <- '2019'
season <- '2018-2019'

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



SPFL_Prem[1:6,] %>%
  getSkyLiveText(., 'SPFL.dbo.SPFL_live_text', season, 'Premiership', yearGames)

SPFL_Champ[1:5,] %>%
  getSkyLiveText(., 'SPFL.dbo.SPFL_live_text', season, 'Championship', yearGames)

SPFL_L1[1:5,] %>%
  getSkyLiveText(., 'SPFL.dbo.SPFL_live_text', season, 'League 1', yearGames)

SPFL_L2[1:5,] %>%
  getSkyLiveText(., 'SPFL.dbo.SPFL_live_text', season, 'League 2', yearGames)


# cup games ----------------------------------------------------------------------------------

getSkyLiveText(league_cup, 'SPFL.dbo.SPFL_live_text', season, 'League Cup', yearGames)


filter(SPFL_Cup, grepl("http://www.skysports.com/football", Link, fixed = TRUE))[1:6,] %>%
  select(Link) %>%
  getSkyLiveText(., 'SPFL_live_text', '2017-2018', 'Scottish Cup', yearGames)
