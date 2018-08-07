#####load the librarys
library(tidyverse)
library(rvest)
library(stringr)
library(stringi)
library(RODBC)
library(data.table)
library(dplyr)


#############################################################################################
# clean up the links data table
#############################################################################################


SPFL_Prem <- 
  getLinks("http://www.skysports.com/scottish-premier-results",
           "http://www.skysports.com/football/")

SPFL_Champ <- 
  getLinks("http://www.skysports.com/football/competitions/scottish-championship/results",
           "http://www.skysports.com/football/")

SPFL_L1 <-  
  getLinks("http://www.skysports.com/football/competitions/scottish-league-one/results",
           "http://www.skysports.com/football/")

SPFL_L2 <- 
  getLinks("http://www.skysports.com/football/competitions/scottish-league-two/results",
           "http://www.skysports.com/football/")

yearGames <- '2018'
season <- '2018-2019'

SPFL_Prem[1:6,] %>%
  getSkyLiveText(., 'SPFL.dbo.SPFL_live_text', season, 'Premiership', yearGames)

SPFL_Champ[1:5,] %>%
  getSkyLiveText(., 'SPFL.dbo.SPFL_live_text', season, 'Championship', yearGames)

SPFL_L1[1:5,] %>%
  getSkyLiveText(., 'SPFL.dbo.SPFL_live_text', season, 'League 1', yearGames)

SPFL_L2[1:5,] %>%
  getSkyLiveText(., 'SPFL.dbo.SPFL_live_text', season, 'League 2', yearGames)


# cup games ----------------------------------------------------------------------------------

filter(SPFL_Cup, grepl("http://www.skysports.com/football", Link, fixed = TRUE))[1:6,] %>%
  select(Link) %>%
  getSkyLiveText(., 'SPFL_live_text', '2017-2018', 'Scottish Cup', yearGames)
