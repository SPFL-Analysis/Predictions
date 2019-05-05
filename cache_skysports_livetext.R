#####load the librarys
library(tidyverse)
library(rvest)
library(stringr)
library(stringi)
library(RODBC)
library(data.table)
library(dplyr)

source("functions.R")
purrr::walk(list.files("R", full.names = T), ~ source(.))
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



prem_live_text <-
  createSkyLiveTextDf(SPFL_Prem[1:12,], 
                      'SPFL.dbo.SPFL_live_text', 
                      season, 
                      'Premiership', 
                      yearGames)

championship_live_text <-
  createSkyLiveTextDf(SPFL_Champ[1:10,], 
                      'SPFL.dbo.SPFL_live_text', 
                      season, 
                      'Championship', 
                      yearGames)

league_1_live_text <-
  createSkyLiveTextDf(SPFL_L1[1:10,], 
                      'SPFL.dbo.SPFL_live_text', 
                      season, 
                      'League 1', 
                      yearGames)

league_2_live_text <-
  createSkyLiveTextDf(SPFL_L2[1:10,], 
                      'SPFL.dbo.SPFL_live_text', 
                      season, 
                      'League 2', 
                      yearGames)

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
    league_2_live_text,
    bbc_live_text_df
  )

saveRDS(updated_live_text_df,
        file.path(getwd(), "data", "spfl_live_text.RDS"))

# cup games ----------------------------------------------------------------------------------

getSkyLiveText(league_cup, 'SPFL.dbo.SPFL_live_text', season, 'League Cup', yearGames)


filter(SPFL_Cup, grepl("http://www.skysports.com/football", Link, fixed = TRUE))[1:6,] %>%
  select(Link) %>%
  getSkyLiveText(., 'SPFL_live_text', '2017-2018', 'Scottish Cup', yearGames)
