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



prem_live_text <-
  createSkyLiveTextDf(SPFL_Prem[1:6,], 
                      'SPFL.dbo.SPFL_live_text', 
                      season, 
                      'Premiership', 
                      yearGames)

championship_live_text <-
  createSkyLiveTextDf(SPFL_Champ[1:5,], 
                      'SPFL.dbo.SPFL_live_text', 
                      season, 
                      'Championship', 
                      yearGames)

league_1_live_text <-
  createSkyLiveTextDf(SPFL_L1[1:5,], 
                      'SPFL.dbo.SPFL_live_text', 
                      season, 
                      'League 1', 
                      yearGames)

league_2_live_text <-
  createSkyLiveTextDf(SPFL_L2[1:5,], 
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
    league_2_live_text
    #,bbc_live_text_df
  )

new_Season <- 
  dplyr::filter(updated_live_text_df,
                season == "2019-2020") 


xG_BBC_chance <- readRDS(file.path(getwd(), "data", "xG_BBC_chance.RDS"))

xG_df <- 
  purrr::map_dfr(seq_len(nrow(new_Season)), 
                 function(x) {
                   live_text_row <- new_Season[["live_text"]][x]
                   i <- 
                     stringr::str_which(live_text_row, 
                                        as.character(xG_BBC_chance$chance_desc))
                   if(length(i) > 1) {
                     stop("multiple matches of live text to chance type")
                   }

                   xG <-
                     if(length(i) != 0) {
                     xG_BBC_chance$xG_BT[i]
                     } else {
                       0
                     }
                   xG_team <- 
                     if(length(i) != 0) {
                       firstOpenBracket <- 
                         stringi::stri_locate_first_fixed(live_text_row, 
                                                    pattern = "(")[1]
                       firstCloseBracket <- 
                         stringi::stri_locate_first_fixed(live_text_row, ")")[1]
                       
                       stringi::stri_sub(live_text_row, 
                                         from = firstOpenBracket+1, 
                                         to = firstCloseBracket-1)
                     } else {
                       NA
                     }
                   
                   dplyr::tibble(
                     xG = xG,
                     team_xG = xG_team
                   )
                 })

team_map <- readRDS("data/team_map.RDS") %>% 
  dplyr::mutate(
    long_name = as.character(long_name),
    short_name = as.character(short_name)
  )

live_text_xG_DF <- 
  dplyr::bind_cols(
    new_Season,
    xG_df
  ) %>% 
  dplyr::filter(
    !is.na(.data$team_xG)
  ) %>% 
  dplyr::mutate(
    matchDate = sky_asDate(.data$matchDate)
  ) %>% 
  group_by(
    season, competition, home, away, team_xG, matchDate
  ) %>% 
  summarise(total_xG = sum(xG)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(
    x = .,
    y = team_map,
    by = c("home" = "short_name")
  ) %>% 
  dplyr::left_join(
    x = .,
    y = team_map,
    by = c("away" = "short_name")
  ) %>% 
  dplyr::mutate(
    home = ifelse(is.na(.data$`long_name.x`), .data$`home`, .data$`long_name.x`),
    away = ifelse(is.na(.data$`long_name.y`), .data$`away`, .data$`long_name.y`)
  ) %>% 
  dplyr::select(-long_name.x, -long_name.y) %>% 
  dplyr::mutate(
    home = stringr::str_replace(.data$`home`, "St. ", "St "),
    away = stringr::str_replace(.data$`away`, "St. ", "St "),
    team_xG = stringr::str_replace(.data$`team_xG`, "St. ", "St ")
  )
               
matchDays <- c("2019-08-23", "2019-08-24", "2019-08-25")
gameWeekResults <- 
  live_text_xG_DF %>% 
    dplyr::filter(matchDate %in% matchDays) %>% 
    dplyr::inner_join(
      x = .,
      y = .,
      by = 
        c("home", 
         "away",
         "competition",
         "matchDate",
         "season"),
      suffix = c("_home", "_away")
    ) %>% 
  dplyr::filter(.data$home == .data$team_xG_home,
                .data$away == .data$team_xG_away) %>% 
  dplyr::select(-dplyr::starts_with("team_xG_"))

gameWeekResults %>% 
  View()

saveRDS(updated_live_text_df,
        file.path(getwd(), "data", "spfl_live_text.RDS"))

# cup games ----------------------------------------------------------------------------------

getSkyLiveText(league_cup, 'SPFL.dbo.SPFL_live_text', season, 'League Cup', yearGames)


filter(SPFL_Cup, grepl("http://www.skysports.com/football", Link, fixed = TRUE))[1:6,] %>%
  select(Link) %>%
  getSkyLiveText(., 'SPFL_live_text', '2017-2018', 'Scottish Cup', yearGames)
