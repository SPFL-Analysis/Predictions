

library(asianOddsAPI)
library(RODBC)

authConn <-
  asianOddsAPI::authenticateConnection(
    username = "",
    password = ""
  )

season <- "2018-2019"
sportsType <- asianOddsAPI::getSportsId(url = authConn$url, token = authConn$token)
marketTypeId <- 2
oddsFormat <- "00"

leaguesId <-
  asianOddsAPI::getLeagueIds(authConn$url, authConn$token,
    sportsType, marketTypeId,
    leaguesString = "SCOTLAND"
  )

leagueList <-
    asianOddsAPI::getFeeds(
      authConn$url, authConn$token,
      paste(leaguesId$leagueId, collapse = ", "),
      sportsType, marketTypeId,
      oddsFormat = oddsFormat
      )

FT1X2 <- asianOddsAPI::getLeagueOdds(leagueList, oddsMarket = "FullTimeOneXTwo")
FT1X2_SQL <- 
  tidyr::spread(dplyr::select(FT1X2, -bookie), market, odds) %>%
  dplyr::mutate(leagueName = stringr::str_remove_all(leagueName, "SCOTLAND "))
writeFT1X2OddsSQL(season, Sys.Date(), FT1X2_SQL)
