library(magrittr)
source('/home/graememcinroy/github/Predictions/R/getDetails.R')

authConn <-
  asianOddsAPI::authenticateConnection(
    username = getDetails()[1],
    password = getDetails()[2]
  )

sportsType <- asianOddsAPI::getSportsId(url = authConn$url, token = authConn$token)
marketTypeId <- 2
oddsFormat <- "00"

leaguesId <-
  asianOddsAPI::getLeagueIds(authConn$url, authConn$token,
    sportsType, marketTypeId,
    leaguesString = "SCOTLAND"
  )

Sys.sleep(11)

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
  dplyr::mutate(leagueName = stringr::str_remove_all(leagueName, "SCOTLAND ")) %>%
  dplyr::select(leagueName, homeTeam, awayTeam, matchDate, home, draw, away)