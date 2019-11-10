username <- getDetails()[1]
password <- getDetails()[2]

library(asianOddsAPI)
library(magrittr)

authConn <-
  asianOddsAPI::authenticateConnection(
    url = "https://webapi.asianodds88.com/AsianOddsService/Login?",
    username,
    password
  )

accInfo <- getAccountSummary(authConn$url, authConn$token)
View(accInfo)

sportsType <-
  asianOddsAPI::getSportsId(
    url = authConn$url,
    token = authConn$token,
    sport = "Football"
  )
marketTypeId <- 2
oddsFormat <- "00"

leaguesId <-
  asianOddsAPI::getLeagueIds(authConn$url,
    authConn$token,
    sportsType,
    marketTypeId,
    leaguesString = "SCOTLAND"
  )

leagueList <-
  asianOddsAPI::getFeeds(
    authConn$url, authConn$token,
    paste(leaguesId$leagueId, collapse = ", "),
    sportsType,
    marketTypeId,
    oddsFormat = oddsFormat
  )

FT1X2 <-
  asianOddsAPI::getLeagueOdds(leagueList,
    oddsMarket = "FullTimeOneXTwo"
  )

FTHdp <-
  asianOddsAPI::getLeagueOdds(leagueList,
    oddsMarket = "FullTimeHdp"
  )

FT_ou <-
  asianOddsAPI::getLeagueOdds(leagueList,
                              oddsMarket = "FullTimeOu"
  )



# gameType 	Type of game where the bets will place.
# H is for HDP
# O is for OverUnder
# X is for 1X2 game
# oddsName 	Where the bets will be placed. Here are the valid values :
#   If GameType is H then :
#   AwayOdds -Value of the AwayOdds from Feed under HDP
#   HomeOdds -Value of Home Odds from Feed under HDP
# If GameType is O then
# verOdds -Value of OverOdds from Feed
# UnderOdds -Value of UnderOdds from Feed
# If GameType is X then
# AwayOdds -Value of AwayOdds from Feed under OneXTwos
# DrawOdds -Value of DrawOdds from Feed under OneXTwos
# HomeOdds -Value of HomeOdds from Feed under OneXTwos

gameId <- -1368722104
OddsName <- "HomeOdds"
bookie <- "IBC"
odds <- 1.850
gametype <-"H"
response <-
  httr::POST(
    url = paste0(authConn$url, "/GetPlacementInfo"),
    body = list(
      GameId = gameId,
      GameType = gametype,
      IsFullTime = 1,
      Bookies = "IBC",
      MarketTypeId = marketTypeId,
      OddsFormat = "00",
      OddsName = OddsName,
      SportsType = sportsType
    ),
    httr::add_headers(
      AOToken = authConn$token,
      Accept = "application/json"
    ),
    encode = "json"
  ) %>%
  httr::content("parsed")

bets <-
  httr::POST(
    url = paste0(authConn$url, "/PlaceBet"),
    body = list(
      GameId = gameId,
      GameType = gametype,
      IsFullTime = 1,
      BookieOdds = paste0(bookie,":",odds),
      MarketTypeId = marketTypeId,
      OddsFormat = "00",
      OddsName = OddsName,
      SportsType = sportsType,
      Amount = 50
    ),
    httr::add_headers(
      AOToken = authConn$token,
      Accept = "application/json"
    ),
    encode = "json"
  ) %>%
  httr::content("parsed")

View(bets)

View(getAccountSummary(authConn$url, authConn$token))
