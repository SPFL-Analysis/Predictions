source(file.path(getwd(), "R", "readBbcLiveText.R"))
league <- "premiership" #premiership championship league-one league-two
year <- "2019"
month <- "05"
season <- "2018-2019"
nGames <- 6

bbc_leauge_links <- c("premiership", "championship", "league-one", "league-two")
comp <- c("Premiership", "Championship", "League 1", "League 2")
sql_league_name <- comp[match(league, bbc_leauge_links)]

bbc_url <-
  paste0(
    "https://www.bbc.co.uk/sport/football/scottish-",
    league ,
    "/scores-fixtures/",
    year,
    "-",
    month,
    "?filter=results"
  )

bbb_html <- xml2::read_html(bbc_url)

links <- 
  rvest::html_attr(
    rvest::html_nodes(
      bbb_html, 
      ".sp-c-fixture__block-link"), 
    "href")


links <- paste0("https://www.bbc.co.uk", links)[1:nGames]


# java -Dwebdriver.chrome.driver="chromedriver.exe" -jar selenium-server-standalone-3.9.1.jar
library(RSelenium)

livetext <- 
  purrr:::map_dfr(links, ~readBbcLiveText(.x)) 

livetext_clean <- 
livetext %>%
  dplyr::mutate(home_team = stringr::str_remove_all(home_team, "'"),
                away_team = stringr::str_remove_all(away_team, "'"),
                live_text = stringr::str_remove_all(live_text, "'"))
  

SQL_tbl <- "SPFL.dbo.SPFL_live_text"
channel <- RODBC::odbcConnect("SPFL")
for (j in 1:nrow(livetext_clean)) {
  qry <- paste("insert into ", SQL_tbl, " select '"
               , season, "', '"
               , sql_league_name, "', '"
               , livetext_clean$home_team[j], "', '"
               , livetext_clean$away_team[j], "', '"
               , livetext_clean$link[j], "', '"
               , livetext_clean$live_text[j], "', "
               , as.numeric(j), ", '"
               , livetext_clean$match_date[j], "'"
               , sep="")
  RODBC::sqlQuery(channel, qry)
}
RODBC::odbcCloseAll()


# dataframe
bbc_live_text_df <- 
  purrr::map_dfr(
    seq_along(1:nrow(livetext_clean)),
    function(j) {
      data.frame(
        season = season,
        competition = sql_league_name,
        home = livetext_clean$home_team[j],
        away = livetext_clean$away_team[j],
        link = livetext_clean$link[j],
        live_text = livetext_clean$live_text[j],
        rowID = as.numeric(j),
        matchDate = livetext_clean$match_date[j],
        stringsAsFactors = FALSE
      )
    }
  )
