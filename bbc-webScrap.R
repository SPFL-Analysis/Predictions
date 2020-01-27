source(file.path(getwd(), "R", "readBbcLiveText.R"))
library(RSelenium)
library(magrittr)
league <- "premiership" #premiership championship league-one league-two
year <- "2020"
month <- "01"
season <- "2019-2020"
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
    month
    ,
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

  cmd <- 'cd ~/Documents/Rselenium; java -Dwebdriver.chrome.driver=chromedriver -jar selenium-server-standalone-3.9.1.jar &'
system(cmd)

livetext <- 
  purrr:::map_dfr(links[c(1, 2)], ~readBbcLiveText(.x)) 

livetext_clean <- 
livetext %>%
  dplyr::mutate(home_team = stringr::str_remove_all(home_team, "'"),
                away_team = stringr::str_remove_all(away_team, "'"),
                live_text = stringr::str_remove_all(live_text, "'"))
  

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
