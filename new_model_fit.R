library(purrr)
library(rvest)
library(stringr)
library(stringi)
library(data.table)
library(dplyr)
library(ggplot2)

source("functions.R")
purrr::walk(list.files("R", full.names = T), ~ source(.))

raw_livetext <- readRDS(file.path(getwd(), "data", "spfl_live_text.RDS"))
team_map <- readRDS("data/team_map.RDS")
xG_BBC_chance <- readRDS(file.path(getwd(), "data", "xG_BBC_chance.RDS"))

matchDates <- NULL #as.Date(c('2020-01-29', '2020-01-25', '2020-01-26'))

seasons <- c("2018-2019", "2019-2020")
df <- dplyr::filter(raw_livetext, .data$season %in% seasons)
xG_data <- get_gameweek_results(df, xG_BBC_chance, team_map)
GLM_Data <-
  dplyr::bind_rows(
    dplyr::transmute(
      xG_data,
      competition,
      xG = .data$total_xG_home,
      team = .data$home,
      opp = .data$away,
      home = 1,
      matchDate,
      goals = .data$goals_home
    ),
    dplyr::transmute(
      xG_data,
      competition,
      xG = .data$total_xG_away,
      team = .data$away,
      opp = .data$home,
      home = 0,
      matchDate,
      goals = .data$goals_away
    )
  )

if (length(matchDates) == 0) {
  matchDates <- unique(FT1X2_SQL[["matchDate"]])
}
all_gameweek_matches <- FT1X2_SQL %>%
  dplyr::filter(.data$matchDate %in% matchDates) %>% 
  dplyr::mutate(
    clean_homeTeam = get_long_name(.data$homeTeam, team_map$long_name, team_map$short_name),
    clean_awayTeam = get_long_name(.data$awayTeam, team_map$long_name, team_map$short_name)
  )

leagues <-
  c(
    "PREMIERSHIP", "CHAMPIONSHIP", "LEAGUE ONE", "LEAGUE TWO",
    "LEAGUE 1", "LEAGUE 2"
  )
matchDates <-
  dplyr::select(all_gameweek_matches, leagueName, matchDate) %>%
  dplyr::filter(leagueName %in% leagues) %>%
  dplyr::group_by(leagueName, matchDate) %>%
  dplyr::distinct() %>%
  tidyr::nest(data = c(matchDate))

test <-
  purrr::pmap_dfr(
    list(
      matchDates$leagueName,
      matchDates$data
    ),
    function(x, y) {
      league_GLM_Data <-
        GLM_Data %>%
        dplyr::mutate(lc_comp = stringr::str_to_lower(.data$competition)) %>%
        dplyr::filter(.data$lc_comp == stringr::str_to_lower(x))

      models_xG <- fitGlmXg(league_GLM_Data)
      models_goals <- fitGlmGoals(league_GLM_Data)

      gameweekMatches <- all_gameweek_matches %>%
        dplyr::mutate(lc_comp = stringr::str_to_lower(.data$leagueName)) %>%
        dplyr::filter(.data$lc_comp == stringr::str_to_lower(x))

      if (any(is.na(gameweekMatches[["clean_homeTeam"]]))) stop("check home team names", call. = FALSE)
      if (any(is.na(gameweekMatches[["clean_awayTeam"]]))) stop("check away team names", call. = FALSE)

      purrr::map_dfr(
        y$matchDate, function(.matchDate) {
          gameDayMatches <- dplyr::filter(gameweekMatches, .data$matchDate == .matchDate)
          purrr::map_dfr(seq(nrow(gameDayMatches)), function(i) {
            predOdds <-
              match_odds(
                models_xG,
                gameDayMatches[[i, "clean_homeTeam"]],
                gameDayMatches[[i, "clean_awayTeam"]]
              )
            dplyr::bind_cols(gameDayMatches[i, ], predOdds)
          })
        }
      )
    }
  )

bets <-
  test %>%
  dplyr::mutate(
    mktPrice_home = ifelse(.data$homeBet == "Home -0.5", .data$home, 1 / (1 - 1 / as.numeric(.data$away))),
    mktPrice_away = ifelse(.data$awayBet == "Away -0.5", .data$away, 1 / (1 - 1 / as.numeric(.data$home)))
  ) %>%
  dplyr::select(
    "leagueName",
    "clean_homeTeam",
    "clean_awayTeam",
    "homeBet",
    "awayBet",
    "mktPrice_home",
    "mktPrice_away",
    "modPrice_home",
    "modPrice_away"
  ) %>%
  tidyr::pivot_longer(
    cols = c("homeBet", "awayBet"),
    names_to = "oldBets",
    values_to = "bet"
  ) %>%
  tidyr::pivot_longer(
    cols = c("mktPrice_away", "mktPrice_home"),
    names_to = "oldMktPrice",
    values_to = "mktPrice"
  ) %>%
  tidyr::pivot_longer(
    cols = c("modPrice_away", "modPrice_home"),
    names_to = "oldModPrice",
    values_to = "modPrice"
  ) %>%
  dplyr::filter((.data$oldBets == "homeBet" &
    .data$oldMktPrice == "mktPrice_home" &
    .data$oldModPrice == "modPrice_home") |
    (.data$oldBets == "awayBet" &
      .data$oldMktPrice == "mktPrice_away" &
      .data$oldModPrice == "modPrice_away")) %>%
  dplyr::select(-dplyr::starts_with("old")) %>%
  dplyr::mutate(
    mktPrice = as.numeric(.data$mktPrice),
    modPrice = as.numeric(.data$modPrice),
    margin = (as.numeric(.data$modPrice) / as.numeric(.data$mktPrice)) - 1,
    bet_label = paste0(.data$clean_homeTeam, " v ", .data$clean_awayTeam, "\n", .data$bet),
  ) %>%
  dplyr::filter(.data$margin >= 0.2)   %>% 
  dplyr::arrange(.data$margin) %>% 
  dplyr::mutate(
    bet_label = forcats::as_factor(.data$bet_label)
  )

interactivePlot <-
  ggplot2::ggplot(bets) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = bet_label, xend = bet_label,
      y = modPrice, yend = mktPrice
    )
  ) +
  ggiraph::geom_point_interactive(
    ggplot2::aes(
      x = bet_label,
      y = modPrice,
      tooltip = paste0("Model Price:", round(modPrice, 2)),
      data_id = paste0("Model Price:", round(modPrice, 2))
    ),
    colour = "grey",
    size = 3
  ) +
  ggiraph::geom_point_interactive(
    ggplot2::aes(
      x = bet_label,
      y = mktPrice,
      tooltip = paste0("Market Price:", round(mktPrice, 2)),
      data_id = paste0("Market Price:", round(mktPrice, 2))
    ),
    colour = "blue",
    size = 3
  ) +
  ggplot2::annotate(
    geom = "text",
    x = bets$bet_label,
    y = 0.5,
    label = paste0("Margin:", round(100 * bets$margin, 0), "%"),
    size = 2
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(1.5, 2.0, 2.5, 3.0),
    limits = c(0, NA)
  ) +
  ggplot2::labs(
    y = "Fractional Odds",
    title = "SPFL Value Odds"
  ) +
  #ggplot2::coord_cartesian(clip = FALSE) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 8)
  ) +
  ggplot2::coord_flip()

ggiraph::girafe(ggobj = interactivePlot)
