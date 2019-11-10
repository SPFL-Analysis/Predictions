get_gameweek_results <- function(df, xG_map_df, team_map_df, gameweek_dates = NULL) {
  
  gameweek_results_df <- df %>% 
    dplyr::mutate(matchDate = sky_asDate(.data$matchDate))
    
  if (length(gameweek_dates) != 0) {
    gameweek_results_df <- 
      dplyr::filter(gameweek_results_df, matchDate %in% gameweek_dates)
  }
  
  if(nrow(gameweek_results_df) == 0) stop("no games for entered gameweek_dates", call. = FALSE)
  
  gameweek_results_df <-
    gameweek_results_df %>% 
    dplyr::mutate(
      home = get_long_name(.data$home, team_map_df$long_name, team_map_df$short_name),
      away = get_long_name(.data$away, team_map_df$long_name, team_map_df$short_name)
    )

  xG_df <- get_xG_df(gameweek_results_df, xG_map_df, team_map_df)
  
  dplyr::bind_cols(
    gameweek_results_df,
    xG_df
  ) %>%
    dplyr::filter(
      !is.na(.data$team_xG)
    ) %>%
    dplyr::mutate(
      matchDate = sky_asDate(.data$matchDate),
      team_xG = get_long_name(.data$team_xG, team_map_df$long_name, team_map_df$short_name)
    ) %>%
    dplyr::group_by(
      season, competition, home, away, team_xG, matchDate
    ) %>%
    summarise(
      total_xG = sum(xG),
      goals = sum(goal)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(
      x = .,
      y = .,
      by =
        c(
          "home",
          "away",
          "competition",
          "matchDate",
          "season"
        ),
      suffix = c("_home", "_away")
    ) %>%
    dplyr::filter(
      .data$home == .data$team_xG_home,
      .data$away == .data$team_xG_away
    ) %>%
    dplyr::select(-dplyr::starts_with("team_xG_"))
}


get_xG_df <- function(df, xG_map_df, team_map_df) {
  purrr::map_dfr(
    seq_len(nrow(df)),
    function(x) {
      live_text_row <- df[["live_text"]][x]
      homeTeam <- df[["home"]][x]
      away_team <- df[["away"]][x]
      i <-
        stringr::str_which(
          live_text_row,
          as.character(xG_map_df$chance_desc)
        )
      if (length(i) > 1) {
        stop("multiple matches of live text to chance type")
      }

      xG <-
        if (length(i) != 0) {
          xG_map_df$xG_BT[i]
        } else {
          0
        }
      xG_team <-
        if (length(i) != 0) {
          firstOpenBracket <-
            stringi::stri_locate_first_fixed(live_text_row,
              pattern = "("
            )[1]
          firstCloseBracket <-
            stringi::stri_locate_first_fixed(live_text_row, ")")[1]

          stringi::stri_sub(live_text_row,
            from = firstOpenBracket + 1,
            to = firstCloseBracket - 1
          )
        } else {
          NA
        }
      goal <-
        if (!is.na(live_text_row) &&
          any(
            startsWith(
              stringr::str_to_lower(live_text_row),
              "goal!"
            ),
            startsWith(
              stringr::str_to_lower(live_text_row),
              "own goal"
            )
          )
        ) {
          1
        } else {
          0
        }
      # update team if its an own goal
      if (!is.na(live_text_row) &&
        startsWith(stringr::str_to_lower(live_text_row), "own goal")) {
        firstComma <-
          stringi::stri_locate_first_fixed(live_text_row,
            pattern = ","
          )[1]
        firstDot <-
          stringi::stri_locate_first_fixed(live_text_row, ".")[1]
        own_goal_by <-
          trimws(stringi::stri_sub(live_text_row,
            from = firstComma + 1,
            to = firstDot - 1
          ))
        xG_team <-
          if (own_goal_by == homeTeam) {
            away_team
          } else {
            homeTeam
          }
        xG_team <- get_long_name(xG_team, team_map_df$long_name, team_map_df$short_name)
      }
      dplyr::tibble(
        xG = xG,
        team_xG = xG_team,
        goal = goal
      )
    }
  )
}