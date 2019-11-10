
# fit GLM xG model --------------------------------
fitGlmXg <- function(GLM_Data) {
  # data clean
  GLM_Data$matchDate <- as.Date(GLM_Data$matchDate)
  # data clean up where xG is zero
  GLM_Data <- mutate(GLM_Data, xG = ifelse(xG == 0, 0.01, xG))

  #####################################################################################################
  # add in time weighting column
  ####################################################################################################

  TW_param <- -0.01
  date_to <- max(as.Date(GLM_Data$matchDate, "%d/%m/%y"), na.rm = FALSE)
  GLM_Data$time_weight <- as.numeric(date_to - GLM_Data$matchDate)
  GLM_Data$time_weight <- exp(TW_param * GLM_Data$time_weight)

  #####################################################################################################
  # loop round all match dates fitting the model to prior results
  # then loop round all games on that match date predicting the odds
  # recursively store the predictions
  #####################################################################################################

  model_xG <- glm(xG ~ team + opp + home
    ,
    data = GLM_Data
    , family = Gamma() # Change the distribution
    , weights = GLM_Data$time_weight
  ) # add & remove weights as required
  return(model_xG)
}

# fit GLM xG model --------------------------------
fitGlmGoals <- function(GLM_Data) {
  # data clean
  GLM_Data$matchDate <- as.Date(GLM_Data$matchDate)
  # data clean up where xG is zero
  GLM_Data <- mutate(GLM_Data, xG = ifelse(xG == 0, 0.01, xG))

  #####################################################################################################
  # add in time weighting column
  ####################################################################################################

  TW_param <- -0.01
  date_to <- max(as.Date(GLM_Data$matchDate, "%d/%m/%y"), na.rm = FALSE)
  GLM_Data$time_weight <- as.numeric(date_to - GLM_Data$matchDate)
  GLM_Data$time_weight <- exp(TW_param * GLM_Data$time_weight)

  #####################################################################################################
  # loop round all match dates fitting the model to prior results
  # then loop round all games on that match date predicting the odds
  # recursively store the predictions
  #####################################################################################################

  model_goals <- glm(goals ~ team + opp + home
    ,
    data = GLM_Data
    , family = poisson() # Change the distribution
    , weights = GLM_Data$time_weight
  ) # add & remove weights as required
  return(model_goals)
}



tau <- Vectorize(function(xx, yy, lambda, mu, rho) {
  if (xx == 0 & yy == 0) {
    return(1 - (lambda * mu * rho))
  } else if (xx == 0 & yy == 1) {
    return(1 + (lambda * rho))
  } else if (xx == 1 & yy == 0) {
    return(1 + (mu * rho))
  } else if (xx == 1 & yy == 1) {
    return(1 - rho)
  } else {
    return(1)
  }
})

# fit DC model ----------------------------
fitDCmodel <- function(data_) {

  ################################################################
  ## taken from http://opisthokonta.net/?p=890
  #####################################################################
  ## Dixon COles adjustment fuction

  tau <- Vectorize(function(xx, yy, lambda, mu, rho) {
    if (xx == 0 & yy == 0) {
      return(1 - (lambda * mu * rho))
    } else if (xx == 0 & yy == 1) {
      return(1 + (lambda * rho))
    } else if (xx == 1 & yy == 0) {
      return(1 + (mu * rho))
    } else if (xx == 1 & yy == 1) {
      return(1 - rho)
    } else {
      return(1)
    }
  })

  #####################################################################
  ## Log likelihood function
  #####################################################################

  logLik <- function(y1, y2, lambda, mu, rho = 0) {
    # rho=0, independence
    # y1: home goals
    # y2: away goals

    sum(log(dpois(y1, lambda)), log(dpois(y2, mu)))
  }


  DClogLik <- function(y1, y2, lambda, mu, rho = 0) {
    # rho=0, independence
    # y1: home goals
    # y2: away goals

    sum(log(tau(y1, y2, lambda, mu, rho)), log(dpois(y1, lambda)), log(dpois(y2, mu)))
  }

  DClogLik_tw <- function(y1, y2, lambda, mu, rho = 0) {
    # rho=0, independence
    # y1: home goals
    # y2: away goals

    time.weight <- data_$Time_weight

    sum(time.weight * log(tau(y1, y2, lambda, mu, rho)), time.weight * log(dpois(y1, lambda)), time.weight * log(dpois(y2, mu)))
  }


  #####################################################################
  ## Data prep function
  #####################################################################
  DCmodelData <- function(df) {
    hm <- model.matrix(~HomeTeam - 1, data = df)
    am <- model.matrix(~AwayTeam - 1, data = df)

    team.names <- unique(c(levels(df$HomeTeam), levels(df$AwayTeam)))

    return(list(
      homeTeamDM = hm,
      awayTeamDM = am,
      homeGoals = df$FTHG,
      awayGoals = df$FTAG,
      teams = team.names
    ))
  }


  #####################################################################
  ## maximum likelihood fucntion
  #####################################################################

  DCoptimFn <- function(params, DCm) {
    home.p <- params[1]
    rho.p <- params[2]

    nteams <- length(DCm$teams)
    attack.p <- matrix(params[3:(nteams + 2)], ncol = 1)
    defence.p <- matrix(params[(nteams + 3):length(params)], ncol = 1)

    lambda <- exp(DCm$homeTeamDM %*% attack.p + DCm$awayTeamDM %*% defence.p + home.p)
    mu <- exp(DCm$awayTeamDM %*% attack.p + DCm$homeTeamDM %*% defence.p)

    return(
      DClogLik(y1 = DCm$homeGoals, y2 = DCm$awayGoals, lambda, mu, rho.p) * -1
    )
  }

  #########################################################################
  ### time weighted function
  #########################################################################

  DCoptimFn_tw <- function(params, DCm) {
    home.p <- params[1]
    rho.p <- params[2]

    nteams <- length(DCm$teams)
    attack.p <- matrix(params[3:(nteams + 2)], ncol = 1)
    defence.p <- matrix(params[(nteams + 3):length(params)], ncol = 1)

    lambda <- exp(DCm$homeTeamDM %*% attack.p + DCm$awayTeamDM %*% defence.p + home.p)
    mu <- exp(DCm$awayTeamDM %*% attack.p + DCm$homeTeamDM %*% defence.p)

    return(
      DClogLik_tw(y1 = DCm$homeGoals, y2 = DCm$awayGoals, lambda, mu, rho.p) * -1
    )
  }

  #########################################################################
  # optimise without DC adjustment
  #########################################################################

  optimFn <- function(params, DCm) {
    home.p <- params[1]
    rho.p <- params[2]

    nteams <- length(DCm$teams)
    attack.p <- matrix(params[3:(nteams + 2)], ncol = 1)
    defence.p <- matrix(params[(nteams + 3):length(params)], ncol = 1)

    lambda <- exp(DCm$homeTeamDM %*% attack.p + DCm$awayTeamDM %*% defence.p + home.p)
    mu <- exp(DCm$awayTeamDM %*% attack.p + DCm$homeTeamDM %*% defence.p)

    return(
      logLik(y1 = DCm$homeGoals, y2 = DCm$awayGoals, lambda, mu, rho.p) * -1
    )
  }

  #####################################################################
  ## normalise attack parameter sum
  #####################################################################
  DCattackConstr <- function(params, DCm, ...) {
    nteams <- length(DCm$teams)
    attack.p <- matrix(params[3:(nteams + 2)], ncol = 1)
    return((sum(attack.p) / nteams) - 1)
  }

  ##########################################################################
  # create a time weighting function using the DCM fixed paramater 0.0065
  # we use 0.0325 since DC used half weeks and we are using days
  ##########################################################################
  data_$Date <- as.Date(data_$matchDate)
  Time_weight <- -0.00325
  max_date <- max(as.Date(data_$Date, "%d/%m/%y"), na.rm = FALSE)
  datediffs <- max_date - as.Date(data_$Date, "%d/%m/%y")
  data_$Time_weight <- 1 # as.numeric(max_date - as.Date(data_$Date, "%d/%m/%y"))
  data_$Time_weight <- 1 # exp(Time_weight*data_$Time_weight)

  #####################################################################
  # initial parameter estimates
  #####################################################################
  dcm <- DCmodelData(data_)
  dta <- data_

  attack.params <- rep(0.01, times = nlevels(dta$HomeTeam))
  defence.params <- rep(-0.08, times = nlevels(dta$HomeTeam))
  home.param <- 0.06
  rho.init <- 0.03
  par.inits <- c(home.param, rho.init, attack.params, defence.params)
  # it is also usefull to give the parameters some informative names
  names(par.inits) <- c("HOME", "RHO", paste("Attack", dcm$teams, sep = "."), paste("Defence", dcm$teams, sep = "."))

  #########################################################################################################
  # fit the model
  ########################################################################################################

  model_DC <- alabama::auglag(
    par = par.inits, # starting point for optimiser
    fn = DCoptimFn, # function to optimise
    heq = DCattackConstr, # equality contratinst
    DCm = dcm
  ) # data from function
  return(model_DC)
  #########################################################################################################
  # end
  ########################################################################################################
}


#####################################################################################################
# function to return predicted match odds
####################################################################################################

  match_odds <- function(model_name, home, away) {
  home <- stringr::str_trim(home)
  away <- stringr::str_trim(away)
  lambda <- predict(model_name, data.frame(home = 1, team = home, opp = away), type = "response")
  mu <- predict(model_name, data.frame(home = 0, team = away, opp = home), type = "response")

  maxgoal <- 8
  probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))

  HomeP <- sum(probability_matrix[lower.tri(probability_matrix)])
  DrawP <- sum(diag(probability_matrix))
  AwayP <- sum(probability_matrix[upper.tri(probability_matrix)])

  homeBet <- if(HomeP > AwayP) {
    dplyr::tibble(
      homeBet = "Home -0.5",
      modPrice_home = 1.0 / HomeP
    )
  } else {
    dplyr::tibble(
      homeBet = "Home +0.5",
      modPrice_home = 1.0 / (1 - AwayP)
    )
  }
  awayBet <- if(AwayP > HomeP) {
    dplyr::tibble(
      awayBet = "Away -0.5",
      modPrice_away = 1.0 / AwayP
    )
  } else {
    dplyr::tibble(
      awayBet = "Away +0.5",
      modPrice_away = 1.0 / (1 - HomeP)
    )
  }

  dplyr::bind_cols(homeBet, awayBet)
}

###################################################################################

writeModelOddsSQL <- function(modelDesc, season, comp, date, df) {
  channel <- odbcConnect("SPFL")

  qry <- paste("insert into SPFL.dbo.model_odds select '"
    , modelDesc, "', '"
    , season, "', '"
    , comp, "', '"
    , date, "', '"
    , df[1, 1], "', '"
    , df[1, 2], "', '"
    , df[1, 3], "', "
    , df[1, 4], ", '"
    , df[1, 5], "'"
    ,
    sep = ""
  ) # create SQL qry

  sqlQuery(channel, qry) # run query

  close(channel)
}

###################################################################################
# write results to SQl --------------------
###################################################################################

writePredictionsSQL <- function(season, date, comp, model_xG, model_goals, model_DC) {
  GW_matches_qry <- paste("select * from SPFL.[dbo].[getFixtures]('", date, "','", comp, "')", sep = "")
  GW_matches <- data.frame(sqlQuery(channel, GW_matches_qry))


  for (i in seq_along(GW_matches$HomeTeam)) {
    df <- match_odds(model_xG, GW_matches[i, 1], GW_matches[i, 2])
    writeModelOddsSQL("GLM xG Gamma TW -0.01", season, comp, date, df)

    df1 <- match_odds(model_goals, GW_matches[i, 1], GW_matches[i, 2])
    writeModelOddsSQL("GLM goals Poisson TW -0.01", season, comp, date, df1)

    df2 <- match_odds_DC(model_DC, GW_matches[i, 1], GW_matches[i, 2])
    writeModelOddsSQL("DC model TW -0.0035", season, comp, date, df2)
  }
}

##########################################################
# function to write the matches and odds to the SQL table
############################################################

writeFT1X2OddsSQL <- function(season, date, FT1X2) {
  channel <- odbcConnect("SPFL")
  for (i in seq_len(nrow(FT1X2))) {
    qry <- paste("insert into SPFL.dbo.market_odds select '"
      , season, "', '"
      , FT1X2$leagueName[i], "', '"
      , FT1X2$matchDate[i], "', '"
      , FT1X2$homeTeam[i], "', '"
      , FT1X2$awayTeam[i], "', "
      , FT1X2$home[i], ", "
      , FT1X2$draw[i], ", "
      , FT1X2$away[i], ", '"
      , date, "', null"
      ,
      sep = ""
    ) # create SQL qry
    sqlQuery(channel, qry) # run query
  }
  close(channel)
}



################################################################################################
# function to scrape all the link from a site which match a pattern
################################################################################################

getLinks <- function(URL) {
  pg <- xml2::read_html(URL)
  tibble::tibble(
    Link =
      rvest::html_attr(
        rvest::html_nodes(
          pg,
          ".matches__link"
        ),
        "href"
      )
  )
}

#############################################################################################
# create function
# loop round all the links and get the live text from them
# insert the live text into a SQL tbl
#############################################################################################

createSkyLiveTextDf <- function(links, SQL_tbl, season, comp, year) {
  purrr::map_dfr(
    seq_along(1:nrow(links)),
    function(i) {
      locateStr <- stri_locate_last_fixed(toString(links[i, 1]), "/")
      splitStr <- as.numeric(locateStr[1, 1])
      string1 <- str_sub(toString(links[i, 1]), 1, splitStr)
      string2 <-
        str_sub(
          toString(links[i, 1]),
          splitStr,
          str_length(toString(links[i, 1]))
        )
      newlink <- paste(string1, "live", string2, sep = "")

      # scrape the live text from the link
      html <- read_html(newlink)
      livetextnodes <- html_nodes(html, ".live-text__text")
      livetext <- html_text(livetextnodes)

      # scrpae for the teams and the date
      teamNodes <- html_nodes(html, ".swap-text--bp10")
      teams <- html_text(teamNodes)
      home <-
        str_replace(
          str_trim(
            str_replace(
              teams[1],
              "\n                        ", ""
            ),
            side = "both"
          ),
          "'", ""
        )
      away <-
        str_replace(
          str_trim(
            str_replace(teams[2], "\n                        ", ""),
            side = "both"
          ),
          "'", ""
        )

      Datenodes <- html_nodes(html, ".match-header__detail-item:nth-child(2)")
      livetextDate <- html_text(Datenodes)
      matchDate <- str_replace(str_trim(str_replace(livetextDate[1], "\n                        ", ""), side = "both"), "'", "")
      matchDate <- paste(matchDate, year, sep = " ")

      # new loop to insert every row of live text
      if (length(livetext) > 0) {
        purrr::map_dfr(
          seq_along(1:length(livetext)),
          function(row_num) {
            newlivetext <-
              str_trim(str_replace(
                livetext[row_num]
                , "\n                        ", ""
              ),
              side = "left"
              )
            newlivetext <- str_replace_all(newlivetext, "'", "")
            data.frame(
              season = season,
              competition = comp,
              home = home,
              away = away,
              link = newlink,
              live_text = newlivetext,
              rowId = row_num,
              matchDate = matchDate,
              stringsAsFactors = FALSE
            )
          }
        )
      } else {
        
        warning(paste0(home, " v ", away,
                       " live text not available, please add using another method",
                       call. = FALSE)
        )
        
        data.frame(
          season = season,
          competition = comp,
          home = home,
          away = away,
          link = NA,
          live_text = NA,
          rowId = NA,
          matchDate = NA,
          stringsAsFactors = FALSE
        )
        # warning(paste0(home, " v ", away, 
        #                " live text not available, please add using another method",
        #                call. = FALSE)
        #)
      }
    }
  )
}


##################################################################################################

getSkyLiveText <- function(links, SQL_tbl, season, comp, year) {
  for (i in 1:nrow(links)) {
    locateStr <- stri_locate_last_fixed(toString(links[i, 1]), "/")
    # locate the last / in the link
    splitStr <- as.numeric(locateStr[1, 1])
    # stroe the index of this
    string1 <- str_sub(toString(links[i, 1]), 1, splitStr)
    # split the link in 2 from index 1 to the located index
    string2 <- str_sub(toString(links[i, 1]), splitStr, str_length(toString(links[i, 1]))) # second split from located index to the end
    newlink <- paste(string1, "live", string2, sep = "")
    # combine the 2 strings and add in the required 'live'

    # scrape the live text from the link
    html <- read_html(newlink)
    livetextnodes <- html_nodes(html, ".live-text__text")
    livetext <- html_text(livetextnodes)


    # scrpae for the teams and the date
    teamNodes <- html_nodes(html, ".swap-text--bp10")
    teams <- html_text(teamNodes)
    home <- str_replace(str_trim(str_replace(teams[1], "\n                        ", ""), side = "both"), "'", "")
    away <- str_replace(str_trim(str_replace(teams[2], "\n                        ", ""), side = "both"), "'", "")

    Datenodes <- html_nodes(html, ".match-header__detail-item:nth-child(2)")
    livetextDate <- html_text(Datenodes)
    matchDate <- str_replace(str_trim(str_replace(livetextDate[1], "\n                        ", ""), side = "both"), "'", "")
    matchDate <- paste(matchDate, year, sep = " ")

    # open SQl channel
    channel <- odbcConnect("SPFL")

    # new loop to insert every row of live text

    if (length(livetext) > 0) {
      for (j in 1:length(livetext)) {
        newlivetext <- str_trim(str_replace(livetext[j], "\n                        ", ""), side = "left") # tidy up the live text
        newlivetext <- str_replace_all(newlivetext, "'", "") # replace ' with blank for SQL insert
        qry <- paste("insert into ", SQL_tbl, " select '"
          , season, "', '"
          , comp, "', '"
          , home, "', '"
          , away, "', '"
          , newlink, "', '"
          , newlivetext, "', "
          , as.numeric(j), ", '"
          , matchDate, "'"
          ,
          sep = ""
        ) # create SQL qry

        sqlQuery(channel, qry) # run query
      }
    } else {
      qry <- paste("insert into ", SQL_tbl, " select '"
        , season, "', '"
        , comp, "', '"
        , home, "', '"
        , away, "', '"
        , newlink, "', 'No live text with the CSS available', 1"
        ,
        sep = ""
      ) # create SQL qry
      sqlQuery(channel, qry) # run query
    }

    close(channel)
  }
}

####################################

create_shotmap <- function(home, away, date) {
  channel <- odbcConnect("SPFL")

  qry <- paste("select * from dbo.getShotMapData('", home, "', '", away, "', '", date, "')", sep = "")
  df <- sqlQuery(channel, qry)

  odbcClose(channel)

  df$home <- as.character(df$home)
  df$away <- as.character(df$away)
  df$goal_value <- as.numeric(df$goal_value)
  home <- unique(df$home)
  away <- unique(df$away)
  homedata <- subset(df, team == home)
  awaydata <- subset(df, team == away)

  annotate_home <- paste(home, "xG =", sprintf("%.2f", round(sum(homedata$xG), 2)), "Goals =", sum(homedata$goal_value))
  annotate_away <- paste(away, "xG =", sprintf("%.2f", round(sum(awaydata$xG), 2)), "Goals =", sum(awaydata$goal_value))

  len <- 100
  height <- 100

  ggplot(data = df) +
    geom_rect(aes(xmin = 0, xmax = len, ymin = 0, ymax = height), fill = "green4", colour = "white", size = 1, alpha = 0.05) +
    geom_rect(aes(xmin = 0, xmax = len / 2, ymin = 0, ymax = height), fill = NA, colour = "white", size = 1) +
    geom_rect(aes(xmin = 0, xmax = 18, ymin = 23, ymax = height - 23), fill = NA, colour = "white", size = 1) +
    geom_rect(aes(xmin = len - 18, xmax = len, ymin = 23, ymax = height - 23), fill = NA, colour = "white", size = 1) +
    geom_rect(aes(xmin = 0, xmax = 6, ymin = 35, ymax = height - 35), fill = NA, colour = "white", size = 1) +
    geom_rect(aes(xmin = len - 6, xmax = len, ymin = 35, ymax = height - 35), fill = NA, colour = "white", size = 1) +
    geom_point(aes(x = len / 2, y = height / 2), fill = "white", colour = "white", shape = 21, size = 1.5, stroke = 1) +
    geom_point(aes(x = 13, y = height / 2), fill = "white", colour = "white", shape = 21, size = 1.5, stroke = 1) +
    geom_point(aes(x = len - 13, y = height / 2), fill = "white", colour = "white", shape = 21, size = 1.5, stroke = 1) +
    geom_curve(aes(x = 18, y = 35, xend = 18, yend = height - 35), colour = "white", size = 1, curvature = 0.5) +
    geom_curve(aes(x = len - 18, y = 35, xend = len - 18, yend = height - 35), colour = "white", size = 1, curvature = -0.5) +
    geom_curve(aes(x = len / 2, y = height / 2 - 10, xend = len / 2, yend = height / 2 + 10), colour = "white", size = 1, curvature = 1) +
    geom_curve(aes(x = len / 2, y = height / 2 - 10, xend = len / 2, yend = height / 2 + 10), colour = "white", size = 1, curvature = -1) +
    geom_point(aes(x = x, y = y, size = xG, shape = chance_type, colour = Goal)
      ,
      position = position_jitterdodge(jitter.height = 3, jitter.width = 4)
      , alpha = 0.6
    ) +
    scale_colour_manual(values = c("blue", "red")) +
    annotate("label", x = len * 0.25, y = height * 0.9, label = annotate_home, size = 4) +
    annotate("label", x = len * 0.75, y = height * 0.9, label = annotate_away, size = 4) +
    labs(shape = "Chance Type", colour = NULL) +
    theme(
      rect = element_blank(),
      line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = c(0.5, 0.2),
      legend.justification = "centre",
      legend.background = element_rect(fill = "white"),
      legend.direction = "horizontal",
      legend.key.size = unit(1, "lines"),
      legend.text.align = 0,
      legend.title.align = 0
    )
}

#########################################################################################


match_odds_DC <- function(model, home, away) {
  lambda <- exp(model$par["HOME"] + model$par[paste("Attack.", home, sep = "")]
    + model$par[paste("Defence.", away, sep = "")])

  mu <- exp(model$par[paste("Attack.", away, sep = "")]
  + model$par[paste("Defence.", home, sep = "")])

  maxgoal <- 8
  probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))

  scaling_matrix <- matrix(tau(c(0, 1, 0, 1), c(0, 0, 1, 1), lambda, mu, model$par["RHO"]), nrow = 2)
  probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] / scaling_matrix

  HomeP <- sum(probability_matrix[lower.tri(probability_matrix)])
  DrawP <- sum(diag(probability_matrix))
  AwayP <- sum(probability_matrix[upper.tri(probability_matrix)])

  HomeWin <- 1.0 / HomeP
  Draw <- 1.0 / DrawP
  AwayWin <- 1.0 / AwayP

  results <- data.frame(home
    , away
    , HomeWin
    , Draw
    , AwayWin,
    stringsAsFactors = FALSE
  )

  return(results)
}
