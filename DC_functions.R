
################################################################
## taken from http://opisthokonta.net/?p=890
#####################################################################
## Dixon COles adjustment fuction
#####################################################################

tau <- Vectorize(function(xx, yy, lambda, mu, rho){
  if (xx == 0 & yy == 0){return(1 - (lambda*mu*rho))
  } else if (xx == 0 & yy == 1){return(1 + (lambda*rho))
  } else if (xx == 1 & yy == 0){return(1 + (mu*rho))
  } else if (xx == 1 & yy == 1){return(1 - rho)
  } else {return(1)}
})

#####################################################################
## Log likelihood function
#####################################################################

logLik <- function(y1, y2, lambda, mu, rho=0){
  #rho=0, independence
  #y1: home goals
  #y2: away goals
  
  sum(log(dpois(y1, lambda)), log(dpois(y2, mu)))
}


DClogLik <- function(y1, y2, lambda, mu, rho=0){
  #rho=0, independence
  #y1: home goals
  #y2: away goals
  
  sum(log(tau(y1, y2, lambda, mu, rho)), log(dpois(y1, lambda)), log(dpois(y2, mu)))
}

DClogLik_tw <- function(y1, y2, lambda, mu, rho=0){
  #rho=0, independence
  #y1: home goals
  #y2: away goals
  
  time.weight = data_$Time_weight
  
  sum(time.weight*log(tau(y1, y2, lambda, mu, rho)), time.weight*log(dpois(y1, lambda)), time.weight*log(dpois(y2, mu)))
}


#####################################################################
## Data prep function
#####################################################################
DCmodelData <- function(df){
  
  hm <- model.matrix(~ HomeTeam - 1, data=df)
  am <- model.matrix(~ AwayTeam -1, data=df)
  
  team.names <- unique(c(levels(df$HomeTeam), levels(df$AwayTeam)))
  
  return(list(
    homeTeamDM=hm,
    awayTeamDM=am,
    homeGoals=df$FTHG,
    awayGoals=df$FTAG,
    teams=team.names
  )) 
}


#####################################################################
## maximum likelihood fucntion
#####################################################################

DCoptimFn <- function(params, DCm){
  
  home.p <- params[1]
  rho.p <- params[2]
  
  nteams <- length(DCm$teams)
  attack.p <- matrix(params[3:(nteams+2)], ncol=1)
  defence.p <- matrix(params[(nteams+3):length(params)], ncol=1)
  
  lambda <- exp(DCm$homeTeamDM %*% attack.p + DCm$awayTeamDM %*% defence.p + home.p)
  mu <- exp(DCm$awayTeamDM %*% attack.p + DCm$homeTeamDM %*% defence.p)
  
  return(
    DClogLik(y1=DCm$homeGoals, y2=DCm$awayGoals, lambda, mu, rho.p) * -1
  )
}

#########################################################################
###time weighted function
#########################################################################

DCoptimFn_tw <- function(params, DCm){
  
  home.p <- params[1]
  rho.p <- params[2]
  
  nteams <- length(DCm$teams)
  attack.p <- matrix(params[3:(nteams+2)], ncol=1)
  defence.p <- matrix(params[(nteams+3):length(params)], ncol=1)
  
  lambda <- exp(DCm$homeTeamDM %*% attack.p + DCm$awayTeamDM %*% defence.p + home.p)
  mu <- exp(DCm$awayTeamDM %*% attack.p + DCm$homeTeamDM %*% defence.p)
  
  return(
    DClogLik_tw(y1=DCm$homeGoals, y2=DCm$awayGoals, lambda, mu, rho.p) * -1
  )
}

#########################################################################
# optimise without DC adjustment
#########################################################################

optimFn <- function(params, DCm){
  
  home.p <- params[1]
  rho.p <- params[2]
  
  nteams <- length(DCm$teams)
  attack.p <- matrix(params[3:(nteams+2)], ncol=1)
  defence.p <- matrix(params[(nteams+3):length(params)], ncol=1)
  
  lambda <- exp(DCm$homeTeamDM %*% attack.p + DCm$awayTeamDM %*% defence.p + home.p)
  mu <- exp(DCm$awayTeamDM %*% attack.p + DCm$homeTeamDM %*% defence.p)
  
  return(
    logLik(y1=DCm$homeGoals, y2=DCm$awayGoals, lambda, mu, rho.p) * -1
  )
}

#####################################################################
## normalise attack parameter sum
#####################################################################
DCattackConstr <- function(params, DCm, ...){
  nteams <- length(DCm$teams)
  attack.p <- matrix(params[3:(nteams+2)], ncol=1)
  return((sum(attack.p) / nteams) - 1)
  
}


#####################################################################################################
# function to return predicted match odds
####################################################################################################

match_odds_DC <- function(model, home, away){
  
  lambda <- exp(model$par['HOME'] + model$par[paste('Attack.', home, sep = "")] 
                + model$par[paste('Defence.', away,sep="")])
  
  mu <- exp(model$par[paste('Attack.', away, sep = "")] 
            + model$par[paste('Defence.', home,sep="")])
  
  maxgoal <- 8
  probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
  
  scaling_matrix <- matrix(tau(c(0,1,0,1), c(0,0,1,1), lambda, mu, model$par['RHO']), nrow=2)
  probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] / scaling_matrix
  
  HomeP <- sum(probability_matrix[lower.tri(probability_matrix)])
  DrawP <- sum(diag(probability_matrix))
  AwayP <- sum(probability_matrix[upper.tri(probability_matrix)])
  
  HomeWin <- 1.0/HomeP
  Draw <- 1.0/DrawP
  AwayWin <- 1.0/AwayP
  
  results <- data.frame(home
                        ,away
                        , HomeWin
                        , Draw
                        , AwayWin , stringsAsFactors = FALSE)
  
  return(results)
  
}


#####################################################################################################
# function to return predicted match odds & over 2.5 goals
####################################################################################################

function(model, home, away){
  
  lambda <- exp(model$par['HOME'] + model$par[paste('Attack.', home, sep = "")] 
                + model$par[paste('Defence.', away,sep="")])
  
  mu <- exp(model$par[paste('Attack.', away, sep = "")] 
            + model$par[paste('Defence.', home,sep="")])
  
  maxgoal <- 8
  probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
  
  scaling_matrix <- matrix(tau(c(0,1,0,1), c(0,0,1,1), lambda, mu, model$par['RHO']), nrow=2)
  probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] / scaling_matrix
  
  scores <- matrix(0, nrow = 9, ncol = 9) 
  scores <- apply(scores, 2, function(x) x + matrix(0:8, ncol = 1)) 
  scores <- apply(scores, 1, function(x) x + matrix(0:8, nrow = 1))
  over_2_5 <- apply(scores, 1, function(x) ifelse(x<3,0,1))
  
  1/sum(over_2_5 * probability_matrix)
  
  HomeP <- sum(probability_matrix[lower.tri(probability_matrix)])
  DrawP <- sum(diag(probability_matrix))
  AwayP <- sum(probability_matrix[upper.tri(probability_matrix)])
  over_2_5_P <- sum(over_2_5 * probability_matrix)
  
  HomeWin <- 1.0/HomeP
  Draw <- 1.0/DrawP
  AwayWin <- 1.0/AwayP
  over_2_5 <- 1/over_2_5_P
  
  results <- data.frame(home
                        ,away
                        , HomeWin
                        , Draw
                        , AwayWin , stringsAsFactors = FALSE)
  
  return(results)
  
}
