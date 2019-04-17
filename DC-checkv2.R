# Make a vector of all team names. 
all_teams <- 
  sort(
    unique(
      c(as.character(dataTest$HomeTeam),
        as.character(dataTest$AwayTeam)), 
    decreasing = FALSE
    ))
    
n_teams <- length(all_teams)

# list of parameters with initial values.
parameter_list <- list(attack = rep(0, n_teams),
                       defense = rep(0, n_teams),
                       home = 0,
                       rho= 0.00)

names(parameter_list$attack) <- all_teams
names(parameter_list$defense) <- all_teams

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

dc_negloglik <- function(params, goals_home, goals_visitor,
                         team_home, team_visitor, param_skeleton){
  
  # relist, to make things easier.
  plist <- relist(params, param_skeleton)
  
  # There is a sum-to-zero constraint on defense parameters.
  # The defense parameter for the first team is computed from the rest.
  #plist$defense <- c(sum(plist$defense)*-1, plist$defense)
  #names(plist$defense)[1] <- names(plist$attack[1]) # add name to first element.
  
  # Home team expected goals
  lambda_home <- exp(plist$attack[team_home] + plist$defense[team_visitor] + plist$home)
  
  # Away team expected goals
  lambda_visitor <- exp(plist$attack[team_visitor] + plist$defense[team_home])
  
  # Dixon-Coles adjustment
  dc_adj <- tau(goals_home, goals_visitor, lambda_home, lambda_visitor, rho = plist$rho)
  
  # Trick to avoid warnings.
  if (any(dc_adj <= 0)){
    return(Inf)
  }
  
  # The log-likelihood
  log_lik_home <- dpois(goals_home, lambda = lambda_home, log=TRUE)
  log_lik_visitor <- dpois(goals_visitor, lambda = lambda_visitor, log=TRUE)
  
  log_lik <- sum((log_lik_home + log_lik_visitor + log(dc_adj)))
  
  return(log_lik*-1)
  
}

optim_res <- optim(par = unlist(parameter_list), 
                   fn=dc_negloglik,
                   goals_home = dataTest$FTHG,
                   goals_visitor = dataTest$FTAG,
                   team_home = dataTest$HomeTeam, 
                   team_visitor = dataTest$AwayTeam,
                   param_skeleton = parameter_list,
                   method = "BFGS")


# relist, and calculate the remaining parameter. 
parameter_est <- relist(optim_res$par, parameter_list)
parameter_est$defense <- c( sum(parameter_est$defense) * -1, parameter_est$defense)
names(parameter_est$defense)[1] <- names(parameter_est$attack[1]) 


estimates_GM <-
  tibble(parameter = names(optim_res$par),
         value     = optim_res$par) %>%
  filter(parameter != "rho",
         parameter != "home") %>%
  #separate(parameter, c("parameter", "team")) %>%
  mutate(value = exp(value))

