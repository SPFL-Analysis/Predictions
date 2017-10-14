
library(RODBC)
library(ggplot2)
library(scales) # has a percent function


#####################################################################################################
# function to write model prices to SQL
####################################################################################################

writeModelOddsSQL <- function(modelDesc, season, comp, date, df){
  
  channel <- odbcConnect("SPFL")
  
  qry <- paste("insert into model_odds select '"
               , modelDesc, "', '"
               , season, "', '"
               , comp, "', '"
               , date, "', '"
               , df[1,1], "', '"
               , df[1,2], "', '"
               , df[1,3], "', "
               , df[1,4], ", '"
               , df[1,5], "'"
               , sep="") # create SQL qry
  
  sqlQuery(channel, qry) #run query
  
  close(channel)
}

#####################################################################################################
# function to return predicted match odds
####################################################################################################

match_odds <- function(model_name, home, away){
  
  lambda <- predict(model_name,data.frame(home=1, team=home, opp=away), type="response")
  mu <- predict(model_name,data.frame(home=0, team=away, opp=home), type="response")
  
  maxgoal <- 8
  probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
  
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