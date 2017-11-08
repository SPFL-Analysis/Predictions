library(RODBC)
channel <- odbcConnect("SPFL")


#####################################################################################################
# data setup
#####################################################################################################

GLM_Data <- sqlQuery(channel,"
select matchDate, team, opp, xG, goals, home from [dbo].[SPFL_Prem_16_17_results]
union all
select matchDate, team, opp, xG, goals, home from [SPFL_17_18_results] where competition = 'premiership'")

GLM_Data <- sqlQuery(channel,"select * from [SPFL_Champ_16_17_results]
union all
select matchDate, team, opp, xG, goals, home from [SPFL_17_18_results] where competition = 'championship'")

GLM_Data <- sqlQuery(channel,"select * from [SPFL_l1_16_17_results]
union all
select matchDate, team, opp, xG, goals, home from [SPFL_17_18_results] where competition = 'league 1'")


GLM_Data$matchDate <- as.Date(GLM_Data$matchDate)


#####################################################################################################
# data clean up where xG is zero
#####################################################################################################

subset(GLM_Data, xG == 0)

GLM_Data[55,4] <- 0.01

#####################################################################################################
# add in time weighting column
####################################################################################################

TW_param <- -0.01
date_to <- max(as.Date(GLM_Data$matchDate,  "%d/%m/%y"), na.rm = FALSE)
GLM_Data$time_weight <- as.numeric(date_to - GLM_Data$matchDate)
GLM_Data$time_weight <- exp(TW_param*GLM_Data$time_weight)

#####################################################################################################
# loop round all match dates fitting the model to prior results
# then loop round all games on that match date predicting the odds
# recursively store the predictions
#####################################################################################################

model_xG <- glm(xG ~ team + opp + home
                     , data=GLM_Data
                     ,family=Gamma() #Change the distribution
                     , weights = GLM_Data$time_weight) #add & remove weights as required


model_goals <- glm(goals ~ team + opp + home
                        , data=GLM_Data
                        ,family=poisson() #Change the distribution
                        , weights = GLM_Data$time_weight) #add & remove weights as required

summary(model_xG)

#####################################################################################################
# Matches to be predicted
####################################################################################################

matchdate <- '2017-11-04'

writePredictionsSQL('2017-2018', matchdate, 'Premiership')
writePredictionsSQL('2017-2018', matchdate, 'Championship')
writePredictionsSQL('2017-2018', matchdate, 'League 1')


#####################################################################################################
# FUNCTION - Matches to be predicted
####################################################################################################

writePredictionsSQL <- function(season, date, comp) {
  
  GW_matches_qry <- paste("select * from [dbo].[getFixtures]('",date, "','", comp, "')", sep="")
  GW_matches <- data.frame(sqlQuery(channel,GW_matches_qry))
  
  
  for(i in seq_along(GW_matches$HomeTeam)) {
    
    df <- match_odds(model_xG, GW_matches[i,1], GW_matches[i,2])
    writeModelOddsSQL('GLM xG Gamma TW -0.01', season, comp, date, df)
    
    df1 <- match_odds(model_goals, GW_matches[i,1], GW_matches[i,2])
    writeModelOddsSQL('GLM goals Poisson TW -0.01', season, comp, date, df1)
    
    df2 <- match_odds_DC(model_DC, GW_matches[i,1], GW_matches[i,2])
    writeModelOddsSQL('DC model TW −0.0035', season, comp, date, df2) 
    
  }
  
}
