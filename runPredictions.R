library(RODBC)
library(tidyverse)
library(alabama)
source(file.path(getwd(), 'functions.R'))
channel <- odbcConnect("SPFL")
matchdate <- '2018-08-04'

#####################################################################################################
# prem
#####################################################################################################
GLM_Data <- sqlQuery(channel,
                     "select matchDate, team, opp, xG, goals, home from SPFL.dbo.[SPFL_17_18_results] where competition = 'premiership'")
data_ <- sqlQuery(channel,"
                  select * from SPFL.[dbo].[SPFL_17_18_results_DCfit]('Premiership')")

models_xG <- fitGlmXg(GLM_Data)
models_goals <- fitGlmGoals(GLM_Data)
model_DC <- fitDCmodel(data_)
writePredictionsSQL('2017-2018', matchdate, 'Premiership', models_xG, models_goals, model_DC)
#####################################################################################################
# champ
#####################################################################################################
GLM_Data <- sqlQuery(channel,"select matchDate, team, opp, xG, goals, home from [SPFL_17_18_results] where competition = 'championship'")
data_ <- sqlQuery(channel,"select * from [dbo].[SPFL_17_18_results_DCfit]('Championship')")

models_xG <- fitGlmXg(GLM_Data)
models_goals <- fitGlmGoals(GLM_Data)
model_DC <- fitDCmodel(data_)
writePredictionsSQL('2017-2018', matchdate, 'Championship', models_xG, models_goals, model_DC)
#####################################################################################################
# League 1
#####################################################################################################
GLM_Data <- sqlQuery(channel,"select matchDate, team, opp, xG, goals, home from [SPFL_17_18_results] where competition = 'League 1'")
data_ <- sqlQuery(channel,"select * from [dbo].[SPFL_17_18_results_DCfit]('League 1')")

models_xG <- fitGlmXg(GLM_Data)
models_goals <- fitGlmGoals(GLM_Data)
model_DC <- fitDCmodel(data_)
writePredictionsSQL('2017-2018', matchdate, 'League 1', models_xG, models_goals, model_DC)
#####################################################################################################
# League 2
#####################################################################################################
GLM_Data <- sqlQuery(channel,"select matchDate, team, opp, xG, goals, home 
                     from [SPFL_17_18_results] where competition = 'league 2'")
data_ <- sqlQuery(channel,"select * from [dbo].[SPFL_17_18_results_DCfit]('League 2')") %>%
  mutate(., matchDate = as.Date(matchDate)) %>%
  rename(., Date = matchDate)

models_xG <- fitGlmXg(GLM_Data)
models_goals <- fitGlmGoals(GLM_Data)
model_DC <- fitDCmodel(data_)
writePredictionsSQL('2017-2018', matchdate, 'League 2', models_xG, models_goals, model_DC)




