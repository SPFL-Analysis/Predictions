library(RODBC)
library(tidyverse)
library(alabama)
source(file.path(getwd(), 'functions.R'))
channel <- odbcConnect("SPFL")
season <- "2018-2019"

# get the game dates for all the leagues
matchDates <-
  select(FT1X2_SQL, leagueName, matchDate) %>% 
    group_by(leagueName, matchDate) %>% 
    summarise() %>% 
    nest(matchDate)

purrr::pwalk(
  list(
    matchDates$leagueName,
    matchDates$data
  ),
  function(x, y) {
    GLM_Data <- 
      sqlQuery(
        channel, 
        glue::glue(
        "SELECT * FROM SPFL.[dbo].[SPFL_GLM_data] ('{season}', '{x}') 
        UNION 
        SELECT * FROM SPFL.[dbo].[SPFL_GLM_data] ('{season}', '{x}')"
        )
      )
    
    data_ <- 
      sqlQuery(
        channel,
        glue::glue(
        "select * from SPFL.[dbo].[SPFL_results_DCfit]('{x}', '{season}') 
        UNION
        select * from SPFL.[dbo].[SPFL_results_DCfit]('{x}', '{season}')"
        )
      )
    
    models_xG <- fitGlmXg(GLM_Data)
    models_goals <- fitGlmGoals(GLM_Data)
    model_DC <- fitDCmodel(data_)
    
    purrr::walk(
      y$matchDate,
      ~writePredictionsSQL(
        season,
        ., 
        x, 
        models_xG, 
        models_goals, 
        model_DC)
    )
  }
)

####################################################################################################
# prem
#####################################################################################################
GLM_Data <- 
  sqlQuery(
    channel, 
    "SELECT * FROM SPFL.[dbo].[SPFL_GLM_data] ('2018-2019', 'Premiership') UNION
    SELECT * FROM SPFL.[dbo].[SPFL_GLM_data] ('2017-2018', 'Premiership')"
    )

data_ <- 
  sqlQuery(channel,
"select * from SPFL.[dbo].[SPFL_results_DCfit]('Premiership', '2018-2019') UNION
select * from SPFL.[dbo].[SPFL_results_DCfit]('Premiership', '2017-2018')")

models_xG <- fitGlmXg(GLM_Data)
models_goals <- fitGlmGoals(GLM_Data)
model_DC <- fitDCmodel(data_)

writePredictionsSQL(season, matchdate, 'Premiership', models_xG, models_goals, model_DC)
#####################################################################################################
# champ
#####################################################################################################
GLM_Data <- 
  sqlQuery(
    channel, 
    "SELECT * FROM SPFL.[dbo].[SPFL_GLM_data] ('2018-2019', 'Championship') UNION
    SELECT * FROM SPFL.[dbo].[SPFL_GLM_data] ('2017-2018', 'Championship')"
  )

data_ <- 
  sqlQuery(channel,
           "select * from SPFL.[dbo].[SPFL_results_DCfit]('Championship', '2018-2019') UNION
           select * from SPFL.[dbo].[SPFL_results_DCfit]('Championship', '2017-2018')")

models_xG <- fitGlmXg(GLM_Data)
models_goals <- fitGlmGoals(GLM_Data)
model_DC <- fitDCmodel(data_)
writePredictionsSQL(season, matchdate, 'Championship', models_xG, models_goals, model_DC)
#####################################################################################################
# League 1
#####################################################################################################
GLM_Data <- 
  sqlQuery(
    channel, 
    "SELECT * FROM SPFL.[dbo].[SPFL_GLM_data] ('2018-2019', 'League 1') UNION
    SELECT * FROM SPFL.[dbo].[SPFL_GLM_data] ('2017-2018', 'League 1')"
  )

data_ <- 
  sqlQuery(channel,
           "select * from SPFL.[dbo].[SPFL_results_DCfit]('League 1', '2018-2019') UNION
           select * from SPFL.[dbo].[SPFL_results_DCfit]('League 1', '2017-2018')")

models_xG <- fitGlmXg(GLM_Data)
models_goals <- fitGlmGoals(GLM_Data)
model_DC <- fitDCmodel(data_)
writePredictionsSQL(season, matchdate, 'League 1', models_xG, models_goals, model_DC)
#####################################################################################################
# League 2
#####################################################################################################
GLM_Data <- 
  sqlQuery(
    channel, 
    "SELECT * FROM SPFL.[dbo].[SPFL_GLM_data] ('2018-2019', 'League 2') UNION
    SELECT * FROM SPFL.[dbo].[SPFL_GLM_data] ('2017-2018', 'League 2')"
  )

data_ <- 
  sqlQuery(channel,
           "select * from SPFL.[dbo].[SPFL_results_DCfit]('League 2', '2018-2019') UNION
           select * from SPFL.[dbo].[SPFL_results_DCfit]('League 2', '2017-2018')")

models_xG <- fitGlmXg(GLM_Data)
models_goals <- fitGlmGoals(GLM_Data)
model_DC <- fitDCmodel(data_)
writePredictionsSQL(season, matchdate, 'League 2', models_xG, models_goals, model_DC)




