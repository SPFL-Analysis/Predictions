#####################################################################
# load libs
#####################################################################

library(alabama)
library(RODBC)
channel <- odbcConnect("SPFL")

#####################################################################
# DC model data
#####################################################################

# Prem
#########

data_ <- sqlQuery(channel,"
select * from [dbo].[SPFL_Prem_16_17_results_DCfit] 
union all
select * from [dbo].[SPFL_17_18_results_DCfit]('Premiership') ")
   #               where homeTeam <> 'Hibernian' and awayTeam <> 'Hibernian'")


#Champ
#########

data_ <- sqlQuery(channel,"
select * from [dbo].[SPFL_Champ_16_17_results_DCfit]
union all
select * from [dbo].[SPFL_17_18_results_DCfit]('Championship')")
#where homeTeam not in ('Livingston','Brechin City','Inverness CT') 
#and awayTeam not in ('Livingston','Brechin City','Inverness CT') ")


#L1
#########

data_ <- sqlQuery(channel,"
select * from [dbo].[SPFL_L1_16_17_results_DCfit]
union all
select * from [dbo].[SPFL_17_18_results_DCfit]('League 1')")
#where homeTeam not in ('Raith Rovers','Ayr United','Forfar Athletic','Arbroath') 
#and awayTeam not in ('Raith Rovers','Ayr United','Forfar Athletic','Arbroath') ")


##########################################################################
#create a time weighting function using the DCM fixed paramater 0.0065
#we use 0.0325 since DC used half weeks and we are using days
##########################################################################

data_$Date <- as.Date(data_$Date)
Time_weight <- -0.00325
max_date <- max(as.Date(data_$Date,  "%d/%m/%y"), na.rm = FALSE)
datediffs <- max_date - as.Date(data_$Date, "%d/%m/%y")
data_$Time_weight <- as.numeric(max_date - as.Date(data_$Date, "%d/%m/%y"))
data_$Time_weight <- exp(Time_weight*data_$Time_weight)


#####################################################################
#initial parameter estimates
#####################################################################

dcm <- DCmodelData(data_) 
dta <- data_

attack.params <- rep(0.01, times=nlevels(dta$HomeTeam))
defence.params <- rep(-0.08, times=nlevels(dta$HomeTeam))
home.param <- 0.06
rho.init <- 0.03
par.inits <- c(home.param, rho.init, attack.params, defence.params)
#it is also usefull to give the parameters some informative names
names(par.inits) <- c('HOME', 'RHO', paste('Attack', dcm$teams, sep='.'), paste('Defence', dcm$teams, sep='.'))


#########################################################################################################
# fit the model 
########################################################################################################

model_DC <- auglag(par=par.inits, # starting point for optimiser
                   fn=DCoptimFn_tw, #function to optimise
                   heq=DCattackConstr, #equality contratinst 
                   DCm=dcm) #data from function

model_DC$par

#########################################################################################################
# store the match odds from the model and write these to SQL
########################################################################################################






