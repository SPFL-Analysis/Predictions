load("~/R/14_bivpois_RDATA/bivpois.R.RData")

library(RODBC)

data_spfl <- sqlQuery(odbcConnect("SPFL"),"
select HomeTeam team1
, AwayTeam team2 
,FTHG g1
,FTAG g2
from [dbo].[SPFL_Prem_16_17_results_DCfit] 
union all
select HomeTeam team1
,AwayTeam team2 
,FTHG g1
,FTAG g2
from [dbo].[SPFL_17_18_results_DCfit]('Premiership') ")



#
# formula for modeling of lambda1 and lambda2
form1 <- ~c(team1,team2)+c(team2,team1)
#
# Model 1: Double Poisson
ex4.m1 <- lm.bp(g1~1, g2~1, l1l2=form1, zeroL3=TRUE, data=data_spfl)
#
# Models 2-5: bivariate Poisson models
ex4.m2 <- lm.bp(g1~1,g2~1, l1l2=form1, data=data_spfl)
ex4.m3 <- lm.bp(g1~1,g2~1, l1l2=form1, l3=~team1, data=data_spfl)
ex4.m4 <- lm.bp(g1~1,g2~1, l1l2=form1, l3=~team2, data=data_spfl)
ex4.m5 <- lm.bp(g1~1,g2~1, l1l2=form1, l3=~team1+team2, data=data_spfl)
#
# Model 6: Zero Inflated Model
ex4.m6 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=data_spfl, jmax=0)
#
# Models 7-11: Diagonal Inflated Bivariate Poisson Models
ex4.m7 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=data_spfl, distribution="geometric")
ex4.m8 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=data_spfl, jmax=1)
ex4.m9 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=data_spfl, jmax=2)
ex4.m10 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=data_spfl, jmax=3)
ex4.m11 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=data_spfl, distribution="poisson")
#
# Models 12: Diagonal Inflated Double Poisson Model
ex4.m12 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=data_spfl, distribution="poisson", zeroL3=TRUE)


ex4.m1$coefficients
