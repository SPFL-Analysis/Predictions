library(RODBC)
library(ggplot2)
library(ggjoy)
channel <- odbcConnect("SPFL")

####################
# Premiership
####################


xG_data <-
  RODBC::sqlQuery(channel, "select * from [dbo].[SPFL_Prem_16_17_results]")

ggplot(data = xG_data) +
  geom_joy(
    aes(y = team, x = goals, fill = team),
    rel_min_height = 0.01,
    alpha = 0.8,
    scale = 1.1
  ) +
  geom_joy(
    aes(y = team, x = xG, fill = team),
    rel_min_height = 0.01,
    alpha = 0.1,
    scale = 1.1
  ) +
  labs(x = "Goals & Expected Goals") +
  theme(legend.position = "none")


####################
# Championship
####################

xG_data <-
  sqlQuery(channel, "select * from [dbo].[SPFL_Champ_16_17_results]")

ggplot(data = xG_data) +
  geom_joy(
    aes(y = team, x = goals, fill = team),
    rel_min_height = 0.01,
    alpha = 0.8,
    scale = 1.1
  ) +
  geom_joy(
    aes(y = team, x = xG, fill = team),
    rel_min_height = 0.01,
    alpha = 0.1,
    scale = 1.1
  ) +
  labs(x = "Goals & Expected Goals") +
  theme(legend.position = "none")

