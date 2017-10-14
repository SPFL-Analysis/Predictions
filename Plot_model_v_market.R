library(RODBC)
library(ggplot2)
library(odds.converter)
channel <- odbcConnect("SPFL")


########################################################################################################
## data taken from C:\Users\David\Documents\Football_Pricing_Model\Model_v_Market_Analysis.sql
########################################################################################################

prices_anal <- sqlQuery(channel, "select * from [dbo].[getMarket_v_Model_odds] ('2017-09-23') where model_desc <> 'Bookies'")

prices_Prem <- sqlQuery(channel, "select * from [dbo].[getMarket_v_Model_odds] ('2017-09-23') where model_desc <> 'Bookies' and league = 'Premiership'")

prices_Champ <- sqlQuery(channel, "select * from [dbo].[getMarket_v_Model_odds] ('2017-09-23') where model_desc <> 'Bookies' and league = 'Championship'")

prices_L1 <- sqlQuery(channel, "select * from [dbo].[getMarket_v_Model_odds] ('2017-09-23') where model_desc <> 'Bookies' and league = 'League 1'")

########################################################################################################
## plot market, model and margins
########################################################################################################

df <- data.frame(
  x = c("xG", "Goals", "DC"),
  y = c(0, 0, 0),
  text1 = c("Home", "Home", "Home"),
  text2 = c("Away", "Away", "Away"),
  text3 = c("Margin", "Margin", "Margin")
)

plotAnalysis(prices_Champ)

########################################################################################################
## plot
########################################################################################################


plotAnalysis <- function(price_data) {
  
  ggplot() +
    geom_col(data = price_data, mapping = aes(x = model_desc 
                                               , y = odds.converter::odds.dec2prob(modPrice)
                                               , fill = Bet) 
             , colour = "black", width = 0.3, position = position_dodge(width = 0.4)) +
    geom_col(data = price_data, mapping = aes(x = model_desc 
                                               , y = odds.dec2prob(mktPrice) 
                                               , fill = Bet) 
             , colour = "black", width = 0.08, position = position_dodge(width = 0.4)) +
    geom_col(data = price_data, mapping = aes(x = model_desc 
                                               , y = margin
                                               , fill = Bet)
             , colour = "black", width = 0.18, position_nudge(-0.32)) +
    scale_fill_manual(values=c("grey45","gold", "tomato1","royalblue1")) +
    geom_label(data = df, aes(x, y, label = text1) ,label.padding = unit(0.1, "lines"), label.size = 0, nudge_x = 0.11, nudge_y = -0.15, size = 1.9, alpha = 0.2) +
    geom_label(data = df, aes(x, y, label = text2) ,label.padding = unit(0.1, "lines"), label.size = 0, nudge_x = -0.10, nudge_y = -0.15, size = 1.9, alpha = 0.2) +
    geom_label(data = df, aes(x, y, label = text3) ,label.padding = unit(0.1, "lines"), label.size = 0, nudge_x = -0.32, nudge_y = -0.15, size = 1.9, alpha = 0.2) +
    geom_hline(yintercept = 0.2, colour = "grey", linetype = "dashed") +
    geom_abline(slope = 0, intercept = 0, colour = "black") +
    facet_wrap(~ game, nrow = 2) +
    coord_flip() +
    labs(x = "Model Name", y = "Probability", fill = "") +
    theme(legend.position = "top", legend.justification = "left"
          , legend.background = element_rect(colour = "black")
          , legend.text = element_text(colour = 'black', size = 7))
  
}





