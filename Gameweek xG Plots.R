
library(RODBC)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringi)
library(stringr)

channel <- odbcConnect("SPFL")


GW_results_prem <- sqlQuery(channel, "select * from dbo.[gameweek_results_v2] ('2017-11-04', '2017-11-05', 'Premiership')")

GW_results_champ <- sqlQuery(channel, "select * from dbo.[gameweek_results_v2] ('2017-11-04', '2017-11-05', 'Championship')")

GW_results_L1 <- sqlQuery(channel, "select * from dbo.[gameweek_results_v2] ('2017-11-04', '2017-11-05', 'League 1')")

GW_results_L2 <- sqlQuery(channel, "select * from dbo.[gameweek_results_v2] ('2017-11-04', '2017-11-05', 'League 2')")

#GW_results_cup<- sqlQuery(channel, "select * from dbo.[gameweek_results_v2] ('2017-11-04', '2017-11-05', 'Scottish Cup')")

##########################################################################################
## plot game week results 
##########################################################################################



p1 <- plot_GW_results_v2(GW_results_prem)

p2 <- plot_GW_results_v2(GW_results_champ)

p3 <- plot_GW_results_v2(GW_results_L1)

p4 <- plot_GW_results_v2(GW_results_L2)


multiplot(p1, p2, p3, p4, cols = 2) #manually added function see scrpit multiplot.R



##########################################################################################
## plot function
##########################################################################################

plot_GW_results <- function(results) {
  
  ggplot(data = results) +
    geom_col(aes(x = game, y = hm_xG)
             ,colour = "black",fill = "blue", width = 0.05, position = position_nudge(x = 0.16)) +
    geom_col(aes(x = game, y = hm_goals)
             ,colour = "black",fill = "red", width = 0.05, position = position_nudge(x = 0.08)) +
    geom_col(aes(x = game, y = aw_xG)
             ,colour = "black",fill = "blue",width = 0.05, position = position_nudge(x = -0.08)) +
    geom_col(aes(x = game, y = aw_goals)
             , colour = "black",fill = "red",width = 0.05, position = position_nudge(x = -0.16)) +
    geom_text(aes(x=game, y=-0.2, label = "Home")
               , nudge_x = 0.12, size = 3.5, alpha = 1) +
    geom_text(aes(x=game, y=-0.2, label = "Away")
               , nudge_x = -0.12, size = 3.5, alpha = 1) +
    geom_label(aes(x=game, y=hm_xG, label = paste("xG = ", round(hm_xG, 2), sep = " "))
               ,colour = "blue", nudge_x = 0.18, nudge_y = 0.15, size = 2.7, alpha = 1) +
    geom_label(aes(x=game, y=hm_goals, label = paste("Goals = ", round(hm_goals, 0), sep = " "))
               ,colour = "red", nudge_x = 0.06, nudge_y = 0.15, size = 2.7, alpha = 1) +
    geom_label(aes(x=game, y=aw_xG, label = paste("xG = ", round(aw_xG, 2), sep = " "))
               , colour = "blue", nudge_x = -0.06, nudge_y = 0.15, size = 2.7, alpha = 1) +
    geom_label(aes(x=game, y=aw_goals, label = paste("Goals = ", round(aw_goals, 0), sep = " "))
               , colour = "red", nudge_x = -0.18, nudge_y = 0.15, size = 2.7, alpha = 1) +
    labs(x = "", y = "Goals & Expected goals") +
    theme(axis.text.x=element_text(hjust=0.95)) +
    coord_flip()
    #ggthemes::theme_economist()
  
}


##########################################################################################
## plot function
##########################################################################################

plot_GW_results_v2 <- function(results) {
  
  results$team <- as.character(results$team)
  results$teamColour<- as.character(results$teamColour)
  results$gameID_2<- as.numeric(results$gameID_2)
  
  results <- results[order(results$gameID_2),]
  
  ggplot(data = results) +
    geom_col(aes(x = reorder(team,-gameID_2), y = xG_BT, fill = reorder(team,gameID_2)),
             colour = "black",width = 0.7) +
    scale_fill_manual(values = results$teamColour)+ 
    geom_text(aes(x = reorder(team,-gameID_2), y = xG_BT + 0.28, label = sprintf("%.2f", round(xG_BT,2)))
              ,size = 3
              ,fontface = "plain"
              ,colour= unlist(rapply(list(results$teamColour)
                                     ,function(x) ifelse(x=="white","black",x)
                                     , how = "replace"))) +
    labs(x = "", y = "Expected goals") +
    facet_grid(gameID ~ competition, scales = "free") +
    theme(strip.background = element_blank(),
          strip.text.y = element_blank(),
          panel.background = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.line.x=element_blank(),
          axis.text.y=element_text(colour = "black",hjust=1, family = "courier"),
          axis.ticks=element_blank(),
          strip.text.x=element_text(size = 12,family = "courier", face = "italic"),
          legend.position="none") +
    coord_flip()

}


