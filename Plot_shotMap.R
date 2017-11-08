
library(RODBC)
library(ggplot2)


# plot shot map -------------------------------------------------------------------------------------------------------------


create_shotmap('Aberdeen','Celtic','2017-10-25')

# Function -------------------------------------------------------------------------------------------------------------

create_shotmap <- function(home, away, date) {
  
  channel <- odbcConnect("SPFL")
  
  qry <- paste("select * from dbo.getShotMapData('", home, "', '", away, "', '", date, "')",sep = "")
  df <- sqlQuery(channel, qry)
  
  odbcClose(channel)
  
  df$home <- as.character(df$home)
  df$away <- as.character(df$away)
  df$goal_value <- as.numeric(df$goal_value)
  home <- unique(df$home)
  away <- unique(df$away)
  homedata <- subset(df, team == home)
  awaydata <- subset(df, team == away)
  
  annotate_home <- paste(home,"xG =", sprintf("%.2f",round(sum(homedata$xG),2)), "Goals =", sum(homedata$goal_value))
  annotate_away <- paste(away,"xG =", sprintf("%.2f",round(sum(awaydata$xG),2)), "Goals =", sum(awaydata$goal_value))
  
  len <- 100
  height <- 100
  
  ggplot(data = df) +
    geom_rect(aes(xmin = 0, xmax = len, ymin = 0, ymax = height),fill="green4", colour = "white", size = 1, alpha = 0.05) +
    geom_rect(aes(xmin = 0, xmax = len/2, ymin = 0, ymax = height), fill = NA, colour = "white", size = 1) +
    geom_rect(aes(xmin = 0, xmax = 18, ymin = 23, ymax = height-23), fill = NA, colour = "white", size = 1) +
    geom_rect(aes(xmin = len-18, xmax = len, ymin = 23,ymax = height-23), fill = NA, colour = "white", size = 1) +
    geom_rect(aes(xmin = 0, xmax = 6, ymin = 35, ymax = height-35), fill = NA, colour = "white", size = 1) +
    geom_rect(aes(xmin = len-6, xmax = len, ymin = 35, ymax = height-35), fill = NA, colour = "white", size = 1) +
    geom_point(aes(x=len/2, y=height/2),fill="white",colour="white",shape = 21, size = 1.5, stroke =1)+
    geom_point(aes(x=13, y=height/2),fill="white",colour="white",shape = 21, size = 1.5, stroke =1)+
    geom_point(aes(x=len-13, y=height/2),fill="white",colour="white",shape = 21, size = 1.5, stroke =1)+
    geom_curve(aes(x=18, y=35, xend = 18, yend=height-35),colour="white",size=1, curvature = 0.5)+
    geom_curve(aes(x=len-18, y=35, xend = len-18, yend=height-35),colour="white",size=1, curvature = -0.5)+
    geom_curve(aes(x=len/2, y=height/2-10, xend = len/2, yend=height/2+10),colour="white",size=1, curvature = 1)+
    geom_curve(aes(x=len/2, y=height/2-10, xend = len/2, yend=height/2+10),colour="white",size=1, curvature = -1)+
    geom_point(aes(x=x, y=y, size=xG, shape = chance_type, colour = Goal)
               ,position = position_jitterdodge(jitter.height = 3, jitter.width=4)
               ,alpha = 0.6)+
    scale_colour_manual(values=c('blue','red'))+
    annotate("label", x = len*0.25, y = height*0.9, label = annotate_home,size = 4) +
    annotate("label", x = len*0.75, y = height*0.9, label = annotate_away,size = 4) +
    labs(shape="Chance Type",colour=NULL) +
    theme(rect = element_blank(), 
          line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = c(0.5, 0.2),
          legend.justification = "centre",
          legend.background = element_rect(fill="white"),
          legend.direction = "horizontal",
          legend.key.size = unit(1,"lines"),
          legend.text.align = 0,
          legend.title.align = 0)
  
}
