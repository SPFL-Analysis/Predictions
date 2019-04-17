channel <- RODBC::odbcConnect("SPFL")

spfl_livetext <- 
  sqlQuery(
    channel,
    "select * from SPFL.dbo.SPFL_live_text"
  )

saveRDS(spfl_livetext, "spfl_live_text.RDS")


team_map <- 
  sqlQuery(
    channel,
    "select * from SPFL.dbo.team_map"
  )

saveRDS(team_map, "team_map.RDS")

xG_BBC_chance <- 
  sqlQuery(
    channel,
    "select * from SPFL.dbo.xG_BBC_chance"
  )

saveRDS(xG_BBC_chance, "xG_BBC_chance.RDS")


chance_type <- 
  sqlQuery(
    channel,
    "select * from SPFL.dbo.chance_type"
  )

saveRDS(chance_type, "chance_type.RDS")
