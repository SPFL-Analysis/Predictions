

select  
t1.league
,t1.season
,model_desc
,t1.HomeTeam + ' v ' + t1.AwayTeam game
,[dbo].[findSprd]('home',t1.homewin,t1.awaywin) homeSprd
,[dbo].[findSprd]('away',t1.homewin,t1.awaywin) awaySprd
,t1.homewin 
,t1.draw 
,t1.awaywin
,t4.homewin mod_homewin
,t4.draw mod_draw
,t4.awaywin mod_awaywin
, [dbo].[getSprdPrice_v2]('home',t1.homewin,t1.draw,t1.awaywin,t1.homewin,t1.awaywin) mkt_homePrice
, [dbo].[getSprdPrice_v2]('away',t1.homewin,t1.draw,t1.awaywin,t1.homewin,t1.awaywin) mkt_awayPrice
, [dbo].[getSprdPrice_v2]('home',t4.homewin,t4.draw,t4.awaywin,t1.homewin,t1.awaywin) mod_homePrice
, [dbo].[getSprdPrice_v2]('away',t4.homewin,t4.draw,t4.awaywin,t1.homewin,t1.awaywin) mod_awayPrice
, ([dbo].[getSprdPrice_v2]('home',t1.homewin,t1.draw,t1.awaywin,t1.homewin,t1.awaywin) 
	/ [dbo].[getSprdPrice_v2]('home',t4.homewin,t4.draw,t4.awaywin,t1.homewin,t1.awaywin)) -1.0 hm_margin
, ([dbo].[getSprdPrice_v2]('away',t1.homewin,t1.draw,t1.awaywin,t1.homewin,t1.awaywin) 
	/ [dbo].[getSprdPrice_v2]('away',t4.homewin,t4.draw,t4.awaywin,t1.homewin,t1.awaywin)) -1.0 aw_margin
FROM market_odds t1
left join team_map t2 on HomeTeam = short_name
left join team_map t3 on AwayTeam = t3.short_name
left join model_odds t4 on t1.season = t4.season and t1.league = t4.league and t1.[date] = t4.[date]
and isnull(t2.long_name, t1.HomeTeam) = replace(t4.HomeTeam,'.','') and isnull(t3.long_name, t1.AwayTeam) = replace(t4.AwayTeam,'.','')  
where t1.season = '2017-2018'
and model_desc is not null
and t1.[date] = '2017-08-12'
order by t1.league, t1.season, t1.HomeTeam, model_desc



---***************************************************************************************************************************************************************
---*
---***************************************************************************************************************************************************************


select * from [dbo].[getMarket_v_Model_odds] ('2017-08-12')
order by game,model_desc





