--*************************************************************
--*** chech what season & competition data we have
--************************************************************

SELECT season,competition,count(*) FROM [SPFL].[dbo].[SPFL_live_text]
left join team_map on home = short_name
group by season,competition
order by season,competition



--*************************************************************
--*** first delete the games that dont have full live text
--************************************************************

SELECT link,count(*) FROM [SPFL].[dbo].[SPFL_live_text]
left join team_map on home = short_name
where season = '2017-2018'
group by link
order by count(*) 


delete from SPFL_live_text
from SPFL_live_text t1 join
(select link from SPFL_live_text 
where season = '2015-2016' and competition = 'Scottish Cup' group by link having count(*) <50) t2 on t1.link = t2.link


--*************************************************************
--*** xG summary from the view [dbo].[SPFL_17_18_xG]
--*** summary for GLM fit in R from [dbo].[SPFL_17_18_results] - filter by league
--************************************************************

select matchDate, team, opp, xG, goals, home from [dbo].[SPFL_Prem_16_17_results]
union all
select matchDate, team, opp, xG, goals, home from [SPFL_17_18_results] where competition = 'premiership'