library(tidyverse)
library(baseballr)
library(lubridate)
library(gt)

#fuctions to scrape transction data
get_data <- function (year) {
  root_url <- 'http://lookup-service-prod.mlb.com'
  params_dates <- sprintf('start_date=%s0101&end_date=%s1231', year, year+1)
  params <- paste('/json/named.transaction_all.bam?&sport_code=%27mlb%27', params_dates, sep = '&')
  js <- jsonlite::fromJSON(paste0(root_url, params))
  return (js)
}
get_processed_data <- function (year) get_data(year=year)$transaction_all$queryResults$row


#scraping trades from 2009 to 2021 and converting to tbl
df1<- 2009:2021 %>% purrr::map(function(x) get_processed_data(x))
df2 <- plyr::ldply(df1,data.frame)
remove(df1)
df2$player_id<-as.numeric(df2$player_id)
df2$team_id<-as.numeric(df2$team_id)
df2$from_team_id<-as.numeric(df2$from_team_id)
df2$trans_date<-ymd_hms(df2$trans_date)
write_csv2(df2,"/Users/jimauer/Documents/GitHub/MLB-Trades/tradedat.csv")

#url to download chadwick
urlfile="https://github.com/chadwickbureau/register/blob/master/data/people.csv?raw=true"
chadwick<-read_csv(url(urlfile))
remove(urlfile)
write_csv2(chadwick,"/Users/jimauer/Documents/GitHub/MLB-Trades/chadwick.csv")
chadwick<-read_csv2("/Users/jimauer/Documents/GitHub/MLB-Trades/chadwick.csv")

fangraphs<-read.csv("/Users/jimauer/Documents/GitHub/MLB-Trades/FanGraphs Leaderboard.csv")
fangraphpitchs<-read.csv("/Users/jimauer/Documents/GitHub/MLB-Trades/FanGraphs Leaderboard-3.csv")

FanGMaster<- bind_rows(fangraphs %>% select(Season,Name,Team,WAR,playerid),
fangraphpitchs %>% select(Season,Name,Team,WAR,playerid))

#charting transactions between MLB teams
df2 %>% filter(type=="Trade",from_team!="") %>% 
  filter(team_id>107,team_id<159,from_team_id>107,from_team_id<159) %>% 
  distinct(transaction_id,.keep_all=TRUE) %>% 
  select(transaction_id,team,from_team) %>%
  group_by(team,from_team) %>% summarise(Trades=n()) %>% 
  ungroup() %>% arrange(desc(Trades)) %>% 
  ggplot(aes(x=team,y=from_team,fill=Trades)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  scale_fill_viridis_c(direction=1) +
  labs(x="Receiving Team", y="Departing Team", title="MLB Teams Most Frequent Trade Partners",
       caption="Data from https://baseballsavant.mlb.com")


teams<-df2 %>% group_by(team_id, team) %>% select(team_id,team) %>% summarise(n=n()) %>% 
  filter(team_id>107,team_id<159)%>% 
  arrange(desc(team))

df2 %>% filter(from_team_id==136,type=="Trade") %>% 
  distinct(transaction_id,.keep_all=TRUE) %>% 
  select(transaction_id,team,from_team) %>%
  group_by(team,from_team) %>% summarise(Trades=n()) %>% 
  ungroup() %>% arrange(desc(Trades))%>% 
  ggplot(aes(y=Trades,x=team)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
