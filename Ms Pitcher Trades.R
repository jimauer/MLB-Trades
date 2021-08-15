#selecting and coding players traded away by the Mariners
mtradeaway<-df2 %>% filter(from_team_id==136,type=="Trade",orig_asset=="Player") %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(season=year(trans_date),GM=ifelse(trans_date>"2015-09-28","Jerry","Jack Z")) %>% 
  select(from_team,player,team,player_id,season,GM) %>% 
  left_join(chadwick %>% select(key_mlbam,key_fangraphs),by=c("player_id"="key_mlbam")) %>% 
  left_join(fangraphs %>% group_by(playerid,Name) %>% summarise(WAR=sum(WAR)),
            by=c("key_fangraphs"="playerid")) %>% 
  arrange(desc(WAR)) %>% 
  select(season,key_fangraphs,GM)


#linking to Fangraphs, summarising WAR post trade and making a GT table of players with most WAR
fangraphpitchs %>% right_join(mtradeaway,by=(c("playerid"="key_fangraphs"))) %>% 
  filter(Season>=season,Team!="SEA") %>% group_by(playerid,Name,GM) %>% 
  summarise(WAR=sum(WAR),season=min(season)) %>% 
  ungroup() %>% 
  arrange(desc(WAR)) %>% 
  select(Name,WAR,season,GM) %>% 
  slice_head(n=10) %>% 
  gt() %>% 
  cols_label(season='Trade Season') %>%
  cols_move_to_end(
    columns = c('WAR','GM','season')) %>% 
  tab_header (
    title = md("Most War After Being Traded from Mariners"),
    subtitle = md("For Batters Traded Since 2009")
  )  %>%
  tab_source_note(
    source_note = "Transaction data from https://baseballsavant.mlb.com"
  ) %>%
  tab_source_note(
    source_note = "WAR from https://www.fangraphs.com"
  ) #%>%  gtsave("/Users/jimauer/Documents/GitHub/MLB-Trades/Most WAR post Ms.png",expand=10)

#linking to Fangraphs, summarising WAR post trade and making a GT table of players with least WAR
fangraphpitchs %>% right_join(mtradeaway,by=(c("playerid"="key_fangraphs"))) %>% 
  filter(Season>=season,Team!="SEA") %>% group_by(playerid,Name,GM) %>% 
  summarise(WAR=sum(WAR),season=min(season)) %>% 
  ungroup() %>% 
  arrange(WAR) %>% 
  select(Name,WAR,season,GM) %>% 
  slice_head(n=10) %>% 
  gt() %>% 
  cols_label(season='Trade Season') %>%
  cols_move_to_end(
    columns = c('WAR','GM')) %>% 
  tab_header (
    title = md("Least War After Being Traded from Mariners"),
    subtitle = md("For Batters Traded Since 2009")
  ) %>%
  tab_source_note(
    source_note = "Transaction data from https://baseballsavant.mlb.com"
  ) %>%
  tab_source_note(
    source_note = "WAR from https://www.fangraphs.com")
#%>%  gtsave("/Users/jimauer/Documents/GitHub/MLB-Trades/Least WAR.png")

#selecting and coding players obtained by the Mariners
mtradefor<-df2 %>% filter(team_id==136,type=="Trade",orig_asset=="Player") %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(season=year(trans_date),GM=ifelse(trans_date>"2015-09-28","Jerry","Jack Z")) %>% 
  select(from_team,player,team,player_id,season,GM) %>% 
  left_join(chadwick %>% select(key_mlbam,key_fangraphs),by=c("player_id"="key_mlbam")) %>% 
  left_join(fangraphs %>% group_by(playerid,Name) %>% summarise(WAR=sum(WAR)),
            by=c("key_fangraphs"="playerid")) %>% 
  arrange(desc(WAR)) %>% 
  select(season,key_fangraphs,GM)


#linking to Fangraphs, summarising WAR post trade and making a GT table of 
#players with most WAR since being obtained
fangraphpitchs %>% right_join(mtradefor,by=(c("playerid"="key_fangraphs"))) %>% 
  filter(Season>=season,Team=="SEA") %>% group_by(playerid,Name,GM) %>% 
  summarise(WAR=sum(WAR),season=min(season)) %>% 
  ungroup() %>% 
  arrange(desc(WAR)) %>% 
  select(Name,WAR,season,GM) %>% 
  slice_head(n=10) %>% 
  gt() %>% 
  cols_label(season='Trade Season') %>%
  cols_move_to_end(
    columns = c('WAR','GM')) %>% 
  tab_header (
    title = md("Most War After Being Obtained By Mariners"),
    subtitle = md("For Batters Traded Since 2009")
  ) %>%
  tab_source_note(
    source_note = "Transaction data from https://baseballsavant.mlb.com"
  ) %>%
  tab_source_note(
    source_note = "WAR from https://www.fangraphs.com"
  ) #%>%  gtsave("/Users/jimauer/Documents/GitHub/MLB-Trades/Best Pitchers Obtained.png")

#linking to Fangraphs, summarising WAR post trade and making a GT table of 
#players with least WAR since coming to Ms
fangraphpitchs %>% right_join(mtradefor,by=(c("playerid"="key_fangraphs"))) %>% 
  filter(Season>=season,Team=="SEA") %>% group_by(playerid,Name,GM) %>% 
  summarise(WAR=sum(WAR),season=min(season)) %>% 
  ungroup() %>% 
  arrange(WAR) %>% 
  select(Name,WAR,season,GM) %>% 
  slice_head(n=10) %>% 
  gt() %>% 
  cols_label(season='Trade Season') %>%
  cols_move_to_end(
    columns = c('WAR','GM')) %>% 
  tab_header (
    title = md("Least War After Being Obtained Mariners"),
    subtitle = md("For Batters Traded Since 2009")
  ) %>%
  tab_source_note(
    source_note = "Transaction data from https://baseballsavant.mlb.com"
  ) %>%
  tab_source_note(
    source_note = "WAR from https://www.fangraphs.com"
  ) #%>% gtsave("/Users/jimauer/Documents/GitHub/MLB-Trades/Lowest WAR since obtained by Ms.png")
