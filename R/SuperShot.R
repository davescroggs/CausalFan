library(tidyverse)
library(superNetballR)
library(tidytext)

# `2017_data` <- readRDS("data/2017_data.RDS")
# `2018_data` <- readRDS("data/2018_data.RDS")
# `2019_data` <- readRDS("data/2019_data.RDS")
`2020_data` <- readRDS("data/2020_data.RDS")
`2021_data` <- readRDS("data/2021_data.RDS")

team_info <- `2020_data` %>% 
  map_dfr(~.x[["teamInfo"]][["team"]]) %>% 
  distinct(squadNickname,squadId,squadName)

process_scores <- function(dat){
  
  season <- lubridate::year(dat[[1]][["matchInfo"]][["utcStartTime"]])
  
  tibble(round = map_chr(dat,~.x[["matchInfo"]][["roundNumber"]]),
         match = map_chr(dat,~.x[["matchInfo"]][["matchNumber"]]),
         home_squad = map_int(dat,~.x[["matchInfo"]][["homeSquadId"]]),
         away_squad = map_int(dat,~.x[["matchInfo"]][["awaySquadId"]]),
         scores = map(dat,~.x[["scoreFlow"]][["score"]])) %>% 
    unnest_longer("scores") %>% unnest_wider("scores") %>% 
    mutate(season = season)
   
}

scores <- map_dfr(list(`2020_data`,`2021_data`),process_scores)

scores %>% 
  count(season,scoreName) %>% 
  pivot_wider(names_from = scoreName,values_from = n)

player_names_func <- function(dat){
  
  season <- lubridate::year(dat[[1]][["matchInfo"]][["utcStartTime"]])
  
  player_info <- dat %>% 
    map_dfr(~.x[["playerStats"]][["player"]]) %>% 
    distinct(squadId,playerId)
  
  player_names <- dat %>% 
    map_dfr(~.x[["playerInfo"]][["player"]]) %>% 
    distinct(playerId,displayName) %>% 
    left_join(player_info,by = "playerId") %>% 
    mutate(season = season)
}

player_names <- map_dfr(list(`2020_data`,`2021_data`),player_names_func) %>%
  arrange(-season) %>%
  distinct(playerId,squadId,.keep_all = T) %>%
  select(-season)

scores %>% 
  filter(periodSeconds >= 600,season > 2019) %>% 
  group_by(playerId,squadId,scoreName,season) %>% 
  summarise(points = n()) %>% 
  pivot_wider(names_from = scoreName,values_from = points,values_fill = list(points = 0)) %>%
  mutate(pt2 = `2pt Goal` + `2pt Miss`,
         pt1 = goal + miss,
         prop2 = pt2/(pt2 + pt1)) %>% 
  left_join(player_names) %>% View()

scores_by_margin <- scores %>% 
  filter(periodSeconds >= 600) %>% 
  arrange(season,round,match) %>% 
  group_by(season,round,match) %>% 
  mutate(
    homeScore = if_else(squadId == home_squad,scorepoints,0L) %>% cumsum(),
    awayScore = if_else(squadId == away_squad,scorepoints,0L) %>% cumsum(),
    scoreDiff = if_else(squadId == home_squad,homeScore - awayScore,awayScore - homeScore),
    scoreMargin = case_when(
      scoreDiff >= 6 ~ "6+ up", 
      between(scoreDiff,1,5) ~ "1-5 up", 
      scoreDiff == 0 ~ "Even",
      between(scoreDiff,-5,-1) ~ "1-5 down", 
      scoreDiff <= -6 ~ "6+ down"),
    scoreMargin = factor(scoreMargin,levels = c("6+ down","1-5 down","Even","1-5 up","6+ up"),ordered = T),
    SS_YN = if_else(str_detect(scoreName,"2"),"Super","Normal")) %>%
  ungroup() %>%
  left_join(team_info,by = "squadId")

scores_by_margin %>% 
  count(season,squadNickname,scoreMargin,SS_YN) %>% 
  ggplot(aes(y = fct_inorder(scoreMargin),x = n,fill = SS_YN)) +
  geom_col() +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Normal" = "#1874CD","Super" = "#FF3030")) +
  labs(x = "Total attempts",
       y = "Score difference",
       fill = "Shot type",
       title = "Power 5 shot attempts in 2021 season",
       caption = "Data from Champion Data") +
  facet_grid(season~squadNickname)
  
scores_by_margin %>% 
  count(season,SS_YN,squadNickname) %>% 
  group_by(season,squadNickname) %>% 
  mutate(pct = n/sum(n),
         pct_ss = if_else(SS_YN == "Super",pct,1 - pct),
         squadNickname = reorder_within(squadNickname,pct,season)) %>% 
  arrange(season,pct_ss) %>% 
  ggplot(aes(y = fct_inorder(squadNickname),x = pct,fill = SS_YN)) +
  geom_col() +
  theme_bw() +
  scale_y_reordered() +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Normal" = "#1874CD","Super" = "#FF3030")) +
  labs(x = "Total attempts",
       y = "Score difference",
       fill = "Shot type",
       title = "Power 5 shot attempts in 2021 season",
       caption = "Data from Champion Data") +
  facet_wrap(~season,scales = "free_y")

scores_by_margin %>% 
  count(season,squadNickname,SS_YN,round,match,wt = scorepoints) %>% 
  pivot_wider(names_from = SS_YN,values_from = n,values_fill = list(n = 0)) %>% 
  mutate(total = Normal + Super,
         pct = Super/total,
         round = as.integer(round)) %>% 
  ggplot(aes(x = total,y = pct,col = squadNickname,group = squadNickname)) +
  geom_point() +
  facet_grid(~season)
  
  

scores %>% 
  filter(periodSeconds >= 600) %>% 
    arrange(season,round,match) %>% 
    group_by(season,round,match) %>% 
    mutate(
      SS_YN = if_else(str_detect(scoreName,"2"),"Super","Normal")) %>%
    ungroup() %>%
    left_join(team_info,by = "squadId") %>% 
  count(season,squadNickname,SS_YN,round,match, scorepoints) %>% 
  pivot_wider(names_from = SS_YN,values_from = n,values_fill = list(n = 0)) %>% 
  mutate(total = (Normal + Super) * scorepoints,
         round = as.integer(round)) %>% 
  group_by(season,squadNickname,round,match) %>% 
  summarise(points_total = sum(total),
            supers = sum(Super),
            shots = sum(Normal) + supers,
            pct_ss = supers / shots) %>% 
  ggplot(aes(y = points_total,x = supers,col = squadNickname,group = squadNickname)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_grid(season~.)


scores_by_margin %>% 
  count(season,squadId,scoreName) %>% 
  pivot_wider(names_from = scoreName,values_from = n,values_fill = list(n = 0)) %>% 
  left_join(team_info) %>% 
  mutate(pct2 =  `2pt Miss`/( `2pt Miss` + `2pt Goal`)) %>% 
  filter(season == 2021, squadNickname == "Magpies")
