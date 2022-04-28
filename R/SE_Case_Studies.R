library(tidyverse)
library(ggraph)
library(tidygraph)
library(superNetballR)
library(patchwork)
library(magrittr)

# Accumulated shared player games -----------------------------------------

`2017_data` <- readRDS("data/2017_data.RDS")
`2018_data` <- readRDS("data/2018_data.RDS")
`2019_data` <- readRDS("data/2019_data.RDS")
`2020_data` <- readRDS("data/2020_data.RDS")
`2021_data` <- readRDS("data/2021_data.RDS")

# Map squad colours
squadCols <- tribble(
  ~squadNickname,	~Primary,	~Secondary,
  "GIANTS"	      , "#f47920",	"#0082ca",
  "Thunderbirds"	, "#ef5091",	"#ffffff",
  "Vixens"	      , "#00a88f",	"#e50e63",
  "Magpies"	  , "#000000",	"#ffffff",
  "Swifts"      , "#ee3124",	"#0093d0",
  "Firebirds"	    , "#381460",	"#fdb515",
  "Lightning"	    , "#782b8f",	"#fdb71d",
  "Fever"	        , "#00953b",	"#000000",
  "No Team",       "#999999", "#595959")

squadPrimaryCols <- squadCols %>%   
  select(-Secondary) %>% 
  deframe()

# Table of team IDs
team_info <- `2020_data` %>% 
  map_dfr(~.x[["teamInfo"]][["team"]]) %>% 
  distinct(squadNickname,squadId,squadName)

# Process files -----------------------------------------------------------

process_squads <- function(dat){
  
  season <- lubridate::year(dat[[1]][["matchInfo"]][["utcStartTime"]])
  
  player_info <- dat %>% 
    map_dfr(~.x[["playerStats"]][["player"]]) %>% 
    distinct(squadId,playerId)
  
  player_names <- dat %>% 
    map_dfr(~.x[["playerInfo"]][["player"]]) %>% 
    distinct(playerId,displayName) %>% 
    group_by(playerId) %>% 
    slice(1) %>% 
    ungroup()
  
  player_interactions <- tibble(round = map_chr(dat,~.x[["matchInfo"]][["roundNumber"]]),
                                match = map_chr(dat,~.x[["matchInfo"]][["matchNumber"]]),
                                playerId = map(dat,~.x[["playerStats"]][["player"]]) %>% modify_depth(2,"playerId")) %>% 
    unnest("playerId") %>% unnest("playerId") %>% 
    left_join(player_info) %>% 
    group_by(round,match,squadId) %>% 
    summarise(crosses = cross2(playerId,playerId,.filter = function(x, y) x >= y)  %>%  map(~paste(.x,collapse = "-")) %>% unlist()) %>% 
    ungroup() %>% 
    count(squadId,crosses) %>% 
    tidyr::extract(crosses,c("player1","player2"),"([[:alnum:]]+)-([[:alnum:]]+)",remove = F) %>% 
    mutate(across(c(player1,player2),as.integer)) %>% 
    mutate(season = season) %>% 
    inner_join(player_names, by = c("player1" = "playerId")) %>% 
    inner_join(player_names, by = c("player2" = "playerId"))
}

player_interactions <- map_dfr(list(`2017_data`,`2018_data`,`2019_data`, `2020_data`,`2021_data`),process_squads)


# Create dummy 2022 season ------------------------------------------------

digits = as.character(1:9)

players_2022 <- readRDS("data/signed_players_2022.RDS") %>% 
  rename("displayName" = "playerName") %>% 
  inner_join(team_info,by = c("name" = "squadNickname")) %>% 
  # Create fake player IDs
  mutate(playerId = map_int(playerId,~if_else(is.na(.x),paste(sample(digits,size = 9,replace = T),collapse = "") %>% as.integer(),.x)))

season_2022 <- players_2022 %>% 
  group_by(squadId) %>% 
  summarise(crosses = cross2(playerId,playerId,.filter = function(x, y) x >= y)  %>%  map(~paste(.x,collapse = "-")) %>% unlist()) %>%
  ungroup() %>% 
  count(squadId,crosses) %>% 
  tidyr::extract(crosses,c("player1","player2"),"([[:alnum:]]+)-([[:alnum:]]+)",remove = F) %>% 
  mutate(across(c(player1,player2),as.integer)) %>% 
  inner_join(select(players_2022,playerId,displayName), by = c("player1" = "playerId")) %>% 
  inner_join(select(players_2022,playerId,displayName), by = c("player2" = "playerId")) %>% 
  group_by(squadId) %>% 
  mutate(n = 630/n(), season = 2022)

# Fevers network -------------------------------------------------------------
# Find all players that have ever played for the Fever
crosses_810 <- player_interactions %>% 
  bind_rows(season_2022) %>% 
  filter(squadId == 810) %>% 
  distinct(crosses) %>% pull(crosses)

# Combined experience by cross
fever_network <- player_interactions %>% 
  bind_rows(season_2022 %>% mutate(n = 0)) %>% 
  filter(crosses %in% crosses_810) %>% 
  group_by(crosses,displayName.x,displayName.y) %>% 
  summarise(total_combined = sum(n),
            last_season = max(season)) %>% 
  ungroup()

#Each players' last season
last_season <- fever_network %>% 
  filter(crosses %in% crosses_810) %>% 
  select(displayName.x,displayName.y,last_season) %>% 
  pivot_longer(cols = -last_season,values_to = "playerName") %>%
  group_by(playerName) %>% summarise(last_season = max(last_season)) %>% 
  distinct(last_season,playerName) %>% 
  ungroup()

nodes <- tibble(playerName = unique(c(fever_network$displayName.x,fever_network$displayName.y))) %>% 
  mutate(ID = 1:n()) %>% 
  left_join(last_season) %>% 
  mutate(last_season = factor(last_season))

edges <- fever_network %>% 
  left_join(nodes %>% select(-last_season),by = c("displayName.x"="playerName")) %>% 
  rename("from" = "ID") %>% 
  left_join(nodes %>% select(-last_season),by = c("displayName.y"="playerName")) %>% 
  rename("to" = "ID") %>% 
  mutate(total_combined = if_else(total_combined == 0,NA_real_,total_combined)) %>% 
  arrange(last_season,crosses)

network <- tbl_graph(nodes = nodes, edges = edges)

ggraph(network, layout = 'kk') + 
  geom_edge_link(aes(edge_width = total_combined,
                     edge_colour = total_combined,
                     edge_alpha = factor(last_season == 2022)),
                 arrow = arrow(length = unit(2, 'mm')), 
                 start_cap = circle(2, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = 'along',
                 label_dodge = -unit(2.5, 'mm'),
                 label_push = unit(10, 'mm')) + 
  geom_node_point(size = 20,aes(col = last_season)) +
  geom_node_text(aes(label = playerName), repel = F,col = "white",size = 3) +
  #scale_edge_color_manual(values = c("grey70","black")) +
  scale_edge_color_continuous(breaks = seq(0,70,10)) +
  scale_edge_alpha_manual(values = c(0.3,1)) +
  scale_color_brewer(palette = "RdYlGn",direction = -1,
                     guide = guide_legend(override.aes=aes(size=8))) +
  scale_edge_width(guide = "none") +
  expand_limits(y = c(-3.5,2))

# Vixens network -------------------------------------------------------------
# Find all players that have ever played for the Fever
crosses_804 <- player_interactions %>% 
  bind_rows(season_2022) %>% 
  filter(squadId == 804) %>% 
  distinct(crosses) %>% pull(crosses)

# Combined experience by cross
vixens_network <- player_interactions %>% 
  bind_rows(season_2022 %>% mutate(n = 0)) %>% 
  filter(crosses %in% crosses_804) %>% 
  group_by(crosses,displayName.x,displayName.y) %>% 
  summarise(total_combined = sum(n),
            last_season = max(season)) %>% 
  ungroup()

#Each players' last season
last_season <- vixens_network %>% 
  filter(crosses %in% crosses_804) %>% 
  select(displayName.x,displayName.y,last_season) %>% 
  pivot_longer(cols = -last_season,values_to = "playerName") %>%
  group_by(playerName) %>% summarise(last_season = max(last_season)) %>% 
  distinct(last_season,playerName) %>% 
  ungroup()

nodes <- tibble(playerName = unique(c(vixens_network$displayName.x,vixens_network$displayName.y))) %>% 
  mutate(ID = 1:n()) %>% 
  left_join(last_season) %>% 
  mutate(last_season = factor(last_season))

edges <- vixens_network %>% 
  left_join(nodes %>% select(-last_season),by = c("displayName.x"="playerName")) %>% 
  rename("from" = "ID") %>% 
  left_join(nodes %>% select(-last_season),by = c("displayName.y"="playerName")) %>% 
  rename("to" = "ID") %>% 
  mutate(total_combined = if_else(total_combined == 0,NA_real_,total_combined)) %>% 
  arrange(-last_season,total_combined)

network <- tbl_graph(nodes = nodes, edges = edges)

ggraph(network, layout = 'kk') + 
  geom_edge_link(aes(edge_width = total_combined,edge_colour = total_combined),
                 arrow = arrow(length = unit(2, 'mm')), 
                 start_cap = circle(2, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = 'along',
                 label_dodge = -unit(2.5, 'mm'),
                 label_push = unit(10, 'mm')) + 
  geom_node_point(size = 20,aes(col = last_season)) +
  geom_node_text(aes(label = playerName), repel = F,col = "white",size = 3) +
  scale_edge_color_continuous(breaks = seq(0,70,10)) +
  scale_color_brewer(palette = "RdYlGn",direction = -1,
                     guide = guide_legend(override.aes=aes(size=8))) +
  scale_edge_width(guide = "none") +
  expand_limits(y = c(-3.5,2))


# Team Lists - Ins & Outs -------------------------------------------------

player_transactions <- player_interactions %>%
bind_rows(season_2022) %>%
arrange(season,squadId, crosses) %>%
group_by(crosses) %>%
mutate(accum = cumsum(n)) %>%
ungroup() %>%
arrange(squadId,season) %>%
group_by(squadId,season) %>% 
summarise(lists = list(unique(c(player1,player2))),
          names = list(unique(c(displayName.x,displayName.y))),
          .groups = "drop_last") %>%
mutate(
left = map2(lag(lists),lists,~setdiff(unlist(.x),unlist(.y))),
stayed = map2(lag(lists),lists,~intersect(unlist(.x),unlist(.y))),
new = map2(lists,lag(lists),~setdiff(unlist(.x),unlist(.y))),
leaving = lead(left),
left_n = map2(lag(names),names,~setdiff(unlist(.x),unlist(.y))),
stayed_n = map2(lag(names),names,~intersect(unlist(.x),unlist(.y))),
new_n = map2(names,lag(names),~setdiff(unlist(.x),unlist(.y))),
leaving_n = lead(names)) %>%
left_join(team_info)

player_transactions %>% 
  filter(squadNickname == "Vixens") %>% View("Vix")

player_interactions %>% 
inner_join(player_transactions %>% select(squadId,season,leaving,leaving_n)) %>%
  mutate(lost_se = pmap_int(list(player1, player2, leaving,n),function(x,y,z,n) (x %in% z | y %in% z) * n)) %>% 
  group_by(crosses) %>%
  mutate(SE = cumsum(n),
         lost_se = cumsum(lost_se)) %>%
  group_by(season,squadId) %>%
  summarise(SE = sum(SE),
            lost_se = sum(lost_se),
            lost_se_pct = scales::percent(lost_se/SE),
            leaving_n = leaving_n[1]) %>% 
  inner_join(team_info) %>% 
  View()




