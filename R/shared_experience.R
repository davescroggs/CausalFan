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
  distinct(squadNickname,squadId)


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

player_interactions %>% 
  arrange(player1,player2) %>% 
  inner_join(players_2020, by = c("player1" = "playerId")) %>% 
  inner_join(players_2020, by = c("player2" = "playerId")) %>%
  ggplot(aes(x = fct_inorder(displayName.x),y = fct_inorder(displayName.y),fill = n,label = n)) +
  geom_tile() +
  geom_text()



player_interactions %>% count(crosses,wt= n) %>% 
  arrange(-n,crosses) %>% 
  left_join(player_interactions %>%
              distinct(crosses,displayName.x,displayName.y),by = "crosses") %>% View()

########
# ANNOTATE THE LARGE DROPS WITH THE PLAYERS THAT HAVE LEFT
########


player_interactions %>% 
  bind_rows(season_2022) %>% 
  arrange(season,squadId, crosses) %>% 
  group_by(crosses) %>% 
  mutate(accum = cumsum(n)) %>% 
  ungroup()  %>% 
  count(squadId,season,wt = accum) %>%
  inner_join(team_info) %>% 
  bind_rows(team_info %>% mutate(season = 2016,n = 0))  %>% 
  #inner_join(ladders %>% filter(season == 2021))
  ggplot(aes(x = season,y = n,col = squadNickname,group = squadNickname)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = squadPrimaryCols)

player_interactions %>% 
  arrange(season,squadId, crosses) %>%
  group_by(squadId, crosses) %>% 
  mutate(accum = cumsum(n)) %>%
  ungroup() %>% 
  count(squadId,season,wt = accum) %>%
  inner_join(team_info) %>% 
  group_by(squadId) %>% 
  mutate(diff = (n - lag(n))/lag(n),
         diff = replace_na(diff,0)) %>% 
  ggplot(aes(x = season,y = diff,col = squadNickname,group = squadNickname)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = squadPrimaryCols)


player_interactions %>% 
  arrange(season,squadId, crosses) %>%
  group_by(crosses) %>% 
  mutate(accum = cumsum(n)) %>% filter(season == 2017,squadId == "807") %>% 
  janitor::adorn_totals()

`2017_data` %>% 
  map_dfr(~.x[["playerStats"]][["player"]]) %>% 
  distinct(squadId,playerId) %>% 
  left_join(`2017_data` %>% 
              map_dfr(~.x[["playerInfo"]][["player"]]) %>% 
              distinct(playerId,displayName) %>% 
              group_by(playerId) %>% 
              slice(1) %>% 
              ungroup(),by = "playerId") %>% 
  arrange(squadId, displayName) %>% 
  left_join(team_info) %>% View()

season_2022 <- players_2022 %>% 
  group_by(squadId) %>% 
  summarise(crosses = cross2(playerId,playerId,.filter = function(x, y) x >= y)  %>%  map(~paste(.x,collapse = "-")) %>% unlist()) %>% 
  ungroup() %>% 
  count(squadId,crosses) %>% 
  tidyr::extract(crosses,c("player1","player2"),"([[:alnum:]]+)-([[:alnum:]]+)",remove = F) %>% 
  mutate(across(c(player1,player2),as.integer)) %>% 
  inner_join(select(players_2022,playerId,displayName), by = c("player1" = "playerId")) %>% 
  inner_join(select(players_2022,playerId,displayName), by = c("player2" = "playerId")) %>% 
  mutate(n = 0, season = 2022)



# -------------------------------------------------------------------------
# Last playing year
crosses_810 <- player_interactions %>% 
  bind_rows(season_2022) %>% 
  filter(squadId == 810) %>% 
  distinct(crosses) %>% pull(crosses)

# Combined experience by cross
fever_network <- player_interactions %>% 
  bind_rows(season_2022) %>% 
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
  mutate(total_combined = if_else(total_combined == 0,NA_real_,total_combined))

network <- tbl_graph(nodes = nodes, edges = edges)

ggraph(network, layout = 'kk') + 
  geom_edge_link(aes(edge_width = total_combined,edge_colour = total_combined),
                 arrow = arrow(length = unit(2, 'mm')), 
                 start_cap = circle(2, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = 'along',
                 label_dodge = -unit(2.5, 'mm'),
                 label_push = unit(10, 'mm')) + 
  geom_node_point(size = 15,aes(col = last_season)) +
  geom_node_text(aes(label = playerName), repel = F,col = "white")


# Example -----------------------------------------------------------------



nodes <- tibble(names = c("Dave","Shaz","NVB"),
                ID = 1:3)

edges <- tibble(names1 = c("Dave","Dave","NVB"),
                names2 = c("Shaz","NVB","Shaz"),
                ID = 1:3,n = c(1,1,14))


# Transfer of minutes played

network <- tbl_graph(nodes = nodes, edges = edges)

  ggraph(network, layout = 'kk') + 
  geom_edge_link(aes(label = n),
                 arrow = arrow(length = unit(2, 'mm')), 
                 start_cap = circle(2, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = 'along',
                 label_dodge = -unit(2.5, 'mm'),
                 label_push = unit(10, 'mm')) + 
  geom_node_point(size = 15) +
  geom_node_text(aes(label = names), repel = F,col = "white")
