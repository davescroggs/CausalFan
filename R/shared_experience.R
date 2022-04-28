library(tidyverse)
library(ggraph)
library(tidygraph)
library(superNetballR)
library(patchwork)

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

# Simple Example ------------------------------------------------------------

nodes <- tibble(names = c("Cath","Dave","Liz","Maud","Liz Jnr"),
                ID = 1:5,
                last_ssn = c("2021","2020","2021","2020","2021"))

edges <- tibble(names = c(rep(list(names = c("Cath","Dave","Liz")),12),
                 rep(list(names = c("Cath","Dave","Maud")),2),
                 rep(list(names = c("Cath","Liz","Liz Jnr")),14)),
       season = as.character(rep(2020:2021,each = 14)),
       round = rep(1:14,2)) %>% 
  unnest(names) %>% 
  group_by(round,season) %>% 
  summarise(crosses = cross2(names,names,.filter = function(x, y) x >= y)  %>%  map(~paste(.x,collapse = "-")) %>% unlist()) %>% 
  ungroup() %>% 
  count(season,crosses) %>% 
  tidyr::extract(crosses,c("player1","player2"),"([[:alnum:]]+)-(.*)",remove = F) %>% 
  left_join(nodes %>% rename("to" = "ID") %>% select(-last_ssn),by = c("player1" = "names")) %>% 
  left_join(nodes %>% rename("from" = "ID") %>% select(-last_ssn),by = c("player2" = "names")) %>% 
  group_by(crosses)

cum_edges <-edges %>%
  group_by(crosses) %>%
  summarise(season = max(season),
    to = to[1],
    from = from[1],
    player1 = player1[1],
    player2 = player2[1],
    n = sum(n))

network <- tbl_graph(nodes = nodes, edges = cum_edges)

p1 <- ggraph(network, layout = 'circle') + 
  geom_edge_link(aes(label = n,col = season),
                 angle_calc = 'along',
                 #label_push = unit(9,'mm'),
                 label_dodge = unit(2.5, 'mm'),
                 label_colour = NA,
                 show.legend = F) +
  geom_node_point(aes(col = last_ssn),size = 15) +
  geom_node_text(aes(label = names), repel = F,col = "white") +
  coord_equal() +
  expand_limits(x = c(-1.25,1.25),y = c(-1.25,1.25)) +
  scale_edge_colour_manual(values = c("2020" = "grey60","2021" = "black")) +
  scale_colour_manual(values = c("2020" = "grey60","2021" = "black")) +
  labs(col = "Last season played") 

p2 <- edges %>% 
  ggplot(aes(x= season,y = n,fill = fct_reorder2(crosses,as.numeric(season),desc(n)))) +
  geom_col() +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) + 
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_viridis_d() +
  theme_minimal() +
  scale_y_continuous(breaks = c(seq(0,40,10),42)) +
  labs(y = "Shared experience",
       x = "Season",
       fill = "Player Pairs") +
  theme(plot.background = element_rect(fill = "#EBEBEB"))

p1 + p2 + plot_layout(widths = c(4,1)) +
  plot_annotation(title = "Still Got It Team Experience",
          subtitle = "Shared experience over the 2020/21 Bridge Season") &
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Fevers network -------------------------------------------------------------
# Last playing year
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
  geom_node_point(size = 20,aes(col = last_season)) +
  geom_node_text(aes(label = playerName), repel = F,col = "white",size = 3) +
  scale_edge_color_continuous(breaks = seq(0,70,10)) +
  scale_color_brewer(palette = "RdYlGn",direction = -1,
                     guide = guide_legend(override.aes=aes(size=8))) +
  scale_edge_width(guide = "none") +
  expand_limits(y = c(-3.5,2))

# Cumulative SE -------------------------------------------------------------------

# High combined experience
player_interactions %>% count(crosses,wt= n) %>% 
  arrange(-n,crosses) %>% 
  left_join(player_interactions %>%
              distinct(crosses,displayName.x,displayName.y),by = "crosses") %>% View()

# Combined experience start 2017 - end 2022
plot_data <- player_interactions %>% 
  bind_rows(season_2022) %>% 
  arrange(season,squadId, crosses) %>% 
  group_by(crosses) %>% 
  mutate(accum = cumsum(n)) %>%
  ungroup()  %>% 
  count(squadId,season,wt = accum) %>%
  inner_join(team_info) %>% 
  bind_rows(team_info %>% mutate(season = 2016,n = 0))  %>%
  mutate(projection = if_else(season == 2022,T,F))


plot_data %>% 
  ggplot(aes(x = season,y = n,col = squadNickname,group = squadNickname)) +
  geom_point(size = 3) +
  geom_line(data = plot_data %>% filter(season != 2022),size = 1.3) +
  geom_line(data = plot_data %>% filter(season %in% c(2021,2022)),linetype = 2,size = 1.3) +
  scale_color_manual(values = squadPrimaryCols) +
  scale_x_continuous(breaks = 2016:2022) +
  theme_bw() + 
  labs(x = "Season",
       y = "End of season SE",
       colour = "Squad",
       title = "Cumulative Team Shared Experience",
       subtitle = "Each team's rise in experience over since SSN inception") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#EDEDED"),
        legend.background = element_rect(colour = "black"))


# Ladder position ---------------------------------------------------------


get_ladders <- function(dat){
  
  season <- lubridate::year(dat[[1]][["matchInfo"]][["utcStartTime"]]) %>% as.integer()
  
  dat %>%
    map_dfr(tidyMatch) %>% 
    ladders() %>% 
    {if (season %in% 2018:2019) {
      arrange(., -points_new,-(goals_for/goals_against))
      }else {
        arrange(., -points,-(goals_for/goals_against))}} %>% 
    #{if (season %in% 2018:2019) . else .} %>% 
    left_join(team_info %>% add_row(squadName = "Magpies Netball",
                                    squadId = 8119,
                                    squadNickname = "Magpies")) %>% 
    transmute(pos = 1:n(), squadId,season = season)
  
}

season_ladders <- map_dfr(list(`2017_data`,`2018_data`,`2019_data`, `2020_data`,`2021_data`),get_ladders)

p2_data <- plot_data %>% 
  arrange(season,-n) %>% 
  group_by(season) %>% 
  mutate(most_exp = 1:n()) %>% 
  ungroup() %>% 
  left_join(season_ladders) %>%
  arrange(squadNickname,season) %>%
  filter(season > 2017)

p2_data %>% 
  ggplot(aes(season,as.character(most_exp),col = squadNickname,group = squadNickname)) +
  geom_point(size = 10) +
  geom_line(data = p2_data %>% filter(season != 2022),size = 1.5) +
  geom_line(data = p2_data %>% filter(season %in% c(2021,2022)),linetype = 2,size = 1.5) +
  #geom_line(size = 1.5) +
  geom_text(data = p2_data %>% filter(season != 2022),aes(label = scales::ordinal(pos)),col = "white") +
  geom_text(data = p2_data %>% filter(season == 2022),label = "?",col = "white") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = squadPrimaryCols) +
  theme_bw() +
  labs(colour = "Squad",
       title = "Final ladder position vs. Total SE rank\n",
       subtitle = "Cumulative SE rank is right axis and position on plot\nFinal ladder position is the number on each point",
       x = "Season end",
       y = "Shared experience rank",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#EDEDED"),
        legend.background = element_rect(fill = "#EDEDED",colour = "black"))
