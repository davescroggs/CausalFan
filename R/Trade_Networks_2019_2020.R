# Load libraries
library(tidyverse)
library(superNetballR)
library(tidygraph)
library(ggraph)

# Load reference and season data ----

`2020_data` <- readRDS("data/2020_data.RDS")
`2019_data` <- readRDS("data/2019_data.RDS")

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

squadSecondaryCols <- squadCols %>%   
  select(-Primary) %>% 
  deframe()

# Table of team abbreviations
name_abrv <- tibble::tribble(
  ~squadNickname, ~abrv,
  "Fever", "FEV",
  "Firebirds", "FIR",
  "GIANTS", "GIA",
  "Lightning", "LIG",
  "Magpies", "MAG",
  "No Team", "NO",
  "Swifts", "SWI",
  "Thunderbirds", "THU",
  "Vixens", "VIX"
)



# Table of team IDs
team_info <- `2020_data` %>% 
  map_dfr(~.x[["teamInfo"]][["team"]]) %>% 
  distinct(squadNickname,squadId)

# Table of player IDS
player_info <- c(`2020_data`,`2019_data`) %>% 
  map_dfr(~.x[["playerInfo"]][["player"]]) %>% 
  select(playerId,displayName) %>% 
  distinct(playerId,.keep_all = T)

# Extract data ------------------------------------------------------------
# Extract player stats
players_2020 <- `2020_data` %>% 
  map_dfr(~.x[["playerStats"]][["player"]]) %>% 
  count(playerId,squadId,wt = minutesPlayed,name = "minutesPlayed") %>% 
  left_join(team_info, by = "squadId") %>% 
  left_join(player_info, by = "playerId") %>% 
  mutate(season = "2020")

# Extract player stats
players_2019 <- `2019_data` %>% 
  map_dfr(~.x[["playerStats"]][["player"]]) %>% 
  count(playerId,squadId,wt = minutesPlayed,name = "minutesPlayed") %>% 
  left_join(team_info, by = "squadId") %>% 
  left_join(player_info, by = "playerId") %>% 
  mutate(season = "2019")


# Create network diagrams --------------------------------------------------

## Trade network summary data
trade_network <- full_join(players_2019,players_2020,
                           by = "playerId",
                           suffix = c("_2019","_2020")) %>%
  mutate(across(c(squadNickname_2019,squadNickname_2020),~if_else(is.na(.x),"No Team",.x))) %>% 
  mutate(displayName_2019 = if_else(is.na(displayName_2019),displayName_2020,displayName_2019)) %>% 
  group_by(squadNickname_2019,squadNickname_2020) %>% 
  summarise(players = list(displayName_2019),
            trades = length(displayName_2019),
            minutes = sum(minutesPlayed_2019,na.rm = T)) %>% 
  ungroup()

nodes <- trade_network %>%
  distinct(squadNickname_2019) %>% 
  rename("squadNickname" = "squadNickname_2019") %>% 
  mutate(ID = 1:n()) %>% 
  left_join(name_abrv)

edges <- trade_network %>% 
  left_join(nodes,by = c("squadNickname_2019"="squadNickname")) %>% 
  rename("from" = "ID") %>% 
  left_join(nodes,by = c("squadNickname_2020"="squadNickname")) %>% 
  rename("to" = "ID") %>% 
  select(from,to,minutes,trades) %>% 
  # Manually set the direction of the edge loops
  left_join(tribble(
  ~abrv, ~direction,
  "FIR",       0L,
  "FEV",         0L,
  "VIX",        45L,
  "LIG",        90L,
  "SWI",       110L,
  "GIA",       179L,
  "MAG",       220L,
   "NO",       270L,
  "THU",       270L
  ) %>% 
  left_join(nodes),by = c("to" = "ID"))


# Transfer of minutes played

network_minutes <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

ggraph(network_minutes, layout = 'stress', circular = TRUE) + 
  geom_edge_parallel(aes(label = minutes),
                     arrow = arrow(length = unit(2, 'mm')), 
                     start_cap = circle(2, 'mm'),
                     end_cap = circle(5, 'mm'),
                     angle_calc = 'along',
                     label_dodge = -unit(2.5, 'mm'),
                     label_push = unit(10, 'mm')) +
  geom_edge_loop(aes(label = minutes,direction = direction),
                 arrow = arrow(length = unit(2, 'mm')), 
                 start_cap = circle(2, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = 'along',
                 label_dodge = -unit(2.5, 'mm'),
                 force_flip = T) +
  geom_node_point(size = 10,aes(col = squadNickname)) + 
  geom_node_text(aes(label = abrv), repel = F,col = "white") +
  coord_fixed() +
  scale_color_manual(values = squadPrimaryCols)

# Number of trades 

ggraph(network_trades, layout = 'stress', circular = TRUE) + 
  geom_edge_parallel(aes(label = trades),
                     arrow = arrow(length = unit(2, 'mm')), 
                     start_cap = circle(2, 'mm'),
                     end_cap = circle(5, 'mm'),
                     angle_calc = 'along',
                     label_dodge = -unit(2.5, 'mm'),
                     label_push = unit(10, 'mm')) +
  geom_edge_loop(aes(label = trades,direction = direction),
                 arrow = arrow(length = unit(2, 'mm')), 
                 start_cap = circle(2, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = 'along',
                 label_dodge = -unit(2.5, 'mm'),
                 force_flip = T) +
  geom_node_point(size = 10,aes(col = squadNickname)) + 
  geom_node_text(aes(label = abrv), repel = F,col = "white") +
  coord_fixed() +
  scale_color_manual(values = squadPrimaryCols)


# To
bind_rows(trade_network %>% 
            filter(squadNickname_2019 != squadNickname_2020) %>% 
            rename(squadNickname = squadNickname_2020) %>% 
            count(squadNickname,wt = minutes) %>% 
            mutate(result = "To"),
          
          # From
          trade_network %>% 
            filter(squadNickname_2019 != squadNickname_2020) %>% 
            rename(squadNickname = squadNickname_2019) %>% 
            count(squadNickname,wt = minutes) %>% 
            mutate(result = "From"),
          
          # Retained
          retained <- trade_network %>% 
            filter(squadNickname_2019 == squadNickname_2020) %>% 
            rename(squadNickname = squadNickname_2020) %>% 
            count(squadNickname,wt = minutes) %>% 
            mutate(result = "Retained")) %>% 
  mutate(minutes = n,
         result = factor(result,levels = c("To","From","Retained"),ordered = T)) %>%
  arrange(result,minutes) %>% 
  ggplot(aes(x = minutes,y = fct_reorder2(squadNickname,result,-minutes),fill = result)) +
  geom_col(position = "dodge") +
  tidytext::scale_y_reordered() + theme_bw()




full_join(players_2019,players_2020,
          by = "playerId",
          suffix = c("_2019","_2020")) %>%
  mutate(across(c(squadNickname_2019,squadNickname_2020),~if_else(is.na(.x),"No Team",.x))) %>% 
  mutate(displayName_2019 = if_else(is.na(displayName_2019),displayName_2020,displayName_2019)) %>% 
  filter(squadNickname_2020 == "No Team") %>% 
  arrange(-minutesPlayed_2019) %>% 
  head(10) %>% 
  select(Player = displayName_2019,squad = squadNickname_2019,`Total season minutes played` = minutesPlayed_2019)


ladders <- tibble::tribble(
  ~position, ~Squad, ~Season,
  1L, "Lightning",  "2019",
  2L,   "Swifts",  "2019",
  3L,         "Vixens",  "2019",
  4L,      "Magpies",  "2019",
  5L,           "GIANTS",  "2019",
  6L,         "Fever",  "2019",
  7L,    "Thunderbirds",  "2019",
  8L,     "Firebirds", "2019",
  1L,         "Vixens", "2020",
  2L, "Lightning", "2020",
  3L,         "Fever", "2020",
  4L,   "Swifts", "2020",
  5L,     "Firebirds", "2020",
  6L,           "GIANTS", "2020",
  7L,    "Thunderbirds", "2020",
  8L,      "Magpies", "2020"
) 

ggplot(ladders,aes(x = Season,y = position,col = Squad,group = Squad)) +
  geom_point(size = 10) +
  geom_line(size = 1) +
  geom_text(aes(label = position),size = 6,col = "white") +
  geom_label(data = ladders %>% filter(Season == "2020"),aes(label = Squad,fill = Squad)
             ,nudge_x = 0.1,
             size = 5,
             hjust = 0) +
  scale_y_reverse(breaks = 1:8) +
  scale_color_manual(values = squadPrimaryCols) +
  scale_fill_manual(values = squadSecondaryCols) +
  theme_minimal() +
  labs(title = "Final Ladder Position",
       y = "") +
  theme(panel.grid = element_blank(),
        #plot.margin = margin(0.1, 0, 0.1, 0.1, unit = "cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 13),
        panel.grid.major.x = element_line(colour = "grey"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5,size = 20),
        axis.title.x = element_text(size = 17))
 

left_join(edges %>% 
  filter(from != to) %>% 
  count(to,wt= trades,name = "inward"),
edges %>% 
  filter(from != to) %>% 
  count(from,wt= trades,name = "outward"),by = c("to" = "from")) %>% 
  mutate(diff = inward - outward) %>% 
  left_join(nodes,by = c("to" = "ID")) %>% 
  select(squadNickname,inward,outward,diff)
