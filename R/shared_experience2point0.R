library(tidyverse)
library(ggraph)
library(tidygraph)
library(superNetballR)
library(patchwork)

# Accumulated shared player games -----------------------------------------

`2009_data` <- readRDS("data/2009_data.RDS")
`2010_data` <- readRDS("data/2010_data.RDS")
`2011_data` <- readRDS("data/2011_data.RDS")
`2012_data` <- readRDS("data/2012_data.RDS")
`2013_data` <- readRDS("data/2013_data.RDS")
`2014_data` <- readRDS("data/2014_data.RDS") %>% 
  keep(function(x) vec_depth(x[["playerStats"]]) > 2)
`2015_data` <- readRDS("data/2015_data.RDS")
`2016_data` <- readRDS("data/2016_data.RDS")
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

all_years = list(`2009_data`,`2010_data`,`2011_data`,`2012_data`,`2013_data`,`2014_data`,`2015_data`,`2016_data`,`2017_data`,`2019_data`,`2018_data`,`2020_data`,`2021_data`)
# Table of team IDs
team_info <- map_dfr(all_years,function(dat){
                 map_dfr(dat,~.x[["teamInfo"]][["team"]])
}) %>% 
  distinct(squadId,.keep_all = T)



# Process files -----------------------------------------------------------

process_squads <- function(dat){
  
  season <- lubridate::year(dat[[1]][["matchInfo"]][["utcStartTime"]])
  print(season)
  
  player_info <- dat %>% 
    map_dfr(~.x[["playerStats"]][["player"]]) %>% 
    distinct(squadId,playerId)
  #43
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

player_interactions <- map_dfr(all_years,process_squads)


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
  inner_join(team_info)


p1 <- plot_data %>% 
  ggplot(aes(x = season,y = n,col = squadNickname,group = squadNickname)) +
  geom_point(size = 3) +
  geom_line(size = 1.3) +
  #scale_color_manual(values = squadPrimaryCols) +
  scale_x_continuous(breaks = 2009:2021) +
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

plotly::ggplotly(p1)
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
    # inner_join(team_info %>% add_row(squadName = "Magpies Netball",
    #                                 squadId = 8119,
    #                                 squadNickname = "Magpies")) %>% 
    transmute(pos = 1:n(), squadId,season = season)
  
}

season_ladders <- map_dfr(all_years,get_ladders)

p2_data <- plot_data %>% 
  arrange(season,-n) %>% 
  group_by(season) %>% 
  mutate(most_exp = factor(1:n())) %>% 
  ungroup() %>% 
  left_join(season_ladders) %>%
  arrange(squadNickname,season)

p2_data %>% 
  ggplot(aes(x = (pos),(most_exp))) +
  geom_point() + geom_smooth(method = "lm")

p2_data %>% 
  ggplot(aes(season,most_exp,col = squadNickname,group = squadNickname)) +
  geom_point(size = 10) +
  geom_line(size = 1.5) +
  geom_text(aes(label = scales::ordinal(pos)),col = "white") +
  #geom_text(data = p2_data %>% filter(season == 2022),label = "?",col = "white") +
  scale_y_discrete(limits = str_sort(levels(p2_data$most_exp),numeric = T,decreasing = T)) +
  #scale_colour_manual(values = squadPrimaryCols) +
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
