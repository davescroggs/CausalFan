library(tidyverse)
library(scales)

`2017_data` <- readRDS("data/2017_data.RDS")
`2018_data` <- readRDS("data/2018_data.RDS")
`2019_data` <- readRDS("data/2019_data.RDS")
`2020_data` <- readRDS("data/2020_data.RDS")
`2021_data` <- readRDS("data/2021_data.RDS")
`2017_data_finals` <- readRDS("data/2017_finals_data.RDS")
`2018_data_finals` <- readRDS("data/2018_finals_data.RDS")
`2019_data_finals` <- readRDS("data/2019_finals_data.RDS")
`2020_data_finals` <- readRDS("data/2020_finals_data.RDS")
`2021_data_finals` <- readRDS("data/2021_finals_data.RDS")

team_info <- `2020_data` %>% 
  map_dfr(~.x[["teamInfo"]][["team"]]) %>% 
  distinct(squadNickname,squadId,squadName)

pos_lvls <- c("GS","GA","WA","C","WD","GD","GK","I")

player_info <- map_dfr(list(`2017_data`,`2018_data`,`2019_data`, `2020_data`,`2021_data`,`2017_data_finals`,`2018_data_finals`,`2019_data_finals`,`2020_data_finals`,`2021_data_finals`),function(dat) dat %>% 
  map_dfr(~.x[["playerInfo"]][["player"]])) %>% 
  distinct(playerId,displayName) %>% 
  group_by(playerId) %>% 
  summarise(displayName = last(displayName))

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
  "No Team",       "#999999", "#595959") %>% 
  select(-Secondary) %>% 
  deframe()

process_cpr <- function(dat) {
  
  player_info <- dat %>% 
    map_dfr(~.x[["playerInfo"]][["player"]]) %>% 
    distinct(playerId,displayName)
  
  player_interactions <-
    tibble(round = map_int(dat,  ~ .x[["matchInfo"]][["roundNumber"]]),
      match = map_int(dat,  ~ .x[["matchInfo"]][["matchNumber"]]),
      season = map_int(dat,  ~ as.integer(lubridate::year(.x[["matchInfo"]][["utcStartTime"]]))),
      playerId = map(dat,  ~ .x[["playerStats"]][["player"]])) %>% 
    unnest_auto("playerId") %>% unnest_auto("playerId") %>% 
    select(season,round,match,playerId,centrePassReceives,squadId) %>% 
    left_join(player_info)
}

temp <- map_dfr(list(`2017_data`,`2018_data`,`2019_data`, `2020_data`,`2021_data`,`2017_data_finals`,`2018_data_finals`,`2019_data_finals`,`2020_data_finals`,`2021_data_finals`),process_cpr)

plot_dat <- temp %>% 
  inner_join(team_info) %>% 
  arrange(playerId,season,round) %>% 
  group_by(playerId) %>% 
  mutate(cum_cpr = cumsum(centrePassReceives), max_cpr = max(cum_cpr),
         cum_goals = cumsum(goals), max_goals = last(cum_goals),
         cum_feeds = cumsum(feeds), max_feeds = last(cum_feeds),
         displayName = last(displayName)) %>% 
ungroup() %>% 
  mutate(game = (season - 2017) * max(round) + round)

final_point_lab <- plot_dat %>% 
  group_by(playerId) %>% 
  filter(game == max(game)) %>% 
  slice(1) %>% 
  ungroup()

plot_dat %>% 
  filter(dense_rank(desc(max_cpr)) <= 10) %>%
  ggplot(aes(x = game, y = cum_cpr,group = displayName,col = factor(squadNickname))) +
  geom_point() +
  geom_path() +
  ggrepel::geom_label_repel(data = final_point_lab %>% filter(dense_rank(desc(max_cpr)) %in% seq(2,8,2)),
                            aes(label = displayName),show.legend = F,nudge_x = -30,nudge_y = 30,col = "black",
                            family = "Courier") +
  ggrepel::geom_label_repel(data = final_point_lab %>% filter(dense_rank(desc(max_cpr)) %in% c(10,seq(1,9,2))),
                            aes(label = displayName),show.legend = F,nudge_x = 30,nudge_y = -40,col = "black",
                            family = "Courier") +
  scale_color_manual(values = squadCols) +
  scale_y_continuous(breaks = seq(0,1500,250)) +
  scale_x_continuous(breaks = seq(0,85,17)) +
  labs(title = "Cumulative centre pass receives",
       subtitle = "Top 10 total centre pass receives by individual players in SSN era",
       x = "SSN Game",
       y = "Cumulative centre pass receives",
       col = "Team",
    caption = "Data: Champion Data") +
  expand_limits(x = c(0,120)) +
  theme_bw() +
  theme(plot.background = element_rect(colour = "black"),
        panel.grid.major = element_line(colour = "black"),
        panel.background = element_rect(fill = "#FFE7BA",colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


temp %>% 
  inner_join(team_info) %>% 
  arrange(playerId,-season) %>% 
  group_by(playerId,season) %>% 
  summarise(games = n(),
            season_cpr = sum(centrePassReceives),
            max_cpr = max(centrePassReceives),
            rate_cpr = season_cpr/games,
            displayName = displayName[1]) %>% 
  mutate(total_cpr = sum(season_cpr)) %>% 
  ungroup() %>% 
  mutate(rank_rate = dense_rank(desc(max_cpr)),
         max_cpr = paste(max_cpr," (",ordinal(rank_rate),")",sep = "")) %>% 
  arrange(-rate_cpr) %>% 
  transmute(`#` = 1:n(),player = displayName,season,games,`season cpr` = season_cpr,`max cpr` = max_cpr,`cpr per game` = round(rate_cpr,digits = 1),`SSN cpr` = total_cpr) %>%
  janitor::clean_names(case = "sentence") %>% 
  filter(Number %in% c(1:10,14)) %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic_2()

# Starting Positions ------------------------------------------------------

process_sp <- function(dat) {
  
  player_info <- dat %>% 
    map_dfr(~.x[["playerInfo"]][["player"]]) %>% 
    distinct(playerId,displayName)
  
  player_interactions <-
    tibble(round = map_int(dat,  ~ .x[["matchInfo"]][["roundNumber"]]),
           match = map_int(dat,  ~ .x[["matchInfo"]][["matchNumber"]]),
           season = map_int(dat,  ~ as.integer(lubridate::year(.x[["matchInfo"]][["utcStartTime"]]))),
           playerId = map(dat,  ~ .x[["playerPeriodStats"]][["player"]])) %>% 
    unnest_auto("playerId") %>% unnest_auto("playerId") %>% 
    select(season,round,match,playerId,startingPositionCode,squadId) %>% 
    left_join(player_info)
}

starting_pos <- map_dfr(list(`2017_data`,`2018_data`,`2019_data`, `2020_data`,`2021_data`,`2017_data_finals`,`2018_data_finals`,`2019_data_finals`,`2020_data_finals`,`2021_data_finals`),process_cpr)

starting_pos %>% 
  filter(playerId %in% c(80296,80105,80475)) %>%
  group_by(season,displayName,startingPositionCode) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n),
         pct_label = scales::percent(pct,1),
         startingPositionCode = factor(startingPositionCode,levels = pos_lvls,ordered = T)) %>% 
  select(-n) %>% 
  arrange(displayName,startingPositionCode) %>% 
  ggplot(aes(x = pct, y = displayName,fill = startingPositionCode)) +
  geom_col() +
  geom_text(aes(label = pct_label),position = position_stack(vjust = 0.5),check_overlap = T) +
  facet_wrap(~season, ncol = 1) +
  scale_fill_brewer(palette =  "Set1") +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Player starting position proportion",
       subtitle = "Percentage of time the player started a quarter in a given position",
       x = "Proportion of time starting the quarter in each position",
       y = "Season",
       fill = "Starting position",
       caption = "Data: Champion Data") +
  theme(plot.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# Goals -------------------------------------------------------------------


goals_df <- map_dfr(list(
  `2017_data`,
  `2018_data`,
  `2019_data`,
  `2020_data`,
  `2021_data`,
  `2017_data_finals`,
  `2018_data_finals`,
  `2019_data_finals`,
  `2020_data_finals`,
  `2021_data_finals`
), function(df)
  df %>% map_dfr(~superNetballR::tidyMatch(.x) %>% superNetballR::matchPoints()))

goals_df %>%
  #filter(round > 14) %>% 
  group_by(squadId) %>% 
  summarise(n = n(),
            goals_tot = sum(goals),
            squadName = last(squadName)) %>% View()

(temp %>% 
    inner_join(team_info) %>% 
  group_by(round,match,season) %>% 
  summarise(cpr = sum(centrePassReceives),
            gls = sum(goals),
            squadNickname = paste(unique(sort(squadNickname)),collapse = " v "),
            lab = paste(last(season),"R:",last(round),collapse = " ")) %>%
  ggplot(aes(cpr,gls,label = lab,col = squadNickname)) + 
    geom_jitter(alpha = 0.5) + 
    geom_abline(slope = 1) +
    coord_equal()) %>% plotly::ggplotly()


starting_pos %>% 
  filter(playerId %in% c(993360)) %>% View()
group_by(season,displayName,startingPositionCode) %>% 
  summarise(n = n()) %>% 