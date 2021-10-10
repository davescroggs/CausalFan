library(tidyverse)

game_lists <- list(
  #`2017_data`,
                   #`2018_data`,
                   #`2019_data`,
                   `2020_data`
                   #`2021_data`
)

convert_season <- function(x){
  x <- str_sub(x,1,5)
  
  switch (x,
          "10083" = "Season 2017",
          "10393" = "Season 2018",
          "10724" = "Season 2019",
          "11108" = "Season 2020",
          "11391" = "Season 2021"
  )
}           

filter_if_null_all <- function(df,df_col,cond){
  
  print(is.null(cond) || (exception == "All"))
  df %>% 
    {if (is.null(cond) || (cond == "All")) . else filter(.,{{df_col}} %in% cond) }
}

do_subs <- function(data) {
  season = deparse(substitute(data))
  tibble(
    x = data %>%
      map(safely( ~ .x[["playerSubs"]][["player"]])) %>% map("result"),
    round = data %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
    match = data %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
    season = data %>% map_chr(~.x[["matchInfo"]][["matchId"]]) %>% map_chr(convert_season)) %>%
    unnest_longer("x") %>% unnest_wider("x") %>%
    left_join(data %>%
                map_dfr( ~ .x[["teamInfo"]][["team"]]) %>% distinct(squadNickname, squadId),
              by = "squadId")
}

do_starting_pos <- function(data){
  tibble(x = data %>% 
           map(~.x[["playerPeriodStats"]][["player"]]),
         round = data %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
         match = data %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
         season = data %>% map_chr(~.x[["matchInfo"]][["matchId"]]) %>% map_chr(convert_season)) %>% 
    unnest_longer("x") %>% unnest_wider("x") %>% 
    left_join(data %>% 
                map_dfr(~.x[["teamInfo"]][["team"]]) %>% distinct(squadNickname,squadId),
              by = "squadId") %>%
    select(playerId,round,match,squadNickname,startingPositionCode,period,season)
}

do_player_info <- function(data){
  tibble(x = data %>% 
           map(~.x[["playerInfo"]][["player"]]),
         round = data %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
         match = data %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
         season = data %>% map_chr(~.x[["matchInfo"]][["matchId"]]) %>% map_chr(convert_season)) %>% 
    unnest_longer("x") %>% unnest_wider("x")
}

subs <- map_dfr(game_lists,do_subs)

starting_pos <- map_dfr(game_lists,do_starting_pos)

playerInfo <- map_dfr(game_lists,do_player_info)



add_player_names <- function(df){
  df %>% 
    left_join(playerInfo %>% distinct(playerId,displayName),by = "playerId")
}

positions_per_time <- bind_rows(
  # starting positions
  starting_pos %>% 
    mutate(periodSeconds = 0) %>% 
    rename(position = startingPositionCode),
  # Substitutions
  subs %>%
    mutate(periodSeconds = if_else(periodSeconds == 0, as.integer(1),periodSeconds)) %>% 
    select(season,playerId,round,match,squadNickname,position = toPos,period,periodSeconds)) %>% 
  arrange(season,round,match,squadNickname,playerId,period,periodSeconds) %>%
  group_by(season,squadNickname,playerId,match,round,period) %>% 
  mutate(position = if_else(position == "S","I",position),
         time_on = lead(periodSeconds),
         time_on = if_else(is.na(time_on),900,time_on)) %>%
  ungroup() %>% 
  add_player_names()

total_rounds <- positions_per_time %>% 
  group_by(season) %>% 
  summarise(total_rounds = max(round))

pre_plot_data <- positions_per_time %>% 
  #filter_if_null_all(season,season_selector) %>% 
  #filter_if_null_all(squadNickname,team_input) %>% 
  #filter_if_null_all(displayName,pos_selector) %>% 
  filter(period < 5)

df_custom <- function(x){
  pre_plot_data %>% 
    transmute("minute_{x}" := if_else(periodSeconds <= x*60 & time_on > x*60 & time_on - x*60 > 30,1,0))
}

plot_data <- map_dfc(0:14,df_custom) %>% 
  bind_cols(pre_plot_data,.) %>% 
  pivot_longer(cols = minute_0:minute_14) %>%
  group_by(season,squadNickname,displayName,period,position,name) %>%
  summarise(n = sum(value),
            .groups = "drop_last") %>%
  left_join(total_rounds,by = "season") %>%
  mutate(name = str_extract(name,"\\d+"),
         name = as.integer(name),
         displayName = as.character(displayName),
         nn = sum(n),
         n = n/total_rounds,
         position = factor(position,levels = c("I","GK","GD","WD","C","WA","GA","GS"),ordered = T)) %>% 
  filter(n > 0)



plot_data %>% 
  filter(squadNickname == "Fever") %>% 
  mutate(period = paste("Qtr - ",period,sep = "")) %>% 
  ggplot(aes(x = name,y = position,fill = n)) + 
  geom_tile() +
  scale_x_continuous(breaks = 0:14) +
  #scale_fill_distiller(palette = "Greens",direction = 1,labels = scales::percent,limits = c(0,1)) +
  scale_fill_gradient2(high = '#d7191c',mid = '#ffffbf', low = '#1a9641',labels = scales::percent,limits = c(0,1),midpoint = 0.5) +
  labs(x = "Minutes",
       y = "",
       fill = "% Time on court") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.x.bottom = element_line(),
        axis.ticks.y.left = element_line()) +
  facet_grid(displayName~period)


# Selectors ---------------------------------------------------------------

# Seasons
season_selector <- "Season 2020"
team_input <- c(NULL)
pos_input <- c(NULL)
player_input <- "C.Bassett"


season_selector <- c("Season 2017","Season 2018", "Season 2019","Season 2020","Season 2021")

# Positions
#pos_selector <- pre_plot_data %>% 
  

# Team
pull_distinct <- function(df,col){
  df %>% 
    distinct({{ col}}) %>% 
    pull({{ col }})
}

team_selector <- pull_distinct(pre_plot_data, squadNickname)

# Player
# WRONG
player_selector <- pull_distinct(pre_plot_data, displayName)






# NBA Subs - Positive Risidual --------------------------------------------

name_order <- pre_plot_data %>% 
  group_by(displayName,round) %>% 
  # summarise(n = n()) %>% 
  # arrange(-n) %>% 
  filter(period == 1) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(pos = case_when(
    position == "GS" ~ 1,
    position == "GA" ~ 2,
    position == "WA" ~ 3,
    position == "C" ~ 4,
    position == "WD" ~ 5,
    position == "GD" ~ 6,
    position == "GK" ~ 7,
    position == "I" ~ 8,
    TRUE ~ 9
  )) %>%
  select(displayName,round,pos)
  
plot_cols <- tibble(position = c("I","GS","GA","WA","C","WD","GD","GK"),
                    col = c("#a1a1a1","#f94144","#f3722c","#f8961e","#f9c74f","#90be6d","#43aa8b","#577590")) %>% 
  deframe()

pre_plot_data %>%
  left_join(name_order,by = c("displayName","round")) %>% 
  mutate(id = 1:n(),
         periodSeconds = periodSeconds/60,
         time_on = time_on/60) %>% 
  pivot_longer(cols = c(periodSeconds,time_on)) %>% 
  filter(round == 3,squadNickname == "Fever") %>% 
  ggplot(aes(y = fct_reorder(displayName,-pos),x = value, col = fct_relevel(position,c("I","GK","GD","WD","C","WA","GA","GS")))) +
  geom_line(aes(group = id),size = 8,alpha = 1) +
  #geom_point(size = 5,shape = "|",col = "black") +
  facet_grid(.~paste("Qtr",period)) +
  theme_minimal() +
  labs(col = "",
       y = "",
       x = "Game clock") +
  theme(panel.grid.major = element_blank()) +
  scale_color_manual(values = plot_cols)
  
