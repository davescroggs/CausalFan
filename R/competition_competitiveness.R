library(tidyverse)
library(superNetballR)

dat <- map(paste("data/",2009:2021,"_data.RDS",sep = ""),read_rds) %>% 
  flatten()

safe_date <- possibly(.f = ~as.Date(.x) %>% lubridate::year(),otherwise = NA_real_)

extracted_metrics <- 
  tibble(scores = dat %>%
           map(~pluck(.x,"scoreFlow",1)),
         team_stats = dat %>%
           map(~pluck(.x, "teamStats","team")),
         player_period_stats = dat %>%
           map(~pluck(.x, "playerPeriodStats","player")),
         player_stats = dat %>%
           map(~pluck(.x, "playerStats","player")),
         teamPeriodStats = dat %>%
           map(~pluck(.x, "teamPeriodStats","team")),
         team_info = dat %>%
           map(~pluck(.x, "teamInfo","team")),
         player_info = dat %>%
           map(~pluck(.x, "playerInfo","player")),
         round = dat %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
         match = dat %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
         season = dat %>% map_chr(~pluck(.x,"matchInfo", "localStartTime",.default = NA_character_)) %>%
           safe_date()) %>% 
  filter(!is.na(season),!map_lgl(scores,is.null))

unnest_specific <- function(df,tbl){
  df %>% 
    select(season, round, match, {{tbl}}) %>%
    unnest_longer({{tbl}}) %>%
    unnest_wider({{tbl}})
}

team_info <- extracted_metrics %>% 
  unnest_specific(team_info) %>% 
  group_by(squadId) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(squadId,squadNickname,squadCode) %>% 
  mutate(country = if_else(squadId %in% c(801L,804L,806L,807L,810L,8117L,8118L,8119L),"Australia","New Zealand"))

team_stats <- extracted_metrics %>% 
  unnest_specific(team_stats) %>% 
  mutate(goal1 = if_else(goal1 == 0 | is.na(goal1),goals,goal1),
         generalPlayTurnovers = if_else(generalPlayTurnovers == 0 | is.na(generalPlayTurnovers) ,turnovers,generalPlayTurnovers)) %>%
  replace_na(list(goal1 = 0,goal2 = 0)) %>% 
  mutate(goals = goal1 + goal2*2)

team_stats %>% 
  mutate(goal1 = if_else(goal1 == 0L | is.na(goal1),as.integer(goals),goal1)) %>% 
  replace_na(list(goal1 = 0L,goal2 = 0L)) %>% 
  mutate(goals = goal1 + goal2*2L) %>% 
  ungroup() %>% 
  select(season,round,match,squadId,goals) %>% 
  full_join(x = .,y = .,by = c("season","round","match")) %>%
  filter(squadId.x != squadId.y) %>%
  mutate(result = case_when(goals.x == goals.y ~ "Draw",
                            goals.x < goals.y ~ "Loss",
                            goals.x > goals.y ~ "Win"),
         result = factor(result,levels = c("Win","Loss","Draw"))) %>% 
  rename("squadId" = "squadId.x") %>% 
  group_by(season,squadId,result,.drop = FALSE) %>% 
  summarise(n = n()) %>% 
  mutate(win_pct = n/sum(n)) %>% 
  filter(result == "Win") %>%
  arrange(squadId,season) %>% 
  group_by(squadId) %>% 
  mutate(delta_wins = win_pct - lag(win_pct),
         expected = 0.5 - lag(win_pct)) 
  filter(season < 2017) %>% 
  ggplot(aes(season,win_pct,col = factor(squadId))) +
  geom_point() +
  geom_line()
  
