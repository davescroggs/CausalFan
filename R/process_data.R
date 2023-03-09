# Create data

library(tidyverse)
library(magrittr)

# dat <- map(paste("data/",2017:2022,"_data.RDS",sep = ""),read_rds) %>% 
#   flatten() %>% 
#   discard(~.x[["matchInfo"]][["matchStatus"]] == "scheduled")

dat <- read_rds("data/all_seasons_raw.RDS") %>% 
  discard(~is.null(.x)) %>% 
  discard(~.x[["matchInfo"]][["matchStatus"]] == "scheduled")

safe_date <- possibly(.f = ~as.Date(.x) %>% lubridate::year(),otherwise = NA_real_)

SquadName_Colours <- c("#FDE725FF", "#73D055FF", "#27AD81FF", 
                       "#7E4E90FF", "#CC6A70FF", "#2D708EFF", "#C0C0C0", "#F68F46FF")
names(SquadName_Colours) <- c("Lightning", "Fever", "Vixens", 
                              "Firebirds", "Thunderbirds", "Swifts", "Magpies", "GIANTS")  

extracted_metrics <- 
  tibble(scores = dat %>%
           map(~pluck(.x,"scoreFlow",1)),
         team_stats = dat %>%
           map(~pluck(.x, "teamPeriodStats","team")),
         player_stats = dat %>%
           map(~pluck(.x, "playerPeriodStats","player")),
         player_match_stats = dat %>%
           map(~pluck(.x, "playerStats","player")),
         team_info = dat %>%
           map(~pluck(.x, "teamInfo","team")),
         player_info = dat %>%
           map(~pluck(.x, "playerInfo","player")),
         subs = dat %>%
           map(~pluck(.x,"playerSubs","player")),
         round = dat %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
         match = dat %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
         season = dat %>% map_chr(~pluck(.x,"matchInfo", "localStartTime",.default = NA_character_)) %>%
           safe_date(),
         home = map_int(dat,  ~ .x[["matchInfo"]][["homeSquadId"]]),
         away = map_int(dat,  ~ .x[["matchInfo"]][["awaySquadId"]])) %>% 
  filter(!is.na(season),!map_lgl(scores,is.null)) %>% 
  mutate(round = case_when(
    season == 2021 & round == 12L & match == 5L ~ 8L,
    TRUE ~ round),
    match = case_when(
      season == 2021 & round == 8L & match == 5L ~ 4L,
      TRUE ~ match))

unnest_specific <- function(df,tbl){
  df %>% 
    select(season, round, match, {{tbl}}) %>%
    unnest_longer({{tbl}}) %>%
    unnest_wider({{tbl}})
}

player_info <- extracted_metrics %>% 
  unnest_specific(player_info) %>% 
  distinct(season,playerId,.keep_all = T) %>% 
  select(season,playerId,displayName,surname,firstname)
  # group_by(playerId) %>% 
  # slice(1) %>% 
  # ungroup()

team_info <- extracted_metrics %>% 
  unnest_specific(team_info) %>% 
  select(squadId,squadNickname) %>% 
  distinct(squadId,.keep_all = T)

player_stats <- extracted_metrics %>% 
  unnest_specific(player_stats) %>% 
mutate(goals = if_else(!is.na(goal2),goal1 + goal2 * 2L,goals), 
       #generalPlayTurnovers = if_else(is.na(generalPlayTurnovers),turnovers,generalPlayTurnovers),
         squadId = if_else(squadId == 0,NA_integer_,squadId)) %>% 
  group_by(season,round,match) %>% 
  fill(squadId,.direction = "downup") %>% 
  ungroup() %>% 
  left_join(team_info) %>% 
  left_join(player_info)

updatePeriod <- function(x) {
  x$period = as.integer(x$period)
  return(x)
}

subs <- extracted_metrics %>%
  mutate(subs = map_if(subs,~pluck_depth(.x) == 2,list),
         subs = map(subs,function(y) map(y,~possibly(updatePeriod, otherwise = NULL)(.x)))) %>%  
  unnest_specific(subs) %>% 
  left_join(team_info,by = "squadId")

team_stats <- extracted_metrics %>% 
  unnest_specific(team_stats) %>% 
  mutate(goals = if_else(!is.na(goal2),goal1 + goal2 * 2L,goals))
         #generalPlayTurnovers = if_else(is.na(generalPlayTurnovers),turnovers,generalPlayTurnovers)

player_match_stats <- extracted_metrics %>% 
  unnest_specific(player_match_stats) %>% 
  mutate(goals = if_else(!is.na(goal2),goal1 + goal2 * 2L,goals))

starting_pos <- player_stats %>% 
  left_join(team_info,by = "squadId") %>% 
  select(round,match,period,squadId,playerId,startingPositionCode)

goals <- extracted_metrics %>% 
  unnest_specific(scores) %>% 
  left_join(team_info,by = "squadId") %>%
  left_join(player_info)

schedule <-
  extracted_metrics %>% 
  mutate(round = case_when(
    season == 2021 & round == 12L & match == 5L ~ 8L,
    TRUE ~ round),
    match = case_when(
      season == 2021 & round == 8L & match == 5L ~ 4L,
      TRUE ~ match)) %>% 
  select(season, round, match, home,away)

save(goals,player_info,player_stats,player_match_stats,schedule,subs,team_info,team_stats,SquadName_Colours,file = "data/all_seasons_data.RData")

