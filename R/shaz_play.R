library(tidyverse)
library(lubridate)
library(superNetballR)

`2017_data` <- readRDS("data/2017_data.RDS")
`2018_data` <- readRDS("data/2018_data.RDS")
`2019_data` <- readRDS("data/2019_data.RDS")
`2020_data` <- readRDS("data/2020_data.RDS")
`2021_data` <- readRDS("data/2021_data.RDS")

dat = c(`2021_data`,`2020_data`,`2019_data`,`2018_data`,`2017_data`)

team_round_data <- tibble(
  match = dat %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
  round = dat %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
  season = dat %>% map_chr(~.x[["matchInfo"]][["localStartTime"]]) %>% 
    as.Date() %>% year(),
  stats = dat %>% 
    map(~.x[["teamStats"]][["team"]])) %>% 
  unnest_longer("stats") %>% unnest_wider("stats") %>% 
  mutate(points = coalesce(goals,points))

team_info <- `2021_data` %>% 
  map_dfr(~.x[["teamInfo"]][["team"]]) %>% 
  distinct(squadNickname,squadId)

# Curate data -------------------------------------------------------------

game_results <- tibble(season = dat %>% map_chr(~.x[["matchInfo"]][["localStartTime"]]) %>% as.Date() %>% year(),
                       results = dat %>% map(tidyMatch) %>% map(matchResults)) %>% 
  unnest_auto("results") %>% unnest(-season) %>% 
  select(season,round,game,squadId) %>% 
  {full_join(.,.,by = c("season","round","game"))} %>% 
  filter(squadId.x != squadId.y) %>% 
  rename(squadId = squadId.x,opponent = squadId.y)


# Best Offensive Teams ----------------------------------------------------

# - Rate teams offensive performance and discover what makes it successful
# - Try and extract the goal circle from the actions by the goal circle from the mid-court

team_round_data %>%
  group_by(season,squadId) %>% 
  summarise() %>% 
  group_by(season) %>% 
  mutate(season_OP = mean(OP_team),
         season_TAP = mean(team_ave_points),
         OP = OP_team/season_OP * 100,
         TAP = team_ave_points/season_TAP)



# Feeders -----------------------------------------------------------------
# - Which position is feeding the most?


# Shooting Position -------------------------------------------------------

# - Distance code 
# 0 = Baseline 0 - 0.5 m
# 3 = Short 1- 3 m
# 1 = Long 3 - 4.9 m

team_scores_data <- tibble(
  match = dat %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
  round = dat %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
  season = dat %>% map_chr(~.x[["matchInfo"]][["localStartTime"]]) %>% 
    as.Date() %>% year(),
  home_squad = dat %>% map_chr(~.x[["matchInfo"]][["homeSquadId"]]),
  scores = dat %>% 
    map(~.x[["scoreFlow"]][["score"]])) %>% 
  unnest_longer("scores") %>% unnest_wider("scores")

team_scores_data %>% 
  filter(season != 2017) %>% 
  mutate(distanceCode = factor(distanceCode),
         P5 = periodSeconds >= 600) %>% 
  group_by(season,squadId,scoreName,distanceCode) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = scoreName,values_from = n,values_fill = list(n = 0)) %>% 
  mutate(distanceCode = case_when(distanceCode == "1" ~ "Long",
    distanceCode == "0" ~ "Baseline",
    distanceCode == "3" ~ "Short",
    TRUE ~ "ERROR"),
    shots = goal + miss + `2pt Goal` + `2pt Miss`) %>% inner_join(team_info) %>% View()
  group_by(season,squadId) %>% 
  mutate(shot_prop = shots/sum(shots),
         goals = sum(goal + 2 * `2pt Goal`)) %>%
  ungroup() %>% 
  inner_join(team_info) %>% 
  select(season,squadNickname,distanceCode,shot_prop,goals) %>%
  ggplot(aes(x = shot_prop,y = goals,col = squadNickname)) +
  geom_point() +
  facet_grid(season~distanceCode,scales = "free_x") 
  
  add_runs <- function(out, input){
    if (!last(input) | is.na(input)) {
      return(0)
    }
    return(input + out)
  }
  
  team_scores_data %>% 
    #filter(season == 2021,scorepoints > 0) %>% 
    filter(season == 2021, scorepoints > 0) %>%
    group_by(season,match,round,period) %>%
    mutate(cp1 = rep_len(unique(squadId),length.out = n()),
           cp2 = rep_len(rev(unique(squadId)),length.out = n()),
           is_same = (squadId != lead(squadId)),
           running_count = accumulate(is_same,add_runs),
           max_rc = max(running_count)) %>% 
  filter(running_count == max(running_count)) %>% 
    mutate(CP_order = if_else(squadId == cp1,"cp1","cp2")) %>% 
    ungroup() %>% 
    select(season,match,round,period,CP_order) %>% 
    right_join(team_scores_data %>% 
                 filter(season == 2021,scorepoints > 0)) %>% 
    group_by(season,match,round,period) %>%
    mutate(cp1 = rep_len(unique(squadId),length.out = n()),
           cp2 = rep_len(rev(unique(squadId)),length.out = n()),
           CP = if_else(CP_order == "cp1",cp1,cp2),
           on_cp = squadId == CP) %>%
    group_by(season,match,squadId,round) %>%
    summarise(on_cp = sum(on_cp),
    n = sum(n()),
    pct = on_cp/n) %>% 
    arrange(season,round,match) %>% 
    inner_join(team_round_data %>% 
    select(season,round,match,squadId,centrePassToGoalPerc,goalsFromCentrePass),
    by = c("season", "match", "squadId", "round")) %>% 
    mutate(diff = on_cp - goalsFromCentrePass) %>% View()
    ggplot(aes(diff)) + geom_histogram(binwidth = 1)
  
  
    