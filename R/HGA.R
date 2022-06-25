library(tidyverse)

source("R/load_netball_data.R")
load_netball_data(2022)


# Fever HGA ---------------------------------------------------------------

# Get venue name

dat <- read_rds(file = "data/2022_data.RDS")

schedule <- tibble(round = map_int(dat,~.x[["matchInfo"]][["roundNumber"]]),
         match = map_int(dat,~.x[["matchInfo"]][["matchNumber"]]),
         home = map_int(dat,  ~ .x[["matchInfo"]][["homeSquadId"]]),
         away = map_int(dat,  ~ .x[["matchInfo"]][["awaySquadId"]]),
         venue = map_chr(dat,  ~ .x[["matchInfo"]][["venueName"]]))
rm(dat)

# Ratings

o_rtg <- 
  team_stats %>% 
  left_join(team_info) %>% 
  group_by(season,round,match,squadId) %>% 
  summarise(across(c(goals,goal1,attempt_from_zone1,attempt_from_zone2,goal2, goalAttempts, goalMisses, goalsFromGain, generalPlayTurnovers,turnovers,feeds),~sum(.,na.rm = T))) %>%
  left_join(player_stats %>% 
              group_by(playerId) %>% 
              filter(any(goals > 0)) %>% 
              ungroup() %>% 
              count(season,round, match, squadId,wt = rebounds,name = "offensiveRebounds"),
            by = c("round", "match", "squadId","season")) %>% 
  mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers,
         off_rtg = (goals/possessions*100)) %>% 
  ungroup() %>% 
  left_join(schedule) %>% 
  left_join(team_info)


o_rtg %>% 
  filter(home == 810 | away == 810) %>% 
  mutate(isHome = squadId == home,
         fevHome = home == 810) %>% 
  ggplot(aes(y = goals, x = squadNickname,fill = isHome)) +
  geom_col() +
  facet_wrap(fevHome~round,scales = "free_x",nrow = 2)
  
o_rtg %>% 
  filter(home == 810 | away == 810) %>% 
  mutate(isHome = squadId == home,
         fevHome = home == 810,
         squadNickname = if_else(squadId == 810,"Fever","Other")) %>% 
  group_by(fevHome,squadNickname) %>% 
  mutate(mean_offRtg = mean(off_rtg)) %>% 
  ggplot(aes(x = off_rtg,y = round, col = squadNickname)) + 
  geom_point() +
  facet_wrap(~fevHome,ncol = 1)
  
o_rtg %>% 
  filter(home == 810 | away == 810) %>% 
  mutate(isHome = squadId == home,
         fevHome = home == 810,
         squadNickname = if_else(squadId == 810,"Fever","Other")) %>% 
  ggplot(aes(x = off_rtg,y = squadNickname, fill = squadNickname)) + 
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(~fevHome,ncol = 1)
