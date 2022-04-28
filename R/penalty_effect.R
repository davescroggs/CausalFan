library(tidyverse)
library(patchwork)

source("R/load_netball_data.R")
load_netball_data(2017:2022)

o_rtg_full <- o_rtg  %>% 
  transmute(round,match,opponent = squadNickname,dPoss = possessions,dGoals = goals,dFeeds = feeds) %>% 
  full_join(o_rtg) %>% 
  filter(opponent != squadNickname) %>% 
  mutate(oRtg = goals/possessions*100,
            dRtg = dGoals/dPoss*100)
  
{plot_dat %>% 
ggplot(aes(x = penalties,y = dRtg)) +
  geom_point(aes(col = squadNickname,group= paste(season,round))) +
    geom_smooth(method = "lm")} %>% 
  plotly::ggplotly()

plot_dat %>% 
  select(season,squadNickname,contactPenalties,obstructionPenalties,dRtg) %>% 
  pivot_longer(c(contactPenalties,obstructionPenalties)) %>% 
  ggplot(aes(x = value,y = dRtg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~name,scales = "free_x")


plot_dat %>% 
  group_by(season,squadNickname) %>% 
  summarise(across(c(goals,possessions,dGoals,dPoss,contactPenalties,obstructionPenalties),sum)) %>% 
  mutate(net = (goals/possessions - dGoals/dPoss)*100) %>% 
  pivot_longer(c(contactPenalties,obstructionPenalties)) %>% 
  ggplot(aes(x = value,y = net)) +
  geom_point() +
  facet_wrap(~name,scales = "free_x")


# Penalties by position ---------------------------------------------------

combos <- player_stats %>% 
  select(season,round,match,period,squadId,playerId,startingPositionCode) %>% 
  #filter(round == 1,match == 3,squadId == 807) %>% 
  mutate(time = 0L) %>% 
  group_by(season,round,match,squadId,period) %>% 
  group_split() %>% 
  map_dfr(function(init) {
    
    sea = unique(init$season)
    r = unique(init$round)
    m = unique(init$match)
    s = unique(init$squadId)
    p = unique(init$period)
    
    position_combos <- function(x, y) {
      x$startingPositionCode[match(y$playerId, x$playerId)] <- y$toPos
      x$time <- unique(y$periodSeconds)
      return(x)
    }
    
    subs %>%
      filter(season == sea,round == r, match == m, squadId == s, period == p) %>%
      mutate(toPos = if_else(toPos == "S", "I", toPos)) %>%
      group_by(periodSeconds) %>%
      group_split() %>%
      accumulate(position_combos, .init = init)
  }) %>% 
  group_by(season,round,match,squadId,period,playerId,time) %>% 
  slice_tail(n = 1)

# Goals Per Minute -------------------------------------------------------
position_playingtime <- 
  combos %>% 
  mutate(startingPositionCode = factor(startingPositionCode,c("I","GK","GD","WD","C","WA","GA","GS"),ordered = T)) %>% 
  left_join(player_info %>% select(season,playerId,surname)) %>% 
  group_by(season,round,match,squadId,period,playerId) %>% 
  arrange(season,round,match,squadId,playerId,period,time) %>% 
  mutate(end_time = lead(time),
         end_time = if_else(is.na(end_time),900L,end_time),
         time_tot = (end_time - time)) %>% 
  filter(startingPositionCode != "I") %>% 
  group_by(season,round,match,squadId,period,playerId,surname,startingPositionCode) %>% 
  arrange(season,round,match,squadId,playerId,period) %>% 
  summarise(time_tot = sum(time_tot)) %>% 
  mutate(pct = time_tot/sum(time_tot)) %>% 
  ungroup()


pp_data <- position_playingtime %>%
  left_join(player_stats %>% select(season,round,match,period,playerId,contactPenalties,obstructionPenalties)) %>% 
  mutate(across(c(contactPenalties,obstructionPenalties),~.x * pct)) %>% 
  group_by(season,round,match,squadId,startingPositionCode) %>% 
  summarise(across(c(contactPenalties,obstructionPenalties),sum)) %>% 
  left_join(o_rtg_full,by = c("season", "round", "match", "squadId")) %>% 
  ungroup() %>%
  left_join(o_rtg_full %>% 
              group_by(season,squadId) %>% 
              summarise(mdRtg = mean(dRtg))) %>%
  mutate(
    #across(c(contactPenalties, obstructionPenalties),~.x/dPoss),
    penalties = (contactPenalties + obstructionPenalties),
         oRtg = goals/possessions*100,
                dRtg = dGoals/dPoss*100,
    dRtg_diff = dRtg - mdRtg)



position_playingtime %>%
  left_join(player_stats %>% select(season,round,match,period,playerId,contactPenalties,obstructionPenalties)) %>% 
  mutate(across(c(contactPenalties,obstructionPenalties),~.x * pct)) %>% 
  group_by(season,round,match,squadId) %>% 
  summarise(across(c(contactPenalties,obstructionPenalties),sum)) %>% 
  left_join(o_rtg_full,by = c("season", "round", "match", "squadId")) %>% 
  ungroup() %>% 
  mutate(type = "A") %>% 
  bind_rows(player_stats %>% mutate(type = "B")) %>% 
  group_by(season,type) %>%
  summarise(across(c(contactPenalties,obstructionPenalties),sum)) %>% 
  pivot_wider(names_from = type,values_from = c(contactPenalties,obstructionPenalties))

pp_data %>% 
  arrange(-penalties) %>% 
  head(10) %>% 
  select(1:6,penalties,dRtg,squadNickname,opponent)

o_rtg_by_season %>% 
  distinct(season,moRtg,mdRtg)


# All positions -----------------------------------------------------------

p1 <- pp_data %>% 
  pivot_longer(cols = c(contactPenalties,obstructionPenalties)) %>% 
  ggplot(aes(value,dRtg)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~name)

p2 <- pp_data %>% 
  ggplot(aes(penalties,dRtg)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = F)

p1 / p2



# By position -------------------------------------------------------------

p3 <- pp_data %>% 
  ggplot(aes(contactPenalties,dRtg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~startingPositionCode)

p4 <- pp_data %>% 
  ggplot(aes(obstructionPenalties,dRtg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~startingPositionCode)

p5 <- pp_data %>% 
  ggplot(aes(penalties,dRtg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~startingPositionCode)

p3 + p4


# Deviation from mean -----------------------------------------------------


pp_data %>% 
  ggplot(aes(penalties,dRtg_diff)) +
  geom_point(alpha = 0.15) +
  geom_point(data = pp_data %>% 
               group_by(startingPositionCode) %>% 
               summarise(mPen = mean(penalties),
                         dRtg_diff = 0),
             aes(x = mPen),size = 3,col = "red") +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~startingPositionCode)


pp_data %>% 
  arrange(-dRtg_diff) %>% View()
