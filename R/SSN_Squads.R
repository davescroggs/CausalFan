library(tidyverse)
library(superNetballR)
library(magrittr)

source("R/load_netball_data.R")
load_netball_data(2022)


# Position plot -----------------------------------------------------------

positions_per_time <- bind_rows(
  # Substitutions
  subs %>%
    #mutate(periodSeconds = if_else(periodSeconds == 0, as.integer(1),periodSeconds)) %>% 
    mutate(type = "sub") %>% 
    select(playerId,round,match,squadNickname,position = toPos,period,periodSeconds,type),
  # starting positions
  player_stats %>% 
    select(round,match,period,squadNickname,playerId,startingPositionCode) %>% 
    mutate(periodSeconds = 0,type = "pos") %>% 
    rename(position = startingPositionCode)) %>% 
  arrange(round,match,squadNickname,playerId,period,periodSeconds,desc(type)) %>% 
  distinct(playerId,match,round,period,periodSeconds,.keep_all = T) %>% 
  group_by(squadNickname,playerId,match,round,period) %>% 
  mutate(position = if_else(position == "S","I",position),
         time_on = lead(periodSeconds),
         time_on = if_else(is.na(time_on),900,time_on)) %>%
  ungroup() %>% 
  left_join(player_info) %>% 
  filter(period < 5)

plot_cols <- tibble(position = c("I","GS","GA","WA","C","WD","GD","GK"),
                    col = c("#a1a1a1","#f94144","#f3722c","#f8961e","#f9c74f","#90be6d","#43aa8b","#577590")) %>% 
  deframe()

positions_per_time %>% 
  mutate(id = 1:n(),
         periodSeconds = periodSeconds/60,
         time_on = time_on/60,
         pos = factor(position,c("I","GK","GD","WD","C","WA","GA","GS"),ordered = T)) %>% 
  pivot_longer(cols = c(periodSeconds,time_on)) %>% 
  filter(round == 1,squadNickname == "Vixens") %>% 
ggplot(aes(y = fct_reorder(displayName,desc(pos)),x = value, col = position)) +
  geom_line(aes(group = id),size = 8,alpha = 1) +
  #geom_point(size = 5,shape = "|",col = "black") +
  facet_grid(.~paste("Qtr",period)) +
  theme_minimal() +
  labs(col = "",
       y = "",
       x = "Game clock") +
  theme(panel.grid.major = element_blank()) +
  scale_color_manual(values = plot_cols)


# Player combinations -----------------------------------------------------


combos <- player_stats %>% 
  select(round,match,period,squadId,playerId,startingPositionCode) %>% 
  filter(round == 1,match == 3,squadId == 807) %>% 
  mutate(time = 0L) %>% 
  group_by(round,match,squadId,period) %>% 
  group_split() %>% 
  map(function(init) {
    
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
      filter(round == r, match == m, squadId == s, period == p) %>%
      mutate(toPos = if_else(toPos == "S", "I", toPos)) %>%
      group_by(periodSeconds) %>%
      group_split() %>%
      accumulate(position_combos, .init = init)
  }) %>% 
  group_by(season,round,match,squadId,period,time) %>% 
  slice(-1)

# Goals Per Minute -------------------------------------------------------

squads <- tibble(df = flatten(combos)) %>% 
  mutate(id = 1:n()) %>% 
  unnest_wider(df) %>% 
  unnest(cols = -c(id)) %>% 
  mutate(startingPositionCode = factor(startingPositionCode,c("I","GK","GD","WD","C","WA","GA","GS"),ordered = T)) %>% 
  filter(startingPositionCode != "I") %>% 
  left_join(player_info %>% select(playerId,surname)) %>% 
  group_by(round,match,squadId,period,id) %>% 
  arrange(round,match,squadId,period,time,startingPositionCode,playerId) %>% 
  summarise(squad = paste(startingPositionCode, surname, sep = "-",collapse = "; "),
            start_time = first(time),
            period = first(period)) %>% 
  mutate(end_time = lead(start_time),
         end_time = if_else(is.na(end_time),900L,end_time)) %>% 
  filter(!(start_time == 0 & end_time == 0)) %>% 
  ungroup() %>% 
left_join(bind_rows(schedule,schedule %>% rename(home = away,away = home)) %>% 
  arrange(round,match) %>% 
  rename(squadId = home, opposition = away))

gpm <- squads %>% 
  mutate(time_on = (end_time - start_time)/60,
         goalsFor = pmap_dbl(list(round,match,squadId,period,start_time,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(scorepoints)}),
         goalsAgainst = pmap_dbl(list(round,match,opposition,period,start_time,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(scorepoints)}),
         plus_minus = goalsFor - goalsAgainst)

gpm %>% 
  inner_join(team_info) %>% 
  inner_join(team_info %>% rename(opposition = squadId,opponent = squadNickname)) %>% 
  arrange(opponent) %>% 
  pivot_longer(cols = c(goalsFor,goalsAgainst)) %>%
  filter(squadId == 804) %>% 
  ggplot(aes(x = value,y = fct_inorder(squad),size = time_on,col = name)) +
  geom_point() +
  geom_text(aes(label = period),col = "white",show.legend = F,family = "mono") +
  facet_grid(~opponent) +
  scale_colour_manual(values = c("goalsAgainst" = "red","goalsFor" = "black")) +
  scale_size(breaks = seq(3,15,3)) +
  labs(title = "Success of Australian playing combinations",
       subtitle = "Goals for and against a given line-up while playing together",
       caption = "Data: Champion Data",
       size = "Minutes together",
       colour = "For/Against",
       y = "Unique squad",
       x = "Goals per minute") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"))

# Squad combinations ------------------------------------------------------

gpm %>% 
  mutate(minutes_played = round((end_time - start_time)/60),
         p5_min = (if_else(end_time <= 900,end_time,900L) - if_else(start_time > 600,start_time,600L)),
         p5_min = round(if_else(p5_min < 0,0L,p5_min)/60)) %>% 
  group_by(squad) %>% 
  summarise(across(c(minutes_played,p5_min, plus_minus),sum)) %>% 
  arrange(-minutes_played) %>% 
  separate_rows(squad,sep = "; ") %>% 
  tidyr::extract(squad,c("pos","name"),regex = "(.*)-(.*)") %>% 
  pivot_wider(names_from = pos,values_from = name) %>% 
  select(GK:GS,`Minutes Played` = minutes_played,`Power 5 minutes` = p5_min, `Plus Minus` = plus_minus) %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic()

# Shooter Combos ----------------------------------------------------------

squads <- tibble(df = flatten(combos)) %>% 
  mutate(id = 1:n()) %>% 
  unnest_wider(df) %>% 
  unnest(cols = -c(id)) %>% 
  mutate(startingPositionCode = factor(startingPositionCode,c("I","GK","GD","WD","C","WA","GA","GS"),ordered = T)) %>% 
  filter(startingPositionCode %in% c("GS","GA")) %>% 
  left_join(player_info %>% select(playerId,surname)) %>% 
  group_by(round,match,squadId,period,id) %>% 
  arrange(round,match,squadId,period,time,startingPositionCode,playerId) %>% 
  summarise(squad = paste(startingPositionCode, surname, sep = "-",collapse = "; "),
            start_time = first(time),
            period = first(period)) %>% 
  mutate(end_time = lead(start_time),
         end_time = if_else(is.na(end_time),900L,end_time)) %>% 
  filter(!(start_time == 0 & end_time == 0)) %>% 
  ungroup() %>%
  left_join(bind_rows(schedule,schedule %>% rename(home = away,away = home)) %>% 
              arrange(round,match) %>% 
              rename(squadId = home, opposition = away))


gpm <- squads %>% 
  mutate(time_on = (end_time - start_time)/60,
         goalsFor = pmap_dbl(list(round,match,squadId,period,start_time,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(scorepoints)}),
         goalsAgainst = pmap_dbl(list(round,match,opposition,period,start_time,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(scorepoints)})) %>% 
  group_by(round,match,squadId,period,squad,opposition) %>% 
  summarise(goalsFor = sum(goalsFor),
            time_on = sum(time_on),
            goalsFor = goalsFor/time_on)

gpm %>% 
  inner_join(team_info) %>% 
  inner_join(team_info %>% rename(opposition = squadId,opponent = squadNickname)) %>% 
  arrange(opponent) %>% 
  filter(squadId == 804) %>%
  ggplot(aes(x = goalsFor,y = fct_inorder(squad),size = time_on,col = opponent)) +
  geom_point() +
  geom_text(aes(label = period),col = "white",show.legend = F,family = "mono") +
  scale_size(limits = c(3,15),breaks = seq(3,15,3),range = c(3,15)) +
  scale_colour_manual(values = c("Silver Ferns" = "#000000","SPAR Proteas" = "#FCD270","Vitality Roses" = "#E41D36")) +
  #facet_wrap(~opponent,ncol = 1) +
  theme_bw() +
  labs(size = "Minutes together",
       colour = "Opponent",
       title = "Goals per minute of Australian shooting combinations",
       subtitle = "Goals for a given shooting pair while playing\ntogether against a given opponent",
       caption = "Data: Champion Data",
       y = "Shooting combinations",
       x = "Goals per minute") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black")) +
  guides(colour = guide_legend(override.aes = list(size=4)))
