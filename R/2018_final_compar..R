library(tidyverse)

load("data/all_seasons_finals_data.RData")

player_stats %>% 
  count(season,round,displayName,wt = generalPlayTurnovers,sort = T) %>% 
  filter(str_detect(displayName,"Harten"),season ==2022) %>% 
  print(n = 50)


o_rtg %>% 
  filter(season == 2018) %>% 
  group_by(squadNickname) %>% 
  summarise(oRtg = sum(goals)/sum(possessions)*100)


o_rtg %>% 
  filter(season == 2018) %>% 
  group_by(squadNickname) %>% 
  summarise(across(c(goals:possessions,-turnovers),mean),.groups = "drop") %>% 
  mutate(goal1_acc = goal1/attempt_from_zone1,
         goal2_acc = goal2/attempt_from_zone2,
         eff_acc = goals/goalAttempts) %>% 
  select(squadNickname,goals,eff_acc,goal1,attempt_from_zone1,goal1_acc,goal2,attempt_from_zone2,goal2_acc,goalsFromGain,possessions,feeds,offensiveRebounds,generalPlayTurnovers) %>% 
  rename(stat_rename) %>% 
  mutate(across(-Team,list(ave_diff = ~(.x - mean(.x))/mean(.x)),.names = "{.col}.{.fn}"),
         #across(2:13,round,digits = 1),
         across(c(3,6,9),~scales::percent(.x,0.1)),
         across(c(2:14,-3,-6,-9),~round(.x,digits = 1) %>% as.character())) %>% 
  rename_at(2:14,~paste0(.x,".sqdmean")) %>% 
  pivot_longer(-Team,names_to = c("Stat",".value"),names_sep = "\\.") %>% 
  inner_join(win_loss %>% 
               rename(Team = squadNickname) %>% 
               group_by(Team) %>% 
               summarise(across(c(goals,goals.join,wl),sum)) %>% 
               mutate(pct = goals/goals.join) %>% 
               arrange(-wl,-pct) %>% 
               mutate(pos = 1:n())) %>% 
  ggplot(aes(x = fct_inorder(Stat),y = fct_reorder(Team,-pos))) +
  geom_tile(aes(fill = ave_diff)) +
  geom_text(aes(label = sqdmean)) +
  scale_fill_distiller(palette = "RdYlGn",labels = scales::percent,limit = c(-0.5,0.5),direction = 1) +
  theme_bw() +
  labs(title = "Mid-season stat review",
       subtitle = "Relevant team statistics from Rounds 1-7 season 2022",
       y = "",
       x = "Stat",
       fill = "% difference\nfrom mean",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"),
        axis.text.x = element_text(angle = 45,hjust = 1))


o_rtg <- 
  team_stats %>% 
  left_join(team_info) %>% 
  group_by(season,round,match,squadNickname) %>% 
  summarise(across(c(goals, goalAttempts, goalMisses, goalsFromGain, generalPlayTurnovers,turnovers,feeds),~sum(.,na.rm = T))) %>%
  left_join(player_stats %>% 
              group_by(playerId) %>% 
              filter(any(goals > 0)) %>% 
              ungroup() %>% 
              count(season,round, match, squadNickname,wt = rebounds,name = "offensiveRebounds"),
            by = c("round", "match", "squadNickname","season")) %>% 
  mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers,
         off_rtg = (goals/possessions*100)) %>% 
  ungroup()


o_rtg %>% 
  filter(season == 2018,round == 17) %>% 
  select(-c(goal1,goal2,attempt_from_zone1,attempt_from_zone2))

player_stats %>% 
  filter(season == 2018,round == 17) %>% 
  group_by(squadNickname,displayName) %>% 
  summarise(pos = first(startingPositionCode),
            across(c(goals,goalAttempts,rebounds,feeds,generalPlayTurnovers,gain),sum)) %>% 
  mutate(pos = factor(pos,levels = c("GS","GA","WA","C","WD","GD","GK"),ordered = T),
         goal_percentage = scales::percent(goals/sum(goals))) %>% 
  arrange(pos,squadNickname) %>% 
  filter(pos != "I")


plot_dat <- 
  o_rtg %>% 
  ungroup() %>% 
  filter(season == ssn,squadNickname %in% team_names,round == rnd) %>%
  mutate(effective_misses = goalMisses - offensiveRebounds) %>% 
  select(-c(season,goal1,goal2,attempt_from_zone1,attempt_from_zone2,goalsFromGain,turnovers,feeds,goalMisses,offensiveRebounds,goalAttempts,off_rtg)) %>% 
  pivot_longer(-c(round,match,squadNickname,possessions)) %>% 
  mutate(name = case_when(
    name == "effective_misses" ~ "Effective misses",
    name == "generalPlayTurnovers" ~ "Turnovers",
    name == "goals" ~ "Goals"),
    name = factor(name,levels = c("Turnovers", "Effective misses", "Goals"),ordered = T))



plot_dat %>% 
  ggplot(aes(value,squadNickname)) +
  geom_col(aes(fill = name), col = "black") +
  geom_text(data = plot_dat %>% count(squadNickname,wt = value),aes(x = n,label = n),hjust = 0,size = 6,nudge_x = 1) +
  geom_text(aes(label = value,group = name),position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(0,100,10)) +
  theme_bw() +
  labs(title = "2018 Grand Final - Fever v Lightning",
       subtitle = "The outcome of each team's possessions",
       y = "",
       x = "Possession outcome count",
       fill = "Stat type",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  expand_limits(x = plot_dat %>% count(squadNickname,wt = value) %$% (max(n) + 3))
