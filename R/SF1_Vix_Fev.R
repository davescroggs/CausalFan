library(tidyverse)
library(patchwork)
library(gt)
library(gtExtras)
library(magrittr)
library(ggnewscale)
library(glue)

load("data/2022_finals_data.RData")

rnd = 15
ssn = 2022
mtch = 1

teams <- schedule %>% 
  filter(round == rnd,season == ssn,match == mtch) %>% 
  select(home,away) %>%
  as_vector() 

team_list = team_info %>% select(squadId,squadNickname) %>% deframe()
team_names <- map_chr(teams,~team_list[as.character(.x)])

SquadName_Colours_teams <- SquadName_Colours[team_names]


o_rtg <- 
  player_stats %>% 
  group_by(season,round,match,squadNickname) %>% 
  summarise(across(c(goals,goal1,goal2, goalAttempts, goalMisses, generalPlayTurnovers,turnovers,feeds),~sum(.,na.rm = T))) %>%
  left_join(player_stats %>% 
              group_by(playerId) %>% 
              filter(any(goals > 0)) %>% 
              ungroup() %>% 
              count(season,round, match, squadNickname,wt = rebounds,name = "offensiveRebounds"),
            by = c("round", "match", "squadNickname","season")) %>% 
  mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers)


# Season O Rating ---------------------------------------------------------

o_rtg %>% 
  filter(squadNickname %in% team_names) %>% 
  mutate(oRtg = goals/possessions * 100,
         hth_matchup = if_else(round %in% c(4,10,15),"Yes","No")) %>% 
  ggplot(aes(round,oRtg,col = squadNickname,group = squadNickname)) +
  geomtextpath::geom_labelhline(data = ~group_by(.,squadNickname) %>% summarise(mean_rtg = mean(oRtg)),
                                aes(yintercept = mean_rtg, col = squadNickname, label = glue("{squadNickname} average offensive rating")),
                                linetype = "dashed",show.legend = F) +
  geom_point(aes(size = hth_matchup,shape = hth_matchup)) + 
  geom_path() +
  scale_color_manual(values = c("Fever" = "#73D055FF","Vixens" = "#DD0056")) +
  scale_x_continuous(breaks = 1:15) +
  theme_bw() +
  labs(title = "Season 2022 offensive rating - Fever & Vixens",
       subtitle = "Offensive rating in each round of the 2022 regular season",
       x = "Round",
       y = "Offensive rating (goals per 100 possessions)",
       shape = "Fev v Vix",
       size = "Fev v Vix",
       colour = "Team",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"))

# Possession outcome plots ------------------------------------------------

   
plts <- map(c(15,10,4),function(r){

  game_title <- if_else(r == 15,"Semi final",paste("Round",r))
  caption_text <- if_else(r == 4,"Data: Champion Data","")

 pd <-  o_rtg %>% 
  ungroup() %>% 
  filter(season == ssn,squadNickname %in% team_names,round == r) %>%
  mutate(effective_misses = goalMisses - offensiveRebounds) %>% 
  select(-c(season,goals,feeds,goalMisses,offensiveRebounds,goalAttempts)) %>% 
  pivot_longer(-c(round,match,squadNickname,possessions)) %>% 
  mutate(name = case_when(
    name == "effective_misses" ~ "Effective misses",
    name == "generalPlayTurnovers" ~ "Turnovers",
    name == "goal1" ~ "Normals",
    name == "goal2" ~ "Supers"),
    name = factor(name,levels = c("Turnovers", "Effective misses", "Supers", "Normals"),ordered = T))
  
  pd %>% 
  ggplot(aes(value,squadNickname)) +
  geom_col(aes(fill = name), col = "black") +
  geom_text(data = pd %>% count(squadNickname,wt = value),aes(x = n,label = n),hjust = 0,size = 6,nudge_x = 1) +
  geom_text(data = ~filter(.,value > 0),aes(label = value,group = name),position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(0,100,10)) +
  theme_bw() +
  labs(title = paste(game_title,"possession outcomes"),
       subtitle = "The outcome of each team's possessions",
       y = "",
       x = "Possession outcome count",
       fill = "Stat type",
       caption = caption_text) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        panel.grid.major.y = element_blank()) +
  expand_limits(x = 92)
})


plts %>% 
  reduce(`/`) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom",
        plot.background = element_rect(colour = "black"))


# Possession differential -------------------------------------------------

o_rtg %>% 
  ungroup() %>% 
  inner_join(schedule %>% 
               filter(home %in% c(804,810) & away %in% c(804,810)) %>% 
               select(round,match)) %>% 
  group_by(round) %>% 
  mutate(pct = scales::percent(possessions/sum(possessions)),
         round_label = if_else(round == 15,"Semi final",paste("Round",round)),
         round_label = fct_reorder(round_label,desc(round))) %>% 
  ggplot(aes(possessions,round_label)) +
  geom_col(aes(fill = squadNickname),col = "black") +
  geom_text(aes(label = possessions, group = squadNickname),position = position_stack(vjust = 0.5),col = "black") +
  geom_text(data = ~group_by(.,round_label) %>% summarise(possessions = sum(possessions)),
            aes(label = possessions),
            hjust = 0) +
  scale_fill_manual(values = SquadName_Colours_teams) +
  theme_bw() +
  labs(title = "Possession differential",
    subtitle = "Each teams contribution to total possessions in Fever/Vixens match-ups in 2022",
    y = "",
    x = "Possessions",
    fill = "Team",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black")) +
  xlim(0,180)


# Shot attemps by player --------------------------------------------------

map(team_names,function(plt_tm){
  
  team_pal <- if_else(plt_tm == "Fever","Greens","BuPu")
  
player_stats %>% 
  ungroup() %>% 
  inner_join(schedule %>% 
               filter(home %in% c(804,810) & away %in% c(804,810)) %>% 
               select(round,match)) %>% 
  group_by(playerId) %>% 
  filter(any(attempt_from_zone1 > 0)) %>% 
  ungroup() %>% 
  filter(squadNickname == plt_tm) %>% 
  group_by(squadNickname,round,surname) %>% 
  summarise(attempts = sum(attempt_from_zone1) + sum(attempt_from_zone2)) %>% 
  mutate(round = if_else(round == 15,"Semi",paste("Round",round)) %>% factor(levels = c("Round 4", "Round 10","Semi"),order = T),
         pct = attempts/sum(attempts)) %>% 
  ggplot(aes(attempts,factor(round),fill = surname)) +
  geom_col() +
  geom_text(data = ~filter(.,pct  > 0), aes(label = scales::percent(pct,accuracy = 1)),position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = team_pal) +
  theme_bw() + 
  labs(title = plt_tm,
       y = "",
       x = "Shot attempts by team",
       fill = "Player") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1)) +
  xlim(0,80)
}) %>% 
  reduce(`/`) &
  plot_annotation(title = "Team shot attempts by player",
                  subtitle = "Proportion of shot attempts by player in Fever/Vixens matches",
                  caption = "Data: Champion Data") &
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



# Shooting stats ----------------------------------------------------------------

player_stats %>%   
  filter(season == ssn,round == rnd,match == mtch,squadNickname == team_names[1]) %>%
  group_by(playerId) %>% 
  filter(any(attempt_from_zone1 > 0)) %>% 
  group_by(squadNickname,period) %>% 
  summarise(across(c(goals,goal1,goal2,attempt_from_zone1,attempt_from_zone2),sum)) %>% 
  ungroup() %>% 
  #inner_join(player_info %>% distinct(playerId,displayName),by = "playerId") %>% 
  mutate(period = as.character(period)) %>% 
  janitor::adorn_totals() %>% 
  mutate(normals = paste0(goal1,"/",attempt_from_zone1),
         normal_pct = scales::percent(goal1/attempt_from_zone1,1),
         supers = paste0(goal2,"/",attempt_from_zone2),
         super_pct = scales::percent(goal2/attempt_from_zone2,1),
         across(c(normal_pct,super_pct),replace_na,replace = "0%")) %>% 
  select(-c(goal1,goal2,attempt_from_zone1,attempt_from_zone2)) %>% 
  bind_rows(
    player_stats %>%   
      filter(season == ssn,round == rnd,match == mtch,squadNickname == team_names[2]) %>%
      group_by(playerId) %>% 
      filter(any(attempt_from_zone1 > 0)) %>% 
      group_by(squadNickname,period) %>% 
      summarise(across(c(goals,goal1,goal2,attempt_from_zone1,attempt_from_zone2),sum)) %>% 
      ungroup() %>% 
      #inner_join(player_info %>% distinct(playerId,displayName),by = "playerId") %>% 
      mutate(period = as.character(period)) %>% 
      janitor::adorn_totals() %>% 
      mutate(normals = paste0(goal1,"/",attempt_from_zone1),
             normal_pct = scales::percent(goal1/attempt_from_zone1,1),
             supers = paste0(goal2,"/",attempt_from_zone2),
             super_pct = scales::percent(goal2/attempt_from_zone2,1),
             across(c(normal_pct,super_pct),replace_na,replace = "0%")) %>% 
      select(-c(goal1,goal2,attempt_from_zone1,attempt_from_zone2))) %>% 
  gt() %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(2.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = squadNickname == "Total"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "black",
      weight = px(2.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = "squadNickname",
      rows = everything()
    )
  ) %>%
  cols_label(
    goals = "Goals",
    squadNickname = "Team",
    period = "Period",
    normals = "Normals",
    normal_pct = "%",
    supers = "Supers",
    super_pct = "%"
  )  %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(2.5),
      style = "solid"
    ),
    locations = list(
      cells_column_labels(gt::everything())
    )
  ) %>% 
  tab_source_note(
    source_note = "Data: Champion Data")



# Shot timing -------------------------------------------------------------

goals %>% 
  filter(season == ssn,squadNickname %in% team_names,round == rnd) %>%
  mutate(minute = floor(periodSeconds/60),
         shottype = case_when(
           scoreName == "2pt Goal" ~ "Super made",
           scoreName == "2pt Miss" ~ "Super missed",
           scoreName == "goal" ~ "Normal made",
           scoreName == "miss" ~ "Normal missed",
         ),
         shottype = factor(shottype,levels = c("Normal missed", "Super missed","Super made","Normal made"),ordered = T,
         )) %>% 
  count(squadNickname,round,period,minute,shottype) %>% 
  mutate(n = if_else(shottype %in% c("Super missed","Super made"),2L * n,n)) %>% 
  ggplot(aes(x = minute, y = n,fill = shottype)) +
  geom_col(aes(col = shottype)) +
  scale_fill_manual(values = c("Normal made" = "#1874CD","Super made" = "#FF3030", "Normal missed" = "white", "Super missed" = "white")) +
  scale_colour_manual(values = c("Normal made" = "#1874CD","Super made" = "#FF3030", "Normal missed" = "black", "Super missed" = "black"),guide = "none") +
  labs(title = "Scoring consistency",
       subtitle = "Goals scored in each minute of each quarter of the game",
       fill = "Score type",
       y = "Goals",
       x = "Minute",
       caption = "Data: Champion Data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"),
        strip.text = element_text(size = 10)) +
  guides(fill = guide_legend(override.aes = list(col = c("#1874CD", "#FF3030","black","black")))) +
  facet_grid(squadNickname ~ paste("Qtr -",period))


# Quarter O Rating --------------------------------------------------------

game_ortg <- player_stats %>%
  group_by(season,round,match,squadNickname,period) %>% 
  summarise(across(c(goals,goal1,goal2, goalAttempts, goalMisses, generalPlayTurnovers,feeds),~sum(.,na.rm = T))) %>%
  left_join(player_stats %>% 
              group_by(playerId) %>% 
              filter(any(goals > 0)) %>% 
              ungroup() %>% 
              count(season,round, match, squadNickname,period,wt = rebounds,name = "offensiveRebounds"),
            by = c("round", "match", "squadNickname","season","period")) %>% 
  mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers)  %>% 
  filter(round == rnd,season == ssn,match == mtch) %>% 
  ungroup()

game_ortg %>% 
  filter(squadNickname == team_names[1]) %>% 
  transmute(Team = squadNickname,
            Period = as.character(period),
            Possessions = possessions,
            Turnovers = generalPlayTurnovers,
            Misses = goalMisses,
            `Off Rebounds` = offensiveRebounds, 
            goalAttempts,
            goals) %>% 
  janitor::adorn_totals() %>% 
  mutate(`Shooting (eff)` = paste0(goals,"/",goalAttempts),
         `Off Rating` = round(goals/Possessions*100,1)) %>% 
  select(-goals,-goalAttempts) %>% 
  bind_rows(
    game_ortg %>% 
      filter(squadNickname == team_names[2]) %>% 
      transmute(Team = squadNickname,
                Period = as.character(period),
                Possessions = possessions,
                Turnovers = generalPlayTurnovers,
                Misses = goalMisses,
                `Off Rebounds` = offensiveRebounds, 
                goalAttempts,
                goals) %>% 
      janitor::adorn_totals() %>% 
      mutate(`Shooting (eff)` = paste0(goals,"/",goalAttempts),
             `Off Rating` = round(goals/Possessions*100,1)) %>% 
      select(-goals,-goalAttempts)) %>% 
  gt() %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(2.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = Team == "Total"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "black",
      weight = px(2.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = "Team",
      rows = everything()
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(2.5),
      style = "solid"
    ),
    locations = list(
      cells_column_labels(gt::everything())
    )
  ) %>% 
  gt::tab_footnote("Effective shooting % = (goals + supershot goals * 2)/attempts",cells_column_labels(
    columns = `Shooting (eff)`)) %>% 
  tab_source_note(
    source_note = "Data: Champion Data")


o_rtg %>% 
  ungroup() %>% 
  inner_join(schedule %>% 
               filter(home %in% c(804,8118) & away %in% c(804,8118)) %>% 
               select(round,match)) %>% View


player_stats %>% 
  filter(round == 10,squadNickname == "Vixens") %>% 
  distinct(surname)
