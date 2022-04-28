library(tidyverse)
library(magrittr)

source("R/load_netball_data.R")
load_netball_data(2022)


margins <- 
  goals %>% 
  filter(scorepoints > 0) %>% 
 select(round,match,period,periodSeconds,squadId,scorepoints) %>% 
  left_join(schedule) %>% 
  group_by(round,match,period) %>% 
  # Convert to a single time measure
  mutate(
    periodSeconds = if_else(periodSeconds > 900L,900L,periodSeconds) + 900*(period - 1),
         home_score = if_else(squadId == home, scorepoints, 0L) %>% cumsum(),
         away_score = if_else(squadId == away, scorepoints, 0L) %>% cumsum(),
         margin = home_score - away_score,
         prev_margin = lag(margin) %>% replace_na(0L),
         game_minute = ceiling(periodSeconds / 60),
         min_diff = (periodSeconds - (game_minute - 1) * 60)) %>% 
  group_by(round,match,period,game_minute) %>%
  # Calculate the amount of time in each minute the margin occurs for
  mutate(time_diff = min_diff - lag(min_diff),
         time_diff = if_else(is.na(time_diff),min_diff, time_diff)/60,
         # Weight the margin by the time at that margin
         weighted_goals = time_diff*margin)

squads = unique(goals$squadId)

average_game_margins <- map_dfr(squads,function(x){
margins %>% 
  filter(home == x | away == x) %>%
    # Flip the sign if the focus squad is the away team
  mutate(weighted_goals = if_else(away == x,-weighted_goals,weighted_goals),
         margin = if_else(away == x,-margin,margin)) %>% 
  summarise(
    # Caculate the remaining time between the last goal in the minute and the end of the minute
    missing_secs = 1 - sum(time_diff),
            last_margin = last(margin),
            w_ave = sum(weighted_goals) + missing_secs*last_margin) %>%
  ungroup() %>% 
    # Fill in missing minutes where no goals are scored
    right_join(list(game_minute = 1:15,period = 1:4, round = 1:5) %>%
                 cross_df() %>% 
                 mutate(game_minute = game_minute + 15*(period - 1)),
               by = c("round","period", "game_minute")) %>%
    bind_rows(list(game_minute = 0,w_ave = 0,period = 1:4, round = 1:5) %>%
                cross_df() %>% 
                mutate(game_minute = game_minute + 15*(period - 1))) %>% 
  group_by(round) %>% 
  fill(match) %>% 
  arrange(round,match, game_minute) %>% 
  fill(w_ave,period) %>% 
  group_by(game_minute,period) %>%
    # Calculate average
  summarise(squadId = x,
    mean_goals = mean(w_ave)) %>%
    ungroup()
  })

game_margins <- map_dfr(squads,function(x){
  margins %>% 
    filter(home == x | away == x) %>%
    mutate(weighted_goals = if_else(away == x,-weighted_goals,weighted_goals),
           margin = if_else(away == x,-margin,margin)) %>% 
    summarise(squadId = x,
              missing_secs = 1 - sum(time_diff),
              last_margin = last(margin),
              a = sum(weighted_goals),
              b = missing_secs*last_margin,
              w_ave = sum(weighted_goals) + missing_secs*last_margin) %>%
    ungroup() %>% 
    right_join(list(game_minute = 1:15,period = 1:4, round = 1:5) %>%
                 cross_df() %>% 
                 mutate(game_minute = game_minute + 15*(period - 1)),
               by = c("round","period", "game_minute")) %>%
    bind_rows(list(game_minute = 0,w_ave = 0,period = 1:4, round = 1:5) %>%
    cross_df() %>% 
      mutate(game_minute = game_minute + 15*(period - 1))) %>% 
    group_by(round) %>% 
    fill(match,squadId) %>% 
    arrange(round,match, game_minute) %>% 
    fill(w_ave,period) %>% 
    left_join(team_info)})

average_game_margins %>% 
  left_join(team_info) %>% 
  ggplot(aes(x = game_minute,y = mean_goals,col = squadNickname)) +
  geomtextpath::geom_textvline(xintercept = 15, label = "Qtr 1",linetype = "dashed",hjust = 0.8,col = "grey10") +
  geomtextpath::geom_textvline(xintercept = 30, label = "Qtr 2",linetype = "dashed",hjust = 0.8,col = "grey10") +
  geomtextpath::geom_textvline(xintercept = 45, label = "Qtr 3",linetype = "dashed",hjust = 0.8,col = "grey10") +
  geomtextpath::geom_textvline(xintercept = 60, label = "Qtr 4",linetype = "dashed",hjust = 0.8,col = "grey10") +
  geom_hline(yintercept = 0) +
  geom_line(data = game_margins,aes(y = w_ave,group = paste(round,period)),
            col = "grey60") +
  geom_line(aes(group = paste(squadNickname,period)),size = 2) +
  scale_colour_manual(values = SquadName_Colours) +
  facet_wrap(~squadNickname,nrow = 2) +
  theme_bw() +
  labs(title = "SSN Season 2022 margins",
    subtitle = "Average margin at each minute of the 2022 season",
    colour = "Team",
    x = "Game minute",
    y = "Average margin",
    caption = "Coloured line is average, grey line is each round in 2022\nConcept by @insightlane, data via Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))


margins %>% 
  filter(home == 806 | away == 806) %>%
  # Flip the sign if the focus squad is the away team
  mutate(weighted_goals = if_else(away == 806,-weighted_goals,weighted_goals),
         margin = if_else(away == 806,-margin,margin)) %>% 
  summarise(
    # Caculate the remaining time between the last goal in the minute and the end of the minute
    missing_secs = 1 - sum(time_diff),
    last_margin = last(margin),
    w_ave = sum(weighted_goals) + missing_secs*last_margin) %>%
  ungroup() %>% 
  # Fill in missing minutes where no goals are scored
  right_join(list(game_minute = 1:15,period = 1:4, round = 1:5) %>%
               cross_df() %>% 
               mutate(game_minute = game_minute + 15*(period - 1)),
             by = c("round","period", "game_minute")) %>%
  group_by(round) %>% 
  fill(match) %>% 
  arrange(round,match, game_minute) %>% 
  fill(w_ave,period) %>% 
  group_by(game_minute,period) %>%
  # Calculate average
  summarise(squadId = 806,
            mean_goals = mean(w_ave)) %>%
  ungroup()
