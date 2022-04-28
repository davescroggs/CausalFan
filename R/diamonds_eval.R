most_common_position <- 
 player_stats %>% 
   group_by(surname) %>% 
  filter(startingPositionCode != "I") %>% 
   count(surname,startingPositionCode,sort = T) %>% 
   slice_head() %>% 
   select(-n)
   


shooter_stats <- 
  player_stats %>% 
  group_by(surname) %>% 
  summarise(across(c(minutesPlayed,goals,goal1,goal2,feedWithAttempt,rebounds,generalPlayTurnovers,attempt_from_zone1,attempt_from_zone2,feeds,feedWithAttempt),sum)) %>% 
  filter(attempt_from_zone1 > 50 | surname == "Garbin")


shooter_stats %>% 
  ggplot(aes(goals,generalPlayTurnovers)) +
  geom_point() +
  ggrepel::geom_text_repel(data = ~filter(.,goals > 100 | generalPlayTurnovers > 10), aes(label = surname),min.segment.length = 0)


shooter_stats %>% 
  select(surname,goal1,goal2,attempt_from_zone1,attempt_from_zone2) %>% 
  transmute(surname,attempt_from_zone1,attempt_from_zone2,
            pct_from_zone1 = goal1/attempt_from_zone1,
            pct_from_zone2 = goal2/attempt_from_zone2) %>% 
  arrange(pct_from_zone1) %>% 
  pivot_longer(c(pct_from_zone1,pct_from_zone2,attempt_from_zone1, attempt_from_zone2),names_to = c(".value","Zone"),names_pattern = "(.*)_from_(.*)") %>% 
  mutate(Zone = if_else(Zone == "zone1","Normal","Super")) %>% 
  left_join(most_common_position) %>% 
  mutate(pct_label  = scales::percent(pct,accuracy = 1)) %>% 
  ggplot(aes(x = pct,y = fct_inorder(surname))) +
  geom_point(aes(size = attempt)) +
  geom_text(data = ~filter(.,Zone == "Normal"),aes(label = pct_label),col = "white") +
  scale_size_area(max_size = 20) +
  scale_x_continuous(labels = ~scales::percent(.x,accuracy = 1)) +
  facet_grid(fct_rev(startingPositionCode)~Zone,scales = "free") + 
  theme_bw() +
  labs(title = "SSN Shooting accuracy",
    subtitle = "Accuracy of shooters by position, in SSN season 2022",
    caption = "Size reflects total shot attempts\nData: Champion Data",
    y = "Shooter surname",
    x = "Percentage",
    size = "Attempts") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))

  

# Usage -------------------------------------------------------------------

# usage_data <-
  shooter_stats %>% 
    mutate(miss_zone1 = attempt_from_zone1 - goal1,
           miss_zone2 = attempt_from_zone2 - goal2) %>% 
    transmute(surname,goal1,goal2,miss_zone1,miss_zone2 = miss_zone2/2, feedWithAttempt,generalPlayTurnovers,rebounds,poor_usage = generalPlayTurnovers + miss_zone1 + miss_zone2) %>% 
    pivot_longer(-c(surname,poor_usage)) %>%
    left_join(most_common_position) %>% 
    group_by(surname) %>% 
    mutate(pct = value / sum(value),
           poor_usage_pct = poor_usage/sum(value),
           pct_label  = scales::percent(pct,accuracy = 1),
           name = case_when(name == "goal1" ~ "normal goal",
             name == "goal2" ~ "super goal",
             name == "feedWithAttempt" ~ "feed w attempt",
             name == "rebounds" ~ "rebound",
             name == "miss_zone1" ~ "normal miss",
             name == "miss_zone2" ~ "super miss",
             name == "generalPlayTurnovers" ~ "turnover",
             TRUE ~ name),
           name = factor(name,levels = c("normal goal", "super goal","feed w attempt", "rebound", "normal miss", "super miss",  "turnover"),ordered = T),
           outcome_type = if_else(name %in% c("goal1", "goal2", "goalAssists","rebounds"),"Good","Bad")) %>% 
    ggplot(aes(x = pct, y = tidytext::reorder_within(surname,-poor_usage_pct,startingPositionCode),fill = fct_rev(name))) +
  geom_col() +
  scale_fill_brewer(palette = "Spectral") +
  tidytext::scale_y_reordered() +
  scale_x_continuous(labels = ~scales::percent(.x,accuracy = 1)) +
  geom_text(data = ~filter(.,pct > 0.01),aes(label = pct_label),position = position_stack(vjust = 0.5),check_overlap = T) +
    facet_wrap(~fct_rev(startingPositionCode),scales = "free_y", ncol = 1) +
  theme_bw() +
  labs(title = "Goaler Usage",
    subtitle = "Proportions of SSN shooters terminating actions",
    x = "Percentage",
    y = "Player surname",
    fill = "Action",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))
  
    

# Mids --------------------------------------------------------------------

  o_rtg <- 
    player_stats %>% 
    group_by(squadNickname) %>% 
    summarise(across(c(goals,goal1,goal2, goalAttempts, goalMisses, generalPlayTurnovers,turnovers,feeds),~sum(.,na.rm = T))) %>%
    left_join(player_stats %>% 
                group_by(playerId) %>% 
                filter(any(goals > 0)) %>% 
                ungroup() %>% 
                count(squadNickname,wt = rebounds,name = "offensiveRebounds"),
              by = c("squadNickname")) %>% 
    mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers,
           poss_wt = possessions/mean(possessions),
           offRating = goals/possessions*100)

plot_data_pre <- player_stats %>%
  select(-startingPositionCode) %>% 
  left_join(most_common_position) %>% 
  group_by(surname,startingPositionCode,squadNickname) %>%
  filter(startingPositionCode %in% c("WA","C","WD"),sum(minutesPlayed) > 150) %>% 
  summarise(across(c(feeds,feedWithAttempt,goalAssists,pickups,gain,generalPlayTurnovers,minutesPlayed),sum)) %>% 
  mutate(feed_per_gpt = feeds/generalPlayTurnovers) %>% 
  arrange(startingPositionCode,-gain) %>% 
  select(-minutesPlayed) %>% 
  rename("Feeds" = "feeds",
        "Feeds w attempt" = "feedWithAttempt",
          "Goal assists" = "goalAssists",
         "Pickups" = "pickups",
         "Gain" = "gain",
          "Turnovers" = "generalPlayTurnovers",
          "Feed per turnover" = "feed_per_gpt")

plot_data_post <- player_stats %>%
    select(-startingPositionCode) %>% 
    left_join(most_common_position) %>% 
    group_by(surname,startingPositionCode,squadNickname) %>%
    filter(startingPositionCode %in% c("WA","C","WD"),sum(minutesPlayed) > 150) %>% 
    summarise(across(c(feeds,feedWithAttempt,goalAssists,pickups,gain,generalPlayTurnovers),~sum(.x)/sum(minutesPlayed)*60)) %>% 
  left_join(o_rtg %>%
              select(squadNickname,poss_wt)) %>%
  group_by(surname,startingPositionCode,squadNickname) %>%
  mutate(across(c(feeds,feedWithAttempt,goalAssists,pickups,gain,generalPlayTurnovers),~.x/poss_wt)) %>%
  mutate(feed_per_gpt = feeds/generalPlayTurnovers) %>% 
  arrange(startingPositionCode,-gain) %>% 
  select(-poss_wt) %>% 
  rename("Feeds" = "feeds",
         "Feeds w attempt" = "feedWithAttempt",
         "Goal assists" = "goalAssists",
         "Pickups" = "pickups",
         "Gain" = "gain",
         "Turnovers" = "generalPlayTurnovers",
         "Feed per turnover" = "feed_per_gpt")

plot_data %>% 
  pivot_longer(cols = -3:-1) %>% ungroup() %>% distinct(name) %>% pull(name) %>% clipr::write_clip()
  ggplot(aes(x = value,y = tidytext::reorder_within(surname,value,list(name,startingPositionCode)))) +
  geom_point() +
  tidytext::scale_y_reordered() +
  facet_grid(startingPositionCode~name,scales = "free")


plot_data_post %>% 
  pivot_longer(cols = -3:-1) %>% 
  ggplot(aes(x = value,y = tidytext::reorder_within(surname,value,list(name,startingPositionCode)))) +
  geom_col(aes(fill = startingPositionCode)) +
  tidytext::scale_y_reordered() +
  facet_wrap(~name,scales = "free",nrow = 2)

plot_data_pre %>% 
  pivot_longer(cols = -3:-1) %>% 
  ggplot(aes(x = value,y = tidytext::reorder_within(surname,value,list(name,startingPositionCode)))) +
  geom_col(aes(fill = squadNickname)) +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = SquadName_Colours) +
  facet_wrap(~name,scales = "free",nrow = 2) +
  labs(title = "Total statistcis",
    subtitle = "Accumulated stats from season 2022",
    y = "",
    x = "Stat toal count",
    fill = "Team",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))

plot_data_post %>% 
  pivot_longer(cols = -3:-1) %>% 
  ggplot(aes(x = value,y = tidytext::reorder_within(surname,value,list(name,startingPositionCode)))) +
  geom_col(aes(fill = squadNickname)) +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = SquadName_Colours) +
  facet_wrap(~name,scales = "free",nrow = 2) +
  labs(title = "Adjusted statistcis",
       subtitle = "Totals adjusted for playing time and team pace\ncounts reflect stat per 60 minutes at 2022 league average pace",
       y = "",
       x = "Adjusted count",
       fill = "Team",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"))


# Squads ------------------------------------------------------------------

combos <- player_stats %>% 
  select(season,round,match,period,squadId,playerId,startingPositionCode) %>% 
  filter(season == 2022) %>% 
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
  slice_tail(n = 1) %>% 
  ungroup()

# Goals Per Minute -------------------------------------------------------

squads <- 
  combos %>% 
  #filter(match == mtch,squadId == team_info$squadId[team_info$squadNickname == team_names[1]]) %>%
  left_join(player_info) %>% 
  arrange(season,round,match,squadId,startingPositionCode,period,time) %>%
  group_by(season,round,match,squadId,period,startingPositionCode) %>% 
  mutate(end_time = lead(time),
         end_time = if_else(is.na(end_time),900L,end_time),
         time_tot = (end_time - time)) %>% 
  filter(startingPositionCode != "I") %>% 
  arrange(season,round,match,squadId,period,time) %>% 
  ungroup() %>% 
  left_join(bind_rows(schedule,schedule %>% rename(home = away,away = home)) %>% 
              arrange(round,match) %>% 
              rename(squadId = home, opposition = away))

plot_cols <- tibble(position = c("I","GS","GA","WA","C","WD","GD","GK"),
                    col = c("#a1a1a1","#f94144","#f3722c","#f8961e","#f9c74f","#90be6d","#43aa8b","#577590")) %>% 
  deframe()

aus_players <- tibble(surname = c("Hadley","Brazill","Moloney","Price","Watson","Parmenter"),
                      pct = 1)

squads %>% 
  filter(startingPositionCode != "-") %>% 
  group_by(playerId,surname,squadId,startingPositionCode) %>% 
  summarise(minutes = sum(time_tot)/60) %>% 
  left_join(most_common_position %>% 
              rename("mcp" = "startingPositionCode")) %>% 
    mutate(pct = minutes/sum(minutes),
           mcp = factor(mcp,levels = c("WA","C","WD"),ordered = T),
           aus_player = if_else(surname %in% aus_players,"Diamond","SSN")) %>% 
  filter(surname %in% unique(plot_data$surname)) %>% 
  arrange(mcp) %>% 
  ggplot(aes(x = pct, y = fct_inorder(surname))) +
  geom_col(aes(fill = fct_relevel(startingPositionCode,c("GD","WD","C","WA","GA","GS"))),
           position = position_stack()) +
  geom_col(data = aus_players,col = "black",fill = NA,size = 1.2) +
  scale_fill_manual(values = plot_cols, limits = force)



player_stats %>%
  select(-startingPositionCode) %>% 
  left_join(most_common_position) %>% 
  filter(startingPositionCode %in% c("WA","C","WD"),surname %in% unique(plot_data$surname)) %>% 
  group_by(surname,round,.drop = F) %>%
  summarise(across(c(minutesPlayed),~sum(.x))) %>% 
  mutate(totalMins = sum(minutesPlayed)) %>% 
  ungroup() %>% 
  complete(round,surname) %>% 
  replace_na(list(totalMins = 0)) %>% 
  ggplot(aes(round,y = fct_reorder(surname,totalMins), fill = minutesPlayed)) +
  geom_tile() +
  geom_text(aes(label = minutesPlayed), col = "white") +
  scale_fill_distiller(palette = "RdYlGn") +
  theme_bw() +
  labs(title = "Mid-court playing minutes",
    subtitle = "Minutes on court for players who have played\nmore than 50% of total minutes in 2022",
    caption = "Data: Champion Data",
    y = "Player surname",
    x = "Round",
    fill = "Minutes played") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))


