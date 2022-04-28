library(tidyverse)
library(magrittr)

source("R/load_netball_data.R")
load_netball_data(2020:2021)
  # -------------------------------------------------------------------------


p2 <- goals %>% 
  filter(periodSeconds > 10*60) %>% 
  mutate(SS_YN = str_detect(scoreName,"2")) %>%
  group_by(season,squadId) %>% 
  summarise(ss_pct = sum(SS_YN)/n(),
            y_order = sum(SS_YN)) %>% 
  mutate(season = factor(season)) %>% 
  left_join(team_info) %>% 
  ggplot(aes(ss_pct,fct_reorder(squadNickname,y_order,mean),fill = season)) +
  geom_point(shape = 21,col = 'black', size = 3) +
  scale_fill_manual(values = c("2021" = "black","2020" = "white")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1.0), limits = c(0,0.7)) +
  labs(y = "",
       x = "% of super shots in power 5",
       fill = "Season",
    subtitle = "Super shot usage - Proportion of shots in Power 5 that are super shots in 20/21 seasons",
    caption = "Data: Champion Data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1))



#p3 <- 
  goals %>%
  filter(season == 2020) %>% 
  left_join(schedule, by = c("season", "round", "match")) %>% 
  group_by(season, round,match) %>% 
  mutate(score_difference = if_else(squadId == home, scorepoints,-scorepoints) %>%
           cumsum() %>% 
           if_else(squadId == away, -.,.),
         #score_difference = if_else(squadId != lag(squadId),-lag(score_difference),lag(score_difference)) %>% replace_na(0),
         score_margin = case_when(
           between(score_difference,6,Inf) ~ "6+ up", 
           between(score_difference,1,5) ~ "1-5 up", 
           score_difference == 0 ~ "Even",
           between(score_difference,-5,-1) ~ "1-5 down", 
           between(score_difference,-Inf,-6) ~ "6+ down"),
         score_margin = factor(score_margin,
                               levels = c("6+ up","1-5 up","Even","1-5 down","6+ down"),
                               ordered = T)) %>%
  filter(periodSeconds > 600) %>% 
  mutate(SS_YN = if_else(str_detect(scoreName,"2"),"Super","Normal")) %>% 
  ungroup() %>% 
  count(season,squadNickname,score_margin,SS_YN) %>% 
  ggplot(aes(y = fct_rev(score_margin),x = n,fill = SS_YN)) +
  geom_col() +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 1),
        #axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Normal" = "#1874CD","Super" = "#FF3030")) +
  labs(x = "Total attempts",
       y = "Score difference",
       fill = "Shot type",
       title = "2020 season",
       caption = "Data: Champion Data") +
    expand_limits(x = c(0,190)) +
  facet_wrap(~squadNickname, ncol = 2)

p3 + p4 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Super shot league-wide trends dependant on margin",
                  theme = theme(plot.background = element_rect(colour = "black"),
                                plot.title = element_text(hjust = 0.5)))

player_stats %>% 
  group_by(season,squadId) %>% 
  summarise(across(c(goal1,goal2,attempt_from_zone1,attempt_from_zone2),sum)) %>% 
  mutate(g1_pct = goal1/attempt_from_zone1,
         g2_pct = goal2/attempt_from_zone2) %>% 
  left_join(team_info) %>% 
  ggplot(aes(g2_pct,fct_reorder(squadNickname,g2_pct,mean))) +
  geomtextpath::geom_textvline(xintercept = 0.5,label = "50% accuracy",hjust = 0.14, linetype = "dashed") +
  geom_point(shape = 21,col = 'black', aes(size = attempt_from_zone2,fill = factor(season))) +
  scale_fill_manual(values = c("2021" = "black","2020" = "white")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1.0), limits = c(0,0.7)) +
  scale_size(range = c(7,12)) +
  labs(y = "",
       x = "Supershot accuracy",
       fill = "Season",
       title = "Team supershot shooting accuracy",
       subtitle = "Team-wide shooting accuracy for supershots only in 20/21 seasons",
       caption = "Data: Champion Data",
       size = "Total attempts") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black")) +
  guides(fill = guide_legend(override.aes = list(size = 8)))

player_stats %>% 
  group_by(season,squadId) %>% 
  summarise(across(c(goal1,goal2,attempt_from_zone1,attempt_from_zone2),sum)) %>% 
  mutate(`1 goal` = goal1/attempt_from_zone1,
         `2 goals` = goal2/attempt_from_zone2 *2,
         season = factor(season)) %>% 
  pivot_longer(cols = c(`1 goal`,`2 goals`)) %>% 
  left_join(team_info) %>% 
  ggplot(aes(value,fct_reorder(squadNickname,value,mean),fill = season,
             shape = name)) +
  geom_point(aes(col = season),size = 4) +
  scale_x_continuous(breaks = seq(0.8,1.4,0.05)) +
  scale_shape_manual(values = c(`1 goal` = 21,`2 goals` = 24)) +
  scale_fill_manual(values = c("2021" = "black","2020" = "white")) +
  scale_colour_manual(values = c("2021" = "white","2020" = "black"),guide = "none") +
  labs(title = "Effective points per shot",
       subtitle = "Accounting for additional points per shot when shooting super shots",
       y = "",
       x = "Effective points",
       fill = "Season",
       caption = "Data: Champion Data",
       shape = "Shot type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black")) +
  guides(fill = guide_legend(override.aes = list(size = 4,shape = 21)),
         shape = guide_legend(override.aes = list(size = 4)))


player_stats %>% 
  group_by(season,round,squadId) %>% 
  summarise(across(c(goal1,goal2,attempt_from_zone1,attempt_from_zone2),sum)) %>% 
  mutate(g1_pct = goal1/attempt_from_zone1,
         g2_pct = goal2/attempt_from_zone2) %>%
  replace_na(list(g2_pct = 0)) %>% 
  ungroup() %>% 
  summarise(across(c(g1_pct,g2_pct),list(mean = mean,var = var)))

n = 80
map_dfr(1:10000,~tibble(sim = .x,g1 = rbinom(n,1,0.886) %>% sum(),
       g2 = rbinom(n,1,0.503) %>% sum()*2)) %>% 
  mutate(diff = g1 - g2) %>% 
  ggplot(aes(diff)) + 
  geom_histogram()


player_stats %>% 
  group_by(playerId) %>% 
  filter(any(goal_from_zone1 > 0)) %>% 
  ungroup() %>% 
  group_by(season,round,playerId,period) %>% 
  summarise(across(c(goal1,goal2,attempt_from_zone1,attempt_from_zone2),sum)) %>% 
  # mutate(g1_pct = goal1/attempt_from_zone1,
  #        g2_pct = goal2/attempt_from_zone2) %>%
  replace_na(list(g2_pct = 0)) %>% 
  pivot_longer(cols = goal1:attempt_from_zone2,names_to = c("goals","type"),names_pattern = "(.*)(\\d)") %>% 
  pivot_wider(names_from = goals,values_from = value) %>% 
  count(goal,attempt_from_zone,playerId,season,type,round,sort = T) %>% 
  ggplot(aes(goal,attempt_from_zone)) +
  geom_point(aes(size = n)) +
  geom_abline(slope = 1) +
  facet_wrap(~type)

tibble(shotType = c("normal","super"),slope = c(1,2))

  goals %>%
    count(season,round,playerId,period,scoreName) %>% 
    mutate(attemptType = if_else(str_detect(scoreName,"(M|m)iss"),"miss","goal"),
           shotType = if_else(str_detect(scoreName,"2pt"),"super","normal")) %>% 
    select(-scoreName) %>% 
    pivot_wider(names_from = attemptType,values_from = n) %>% 
    mutate(miss = if_else(is.na(miss),0L,miss),
           goal = replace_na(goal,0),
           attempt = miss + goal,
           goal = if_else(shotType == "super",goal*2L,goal)) %>%
    arrange(shotType) %>% 
    ggplot(aes(attempt,goal,col = shotType)) +
    geom_point() +
    geom_abline(data = tibble(shotType = c("normal","super"),slope = c(1,0.5)),
                aes(slope = slope,intercept = 0))
  
    
  
    goals %>%
      filter(periodSeconds >= 10*60) %>% 
      count(season,round,squadId,period,scoreName) %>% 
      mutate(attemptType = if_else(str_detect(scoreName,"(M|m)iss"),"miss","goal"),
             shotType = if_else(str_detect(scoreName,"2pt"),"super","normal")) %>% 
      select(-scoreName) %>% 
      pivot_wider(names_from = attemptType,values_from = n) %>% 
      mutate(miss = if_else(is.na(miss),0L,miss),
             goal = replace_na(goal,0),
             attempt = miss + goal,
             goal = if_else(shotType == "super",goal*2L,goal)) %>% 
      group_by(season,round,squadId,period) %>% 
      summarise(pct = sum(shotType == "super")/sum(attempt),
             efg = sum(goal)/sum(attempt)) %>% 
      ungroup() %>% 
      count(pct,efg) %>% 
      ggplot(aes(x = pct,y = efg)) +
      geom_point(aes(size = n)) +
      geom_density2d() +
      scale_y_continuous(breaks = 0:14)
  
    
    
   p1 <-  
     goals %>% 
     filter(periodSeconds >= 10*60) %>% 
     mutate(season = factor(season),
            SS = str_detect(scoreName,"2")) %>% 
     group_by(season,round,SS) %>% 
     summarise(n = n()) %>% 
     mutate(n = n/sum(n)) %>% 
     filter(SS) %>%
      ggplot(aes(round,n,fill = season,group = season)) +
      geom_point(shape = 21,col = 'black', size = 3) +
      geom_line(aes(linetype = season)) +
      scale_fill_manual(values = c("2021" = "black","2020" = "white")) +
      scale_linetype_manual(values = c("2020" = "solid","2021" = "dashed"),
                            guide = "none") +
      scale_x_continuous(breaks = 1:14) +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      expand_limits(y = 0) +
      labs(y = "Super shot proportion",
           x = "Round",
           fill = "Season",
           subtitle = "Super shot proportion - League % of shots that were supers in Power 5") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            axis.title.y = element_text(vjust = 0))

   p1 / p2 + plot_layout(guides = 'collect') +
     plot_annotation(title = "Super shot league-wide trends",
                     theme = theme(plot.background = element_rect(colour = "black"),
                                   plot.title = element_text(hjust = 0.5)))
   
   
   
     ggplot(aes(round,n,fill = season,group = season)) +
     geom_point(shape = 21,col = 'black', size = 3) +
     geom_line(aes(linetype = season)) +
     scale_fill_manual(values = c("2021" = "black","2020" = "white")) +
     scale_linetype_manual(values = c("2020" = "solid","2021" = "dashed"),
                           guide = "none") +
     scale_x_continuous(breaks = 1:14) +
     expand_limits(y = 0) +
     labs(y = "Super shot attempts",
          x = "Round",
          fill = "Season",
          subtitle = "Super shot attempts - League wide super shot attempts by round") +
     theme_bw() +
     theme(plot.title = element_text(hjust = 0.5),
           plot.subtitle = element_text(hjust = 0.5),
           plot.caption = element_text(hjust = 1),
           axis.title.y = element_text(vjust = 0))
   
     
