library(tidyverse)
library(gt)
library(magrittr)
library(ggnewscale)

source("R/load_netball_data.R")
load_netball_data()

player_stats %>% 
  filter(season == 2022) %>% 
  group_by(squadNickname,playerId,round) %>% 
  filter(any(goal1 > 0)) %>% 
  summarise(across(c(contains("goal"),-contains("Perc"),attempt_from_zone1,attempt_from_zone2,rebounds),sum)) %>% 
  mutate(pct_from_zone1 = goal1/attempt_from_zone1,
         pct_from_zone2 = (goal2/attempt_from_zone2) %>% replace_na(0)) %>%
  group_by(squadNickname,playerId) %>% 
  mutate(mean_pct_from_zone1 = sum(goal1)/sum(attempt_from_zone1),
         mean_pct_from_zone2 = (sum(goal2,na.rm = T)/sum(attempt_from_zone2,na.rm = T)) %>% replace_na(0)) %>% 
  arrange(mean_pct_from_zone1) %>% 
  select(squadNickname,round,playerId,pct_from_zone1,pct_from_zone2,attempt_from_zone1,attempt_from_zone2,mean_pct_from_zone1,mean_pct_from_zone2) %>%
  pivot_longer(cols = c(pct_from_zone1,pct_from_zone2,attempt_from_zone1,attempt_from_zone2,mean_pct_from_zone1,mean_pct_from_zone2),
               names_to = c(".value","zone"),
               names_pattern = "(.*)_from_(.*)") %>% 
  left_join(player_info %>% filter(season == 2022)) %>% 
  ggplot(aes(x = pct, y = fct_inorder(surname), size=ifelse(attempt==0, NA, attempt),fill = factor(round))) +
  geom_point(aes(shape = "Game"),col = "black") +
  geom_point(aes(x = mean_pct,shape = "Mean")) +
  scale_shape_manual(values = c("Mean" = 4,"Game" = 21)) +
  scale_size(range = c(5,9)) +
  scale_fill_manual(values = c(c("#1E90FF", "#FFD700"))) +
  geomtextpath::geom_texthline(yintercept = 10.5,label = "80 % shooting average", linetype = "dashed") +
  geomtextpath::geom_texthline(yintercept = 16.5,label = "90 % shooting average", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0,1,0.1), labels = scales::percent) +
  facet_wrap(~zone) +
  labs(title = "",
    subtitle = "",
    y = "",
    x = "Shooting accuracy",
    fill = "Round",
    size = "Num attempts",
    shape = "Value",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black")) +
  theme_bw() 


player_stats %>% 
  filter(season == 2022) %>% 
  group_by(squadNickname,playerId,round) %>% 
  filter(any(attempt_from_zone1 > 0)) %>% 
  summarise(across(c(contains("goal"),-contains("Perc"),attempt_from_zone1,attempt_from_zone2,rebounds),sum)) %>% 
  mutate(pct_from_zone1 = goal1/attempt_from_zone1,
         pct_from_zone2 = (goal2/attempt_from_zone2) %>% replace_na(0)) %>%
  group_by(squadNickname,playerId) %>% 
  mutate(mean_pct_from_zone1 = sum(goal1)/sum(attempt_from_zone1),
         mean_pct_from_zone2 = (sum(goal2,na.rm = T)/sum(attempt_from_zone2,na.rm = T)) %>% replace_na(0)) %>% 
  arrange(mean_pct_from_zone1) %>% 
  select(squadNickname,round,playerId,pct_from_zone1,pct_from_zone2,attempt_from_zone1,attempt_from_zone2,mean_pct_from_zone1,mean_pct_from_zone2) %>%
  # pivot_longer(cols = c(pct_from_zone1,pct_from_zone2,attempt_from_zone1,attempt_from_zone2,mean_pct_from_zone1,mean_pct_from_zone2),
  #              names_to = c(".value","zone"),
  #              names_pattern = "(.*)_from_(.*)") %>% 
  left_join(player_info %>% filter(season == 2022)) %>% 
  filter(sum(attempt_from_zone1) > 10) %>% 
  ggplot(aes(x = pct_from_zone1, y = fct_inorder(surname), size=attempt_from_zone1,fill = factor(round))) +
  geom_point(aes(x = if_else(mean_pct_from_zone1 == pct_from_zone1,NA_real_,mean_pct_from_zone1),shape = "Mean accuracy"),size = 3) +
  geom_point(aes(shape = "Game accuracy"),col = "black") +
  geom_text(aes(label = attempt_from_zone1),size = 3) +
  scale_shape_manual(values = c("Average accuracy" = 4,"Game accuracy" = 21)) +
  scale_size_area(max_size = 12, guide = "none") +
  #scale_fill_manual(values = c(c("#1E90FF", "#FFD700"))) +
  geomtextpath::geom_texthline(yintercept = 5.5,label = "80 % shooting average", linetype = "dashed") +
  geomtextpath::geom_texthline(yintercept = 12.5,label = "90 % shooting average", linetype = "dashed") +
  # annotate("segment", x = 0.65, xend = 0.65, y = 13, yend = 15,
  #          arrow = arrow(ends = "last", angle = 45, type = "closed", length = unit(.2,"cm"))) +
  # annotate("segment", x = 0.65, xend = 0.65, y = 6, yend = 8,
  #          arrow = arrow(ends = "last", angle = 45, type = "closed", length = unit(.2,"cm"))) +
  scale_x_continuous(breaks = seq(0,1,0.1), labels = ~scales::percent(.x,accuracy = 1),limits = c(0.3,1)) +
  labs(title = "Normal shot accuracy",
       subtitle = "Shooting accuracy in rounds 1 & 2 of 2022 SSN season",
       y = "Player surname",
       x = "Shooting accuracy",
       fill = "Round",
       size = "Attempts",
       shape = "Point type",
       caption = "Data: Champion Data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black")) +
  guides(fill = guide_legend(override.aes = list(shape = c(16,16),col = c("#1E90FF","#FFD700"))),
         shape = guide_legend(override.aes = list(shape = c(4,16))))


player_stats %>% 
  ungroup() %>% 
  filter(season > 2016) %>% 
  mutate(goal1 = if_else(is.na(goal1),goals,goal1),
    attempt_from_zone1 = if_else(is.na(attempt_from_zone1),goalAttempts,attempt_from_zone1)) %>% 
  arrange(squadNickname,season,round) %>% 
  group_by(season,squadNickname,round) %>% 
  summarise(across(c(goal1,attempt_from_zone1),sum)) %>% 
  mutate(pct = goal1/attempt_from_zone1) %>%
  ggplot(aes(x = round,y = pct,group = squadNickname)) +
  geom_line(aes(col = factor(squadNickname)), size = 1.5) +
  geom_point(aes(fill = factor(squadNickname)),shape = 21,col = "black",show.legend = F) +
  scale_x_continuous(breaks = 1:14) +
  scale_colour_manual(values = SquadName_Colours) +
  scale_fill_manual(values = SquadName_Colours) +
  scale_y_continuous(labels = ~scales::percent(.x,accuracy = 1)) +
  theme_bw() +
  labs(title = "",
       subtitle = "",
       y = "Effective shooting percentage",
       x = "Round",
       colour = "Season",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black")) +
  facet_wrap(~season)


diamondsShooters <- c("1001711", "1001357", "993463", "80475", "1001708", "80113", "80052", "80293", "80072", "80009", "80067")


nm_shooting_stats <- 
  player_stats %>% 
  inner_join(diamondsShooters, by = "playerId") %>% 
  mutate(goal1 = if_else(is.na(goal1),goals,goal1),
         attempt_from_zone1 = if_else(is.na(attempt_from_zone1),goalAttempts,attempt_from_zone1)) %>% 
  arrange(displayName,season,round) %>% 
  group_by(season,squadNickname,playerId) %>%  
  summarise(across(c(goal1,attempt_from_zone1),sum),
            last_match = first(last_match),
            position = first(position)) %>% 
  mutate(pct = goal1/attempt_from_zone1,
         last_diamond = season == last_match) %>% 
  filter(attempt_from_zone1 > 0) %>% 
  left_join(player_info %>% 
              arrange(-season) %>% 
              group_by(playerId) %>% 
              slice_head(n = 1) %>%
              ungroup() %>% 
              distinct(playerId,displayName))


p1 <- nm_shooting_stats %>% 
  filter(position == "GA") %>% 
ggplot(aes(x = factor(season),col = factor(displayName),y = pct,group = displayName)) +
  geom_point(aes(shape = last_diamond),size = 3) +
  geom_line(size = 1.3) +
  scale_color_brewer(palette = "Set1") +
  geomtextpath::geom_texthline(yintercept = 0.85, label = "85% accuracy", linetype = "dashed") +
  scale_shape_manual(values = c(16,18), guide = "none") +
  scale_y_continuous(breaks = seq(0.5,1,0.1),labels = ~scales::percent(.x,accuracy = 1), limits = c(0.4,1)) +
  theme_bw() +
  labs(title = "",
       subtitle = "",
       y = "Shooting accuracy",
       x = "Season",
       colour = "Shooter",
       shape = "Last Diamonds Appearance",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black")) +
  facet_wrap(~position,nrow = 1)

p2 <- nm_shooting_stats %>% 
  filter(position == "GS") %>% 
  ggplot(aes(x = factor(season),y = pct,group = displayName,col = factor(displayName))) +
  geom_point(aes(shape = last_diamond),size = 3) +
  geom_line(size = 1.3) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16,18)) +
  geomtextpath::geom_texthline(yintercept = 0.85,label = "85% accuracy", linetype = "dashed") +
  scale_y_continuous(breaks = seq(0.5,1,0.1),labels = ~scales::percent(.x,accuracy = 1), limits = c(0.4,1)) +
  theme_bw() +
  labs(title = "",
       subtitle = "",
       y = "Shooting accuracy",
       x = "Season",
       colour = "Shooter",
       shape = "Last Diamonds Appearance",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black")) +
  facet_wrap(~position,nrow = 1)


p1 + p2


player_stats %>% 
  group_by(playerId) %>% 
  filter(squadNickname == "Diamonds") %>%
  filter(any(goalAttempts>0)) %>% 
  summarise(last_match = max(season)) %>%  
  clipr::write_clip()

tibble::tribble(
  ~playerId, ~last_match,
     80009L,       2016L,
     80052L,       2019L,
     80067L,       2015L,
     80072L,       2018L,
     80113L,       2021L,
     80293L,       2019L,
     80475L,       2022L,
    993463L,       2022L,
   1001357L,       2022L,
   1001708L,       2021L,
   1001711L,       2022L
  ) %>% left_join(player_info %>% select(playerId,surname) %>% 
                  distinct(playerId,.keep_all = T)) %>% clipr::write_clip()

diamondsShooters <-  tibble::tribble(
  ~playerId, ~last_match,   ~surname, ~position,
     80009L,       2016L, "Medhurst", "GA",
     80052L,       2019L, "Thwaites", "GS",
     80067L,       2015L,     "Bell", "GA",
     80072L,       2018L,  "Pratley", "GA",
     80113L,       2021L,  "Bassett", "GS",
     80293L,       2019L, "Caldwell", "GA",
     80475L,       2022L,     "Wood", "GA",
    993463L,       2022L,  "Tippett", "GA",
   1001357L,       2022L,   "Koenen", "GS",
   1001708L,       2021L,   "Austin", "GA",
   1001711L,       2022L,   "Garbin", "GS",
  1019169L, 2022L, "Dwyer", "GA"
  )



nm_shooting_stats %>% 
  filter(str_detect(displayName,paste("Wood","Austin","Garbin",collapse = "|",sep = "|"))) 
  inner_join(diamondsShooters) %>% 
  mutate(goal1 = if_else(is.na(goal1),goals,goal1),
         attempt_from_zone1 = if_else(is.na(attempt_from_zone1),goalAttempts,attempt_from_zone1)) %>% 
  arrange(displayName,season,round) %>% 
  group_by(season,squadNickname,displayName) %>% 
  summarise(across(c(goal1,attempt_from_zone1),sum),last_match = first(last_match)) %>% 
  mutate(pct = goal1/attempt_from_zone1)
  
  
  
  