library(tidyverse)
source("R/load_netball_data.R")
load_netball_data()

plot_cols <- tibble(position = c("I","GS","GA","WA","C","WD","GD","GK"),
                    col = c("#a1a1a1","#f94144","#f3722c","#f8961e","#f9c74f","#90be6d","#43aa8b","#577590")) %>% 
  deframe()
 


player_stats %>% 
  group_by(season,round, surname,firstname,squadId) %>% 
  summarise(across(c(generalPlayTurnovers,feeds,minutesPlayed),sum)) %>%
  filter(generalPlayTurnovers == 0) %>% 
  left_join(team_info) %>% 
  arrange(-feeds) %>% 
  ungroup() %>% 
  transmute(Rank = dense_rank(desc(feeds)),season,round,Team = squadNickname,Player = paste(firstname,surname),feeds,turnovers = generalPlayTurnovers) %>% 
  janitor::clean_names(case = "upper_camel") %>% 
  head(27) %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic_2()


player_stats %>% 
  filter(season == 2014) %>% 
  group_by(season,round, surname,squadId) %>% 
  summarise(across(c(generalPlayTurnovers,feeds,minutesPlayed),sum)) %>%
  arrange(-feeds)

feed_percentages <- player_stats %>% 
  group_by(season,squadId,round,period) %>% 
  mutate(feed_pct = feeds/sum(feeds))%>% 
  filter(feed_pct > 0) %>% 
  group_by(season,squadId,playerId) %>% 
  summarise(feed_pct = mean(feed_pct)) %>% 
  ungroup()

most_common_pos <- bind_rows(player_match_stats %>% 
                               filter(season < 2016) %>% 
                               filter(!startingPositionCode %in% c("I","-")) %>% 
                               group_by(season,playerId,startingPositionCode) %>% 
                               summarise(n = n()) %>% 
                               #arrange(season,playerId,-n) %>% 
                               slice_max(n) %>% 
                               select(-n),
                             player_stats %>% 
                               filter(season >= 2016) %>% 
                               filter(!startingPositionCode %in% c("I","-")) %>% 
                               group_by(season,playerId,startingPositionCode) %>% 
                               summarise(n = n()) %>% 
                               #arrange(season,playerId,-n) %>% 
                               slice_max(n) %>% 
                               select(-n)) %>% 
  rename("most_common_pos" = "startingPositionCode")

player_stats %>% 
  group_by(season,squadId,playerId) %>% 
  summarise(across(c(minutesPlayed,generalPlayTurnovers,feeds),sum)) %>% 
  mutate(fpt = feeds/generalPlayTurnovers) %>% 
  left_join(player_info) %>% 
  mutate(name_lab = paste(season,displayName)) %>% 
  inner_join(feed_percentages) %>% 
  left_join(most_common_pos) %>% 
  filter(fpt < 100 & (feeds > 150 | (season == 2022 & feeds > 70)),most_common_pos != "WD") %>%
  ggplot(aes(feed_pct,fpt)) +
  geom_point(data = ~group_by(.,most_common_pos) %>% summarise(across(c(feed_pct,fpt),mean)),
             aes(col = most_common_pos), shape = 4,size = 6,stroke = 4) +
  geom_point(aes(col = most_common_pos), size = 1.5) +
  scale_x_continuous(labels = ~scales::percent(.x,1)) +
  #ggrepel::geom_text_repel(data = ~filter(.,((fpt < 12 & feed_pct > 0.48) | (most_common_pos == "C" & feed_pct > 0.44) | most_common_pos == "GS" |  (fpt > 14) | (feed_pct > 0.5) | (season == 2022 & fpt > 7.5)| (most_common_pos == "GA" & feed_pct > 0.4))),aes(label = name_lab),min.segment.length = 0) +
  scale_color_manual(values = c("#E41A1C", "#4DAF4A",  "#FF7F00", "#984EA3")) +
  #viridis::scale_color_viridis(discrete = T) +
  theme_bw() +
  labs(title = "Feeding efficiency in ANZC and SSN, 2009 - 2022",
       subtitle = "Each players feeds per turnover over a season, considering their feeding workload\nMost common position is assumed from a player's starting position and may be inaccurate in some circumstances.\nCrosses indicate mean value for position type. \nPlayers must have a minimum 150 feeds in a season to be eligible.",
       x = "Player % of team total feeds",
       y = "Feeds per turnover",
       col = "Most common position",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black")) + 
  guides(colour = guide_legend(override.aes = list(size = 2.5)))
  

# Shadows -----------------------------------------------------------------


player_stats %>% 
  group_by(season,squadId,playerId) %>% 
  summarise(across(c(minutesPlayed,generalPlayTurnovers,feeds),sum)) %>% 
  mutate(fpt = feeds/generalPlayTurnovers) %>% 
  left_join(player_info) %>% 
  mutate(name_lab = paste(season,displayName)) %>% 
  inner_join(feed_percentages) %>% 
  left_join(most_common_pos) %>% 
  filter(fpt < 100 & (feeds > 150 | (season == 2022 & feeds > 70)),most_common_pos != "WD") %>%
  ggplot(aes(feed_pct,fpt)) +
  geom_point(data = ~select(.,-most_common_pos),col = "grey") +
  geom_point(aes(col = most_common_pos), size = 2) +
  scale_x_continuous(labels = ~scales::percent(.x,1)) +
  ggrepel::geom_text_repel(data = ~filter(.,((fpt < 12 & feed_pct > 0.50) | (most_common_pos == "C" & feed_pct > 0.44) | most_common_pos == "GS" |  (fpt > 14) | (feed_pct > 0.5) | (most_common_pos == "GA" & (fpt > 9 | feed_pct > 0.35)))),aes(label = name_lab),min.segment.length = 0) +
  scale_color_manual(values = c("#E41A1C", "#4DAF4A",  "#FF7F00", "#984EA3")) +
  facet_wrap(~most_common_pos) +
  #viridis::scale_color_viridis(discrete = T) +
  theme_bw() +
  labs(title = "Feeding efficiency in ANZC and SSN, 2009 - 2022",
       subtitle = "Each players feeds per turnover over a season, considering their feeding workload\nMost common position is assumed from a player's starting position and may be inaccurate in some circumstances. Players must have a minimum 150 feeds in a season to be eligible.",
       x = "Player % of team total feeds",
       y = "Feeds per turnover",
       col = "Most common position",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"))

player_stats %>% 
  filter(season == 2010,playerId == 80014,feeds > 0) %>%
  distinct(round)
  