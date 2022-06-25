library(tidyverse)
source("R/load_netball_data.R")
load_netball_data(2020:2022)

player_stats %>% 
  group_by(season,round,match,squadNickname,displayName) %>% 
  summarise(across(c(goal2,attempt_from_zone2),sum)) %>% 
  filter(attempt_from_zone2 > 0) %>% 
  mutate(team_supers = sum(goal2),
         acc = team_supers/sum(attempt_from_zone2)) %>% 
  arrange(-team_supers) %>% print(n = 50)

player_stats %>% 
  group_by(season,round,match,squadNickname) %>% 
  summarise(across(c(goal2,attempt_from_zone2),sum)) %>% 
  ungroup() %>% 
  add_count(goal2,attempt_from_zone2) %>% 
  mutate(label = paste("Round",round,season,"-",squadNickname),
         goi = if_else(season == 2022 & round == 9 & match == 2 & squadNickname == "GIANTS","firebrick3","black")) %>% View
  ggplot(aes(attempt_from_zone2,goal2)) +
  geom_point(aes(size = n)) +
  geomtextpath::geom_textabline(slope = 0.75,linetype = "dashed",label = "75% accuracy",hjust = 0.43) +
  ggrepel::geom_label_repel(data = ~filter(.,goal2 > 11 | attempt_from_zone2 > 20 | (goal2/attempt_from_zone2 >= 0.75 & goal2 > 10)),
                            aes(label = label,col = goi),
                            min.segment.length = 0,force = 0.5,force_pull = 0.5,box.padding = 0.5,max.overlaps = Inf,
                            show.legend = FALSE) +
  scale_colour_identity() +
  scale_size(breaks = c(1,3,5,7,9)) +
  scale_x_continuous(breaks = seq(0,25,5),minor_breaks = 0:25) +
  scale_y_continuous(breaks = seq(0,25,5),minor_breaks = 0:25) +
  theme_bw() +
  labs(title = "Great super shot perfomances",
    subtitle = "All super shot perfomances since introduction in 2020, the best are labelled, with the Giants' latest effort in red.\nThe size of the dot indicates the number of times that goal - attemps combination has occurred.\nThe dashed line shows the 75% accuracy margin, achieved by Giants on Saturday.",
    x = "Super attempts",
    y = "Super goals",
    size = "# occurances",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))
