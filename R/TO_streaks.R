library(tidyverse)

source("R/load_netball_data.R")
load_netball_data()

player_TO_data <- player_stats %>% 
  group_by(playerId) %>%
  mutate(no_TOs = if_else(generalPlayTurnovers == 0 & feeds > 5,"Yes","No"),
         cum_period = 1:n())

runs <- player_TO_data %>% 
  summarise(runs = list(rle(no_TOs))) %>% 
  mutate(rl = map(runs,function(run_list){
tibble(l = run_list$lengths,
       v = run_list$values) %>% 
  mutate(cum_period = cumsum(l)) %>% 
  filter(v == "Yes",l > 4)
})) %>%  
  select(-runs) %>% 
  unnest(rl) %>% 
  arrange(-l)

streak_labs <- 
  runs %>% 
  filter(l > 4) %>% 
  left_join(player_info %>% transmute(playerId,Name = paste(firstname,surname)) %>% distinct(playerId,.keep_all = T)) %>% 
    mutate(cum_period = cum_period - round(l/2)) %>% 
  left_join(player_TO_data %>% select(playerId,season,cum_period,round,period)) %>% 
  group_by(playerId) %>% 
  mutate(round_period = (round - 1) * 4 + period,
         max_streak = max(l))

 player_TO_data %>% 
  inner_join(runs %>% 
               filter(l > 4) %>% 
               group_by(playerId) %>% 
               summarise(max_streak = max(l),
                         streaks = list(l),
                         record_start = list(cum_period - l + 1),
                         record_finish = list(cum_period)) %>%  
               arrange(-max_streak),
             by = "playerId") %>% 
   left_join(player_info %>% transmute(playerId,Name = paste(firstname,surname)) %>% distinct(playerId,.keep_all = T)) %>% 
   ungroup() %>% 
   mutate(record_round = pmap_lgl(list(cum_period,record_start,record_finish),function(x,y,z) {
     map2_lgl(unlist(y),unlist(z), ~ between(x,.x,.y)) %>% any()}),
     no_TOs = if_else(record_round,"Record",no_TOs),
     no_TOs = fct_relevel(no_TOs,c("Record", "Yes", "No"))) %>% 
   mutate(round_period = (round - 1) * 4 + period) %>%  
   ggplot(aes(x = round_period,y = Name)) +
   geom_tile(aes(fill = no_TOs)) +
   geom_label(data = streak_labs,aes(label = l)) +
   scale_fill_manual(values = c("#FF3030", "#1C86EE","#A1A1A1")) +
   facet_grid(fct_rev(factor(max_streak))~season,scales = "free_y") +
   theme_bw() +
   labs(title = "Zero turnover streaks 2009 - 2022",
     subtitle = "Consecutive quarters with no turnovers and at least 5 feeds\nBest record is on the right had panels, ranging 5-8 consecutive quarters.\nGrey squares if TO occured or < 5 feeds, blue where no TO & > 5 feeds, and red is record streaks.",
     x = "Round-period (Periods from the start of the season)",
     y = "Player Name",
     fill = "Turnover Games",
     caption = "Data: Champion Data") +
   theme(plot.title = element_text(hjust = 0.5),
     plot.subtitle = element_text(hjust = 0.5),
     plot.caption = element_text(hjust = 1),
     plot.background = element_rect(colour = "black"))
 
 runs %>% 
   filter(l > 4) %>% 
   left_join(player_info %>% transmute(playerId,Name = paste(firstname,surname)) %>% distinct(playerId,.keep_all = T)) %>% 
   mutate(cum_period = cum_period - l) %>% 
   left_join(player_TO_data %>% select(playerId,season,cum_period,round,period)) %>% 
   ungroup() %>% 
   select(Name,season, round, period,streak = l) %>% 
   janitor::clean_names(case = "title") %>% 
   knitr::kable() %>% 
   kableExtra::kable_classic()
 