player_interactions %>% 
  bind_rows(season_2022) %>% 
  arrange(season,squadId, crosses) %>% 
  group_by(crosses) %>% 
  mutate(accum = cumsum(n)) %>%
  ungroup() %>% 
  arrange(squadId,season) %>%
  group_by(squadId,season) %>% 
  summarise(lists = list(unique(c(displayName.x,displayName.y))),.groups = "drop_last") %>% 
  mutate(staying = map2(lag(lists),lists,~intersect(unlist(.x),unlist(.y))),
         leaving = map2(lists,lead(lists),~setdiff(unlist(.x),unlist(.y))),
         new = map2(lists,lag(lists),~setdiff(unlist(.x),unlist(.y)))) %>% 
  left_join(team_info) -> player_transactions

player_transactions %>% 
  filter(squadNickname == "Vixens",season == 2020) %$% 
  unlist(leaving) -> gg

player_interactions %>% 
  arrange(season,squadId, crosses) %>% 
  group_by(crosses) %>% 
  mutate(accum = cumsum(n)) %>%
  ungroup()  %>% 
  filter(displayName.x %in% c(gg,"L.Watson") | displayName.y %in% c(gg,"L.Watson"),season == 2020) %$% sum(accum)

