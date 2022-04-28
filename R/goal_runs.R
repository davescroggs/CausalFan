goals %>%
  filter(season == 2021) %>% 
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
  filter(periodSeconds > 600) %>% View()


goals %>% 
  filter(periodSeconds > 10*60,squadNickname == "Vixens") %>% 
  mutate(SS_YN = str_detect(scoreName,"2")) %>%
  group_by(season,squadId,round) %>% 
  summarise(ss_pct = sum(SS_YN)/n(),
            y_order = sum(SS_YN)) %>% 
  mutate(season = factor(season)) %>% 
  left_join(team_info)


goals %>% 
  filter(periodSeconds > 10*60) %>% 
  mutate(SS_YN = str_detect(scoreName,"2")) %>%
  count(season,squadId,SS_YN, wt = scorepoints)

goals %>% 
  filter(periodSeconds > 10*60) %>% 
  mutate(SS_YN = str_detect(scoreName,"2")) %>%
  group_by(season,squadId) %>% 
  summarise(ss_pct = sum(SS_YN)/n(),
            y_order = sum(SS_YN)) %>% 
  mutate(season = factor(season)) %>% 
  left_join(team_info) %>% filter(squadNickname == "GIANTS")


t <- 
  goals %>%
  mutate(SS_YN = str_detect(scoreName,"2")) %>%
  arrange(season,playerId,round,period,periodSeconds) %>% 
  filter(periodSeconds > 600) %>% 
  group_by(season,playerId,SS_YN) %>%
  summarise(runs = rle(scoreName) %>% extract_rle() %>% list()) %>% 
  left_join(player_info)

t %>% unnest_auto(runs) %>% unnest(l,v) %>%  View()
  
tibble(l = t[[1]]$lengths,
       v = t[[1]]$values) %>%
  ggplot(aes(l)) +
  geom_bar() +
  facet_wrap(~v) +
  scale_x_continuous(breaks = 1:100)

get_runs <- function(out,in_here){
  x <- in_here
 bind_rows(out,tibble(l = x$lengths,
       v = x$values))
}

extract_rle <- function(x) {
  tibble(l = x$lengths,
         v = x$values)
}

reduce(t$runs,get_runs,.init = tibble(l = NA_real_,v = NA_character_)) %>% 
  filter(!is.na(l)) %>% 
  ggplot(aes(l)) +
  geom_bar() +
  facet_wrap(~v) +
  scale_x_continuous(breaks = 1:100)

goals %>% 
  arrange(season,playerId,round,period,periodSeconds) %>% 
  mutate(SS_YN = str_detect(scoreName,"2")) %>%
  left_join(player_info) %>% 
  filter(str_detect(surname,"Fowler"), periodSeconds > 600) %>% 
  group_by(season,SS_YN) %>% 
  mutate(id = 1:n()) %>% 
  ungroup() %>% 
  filter(scorepoints == 1) %>%
  mutate(gap = id != lag(id) + 1,
         n = 1:n()) %>%
  filter(gap)
  ggplot(aes(x = n,y = id,col = gap)) +
  geom_point()
