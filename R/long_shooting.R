library(tidyverse)
source("R/load_netball_data.R")
load_netball_data(2009:2022)

goals %>% 
  count(distanceCode,scorepoints) %>% 
  arrange(scorepoints)

goals %>% colnames()
  mutate(across(c(distanceCode,positionCode,scorepoints),as.factor)) %>% 
  count(distanceCode,positionCode,.drop = F)


# 3 = baseline

temp <- read_csv("~/Downloads/inTheZone.csv")

temp %>% 
  filter(year > 2020) %>% 
  rename(round = roundNo,
         match = gameNo,
         season = year,
         squadNickname = squadName) %>% 
    left_join(goals,by = c("season", "squadNickname","playerId", "round", "match", "period", "periodSeconds")) %>% 
  distinct(positionCode,distanceCode,shotLocation) %>%
  filter(!is.na(positionCode)) %>% 
  arrange(positionCode,distanceCode) %>% 
  tidyr::extract(col = shotLocation,into = c("dir","dist"),regex = "(.*)-(\\w+$)") %>% clipr::write_clip()
  
distance_codes <- tibble::tribble(
              ~dir,   ~dist, ~positionCode, ~distanceCode,
            "left", "short",            0L,            0L,
            "left",  "long",            0L,            1L,
   "left-baseline",  "long",            0L,            3L,
          "middle", "short",            1L,            0L,
           "right", "short",            1L,            0L,
          "middle",  "long",            1L,            1L,
   "left-baseline", "short",            1L,            3L,
           "right", "short",            2L,            0L,
   "left-baseline", "short",            2L,            0L,
           "right",  "long",            2L,            1L,
  "right-baseline",  "long",            2L,            3L,
  "right-baseline", "short",            3L,            3L
  )



goals %>% 
  filter(season > 2016) %>% 
  left_join(distance_codes) %>% 
  group_by(season,squadNickname,
           #SS_p = (season > 2019 & periodSeconds > 600),
           dist) %>% 
  summarise(n = n())  %>% 
  left_join(goals %>% 
              group_by(season,squadNickname) %>% 
              summarise(n_match = n_distinct(round)) %>% 
              ungroup()) %>% 
  group_by(season,squadNickname) %>% 
  mutate(n = n/n_match,
         pct = scales::percent(n/sum(n))) %>%
  ggplot(aes(x = n,y = factor(squadNickname),fill = paste(dist))) + 
  geom_col() +
  geom_label(aes(label = pct),position = position_stack(vjust = 0.5)) +
  facet_wrap(~season,scales = "free_y")
  
shooting_stats %>% 
  left_join(team_info) %>% 
  left_join(distance_codes) %>% 
  group_by(year,squadNickname,dist) %>% 
  summarise(n = n(),
            n_match = n_distinct(round,match,series),
            lpg = n/n_match)  %>% 
  mutate(pct = scales::percent(lpg/sum(lpg))) %>% 
  #filter(dist == "long") %>% 
  ggplot(aes(x = lpg,y = factor(squadNickname),fill = dist)) + 
  geom_col() +
  geom_label(aes(label = pct),position = position_stack(vjust = 0.5)) +
  facet_wrap(~year,scales = "free_y")
  

goals %>% 
  #filter(season > 2016) %>% 
  left_join(distance_codes) %>% 
  group_by(season,
           #SS_p = (season > 2019 & periodSeconds > 600),
           dist) %>% 
  summarise(n = n())  %>% 
  left_join(goals %>% 
              group_by(season) %>% 
              summarise(n_match = n_distinct(round,squadNickname)) %>% 
              ungroup()) %>% 
  group_by(season) %>% 
  mutate(n = n/n_match,
         pct = scales::percent(n/sum(n))) %>%
  ggplot(aes(x = n,y = factor(season),fill = paste(dist))) + 
  geom_col() +
  geom_label(aes(label = pct),position = position_stack(vjust = 0.5))

shooting_stats %>% 
  left_join(team_info) %>% 
  left_join(distance_codes) %>% 
  group_by(year,dist) %>% 
  summarise(n = n(),
            n_match = n_distinct(round,match,series,squadNickname),
            lpg = n/n_match)  %>% 
  mutate(pct = scales::percent(lpg/sum(lpg))) %>% 
  #filter(dist == "long") %>% 
  ggplot(aes(x = lpg,y = factor(year),fill = dist)) + 
  geom_col() +
  geom_label(aes(label = pct),position = position_stack(vjust = 0.5))
  facet_wrap(~year,scales = "free_y")

  
  goals %>% 
    filter(season > 2016) %>% 
    left_join(distance_codes) %>% 
    group_by(season,
             dist,
             make_miss = str_extract(scoreName, "(G|g)oal|(m|M)iss") %>% tolower()) %>% 
    summarise(n = n()) %>%
    mutate(pct = n/sum(n)) %>% 
    ggplot(aes(x = pct,y = factor(season),fill = fct_rev(make_miss))) +
    geom_col() +
    facet_wrap(~dist, ncol = 1)
  