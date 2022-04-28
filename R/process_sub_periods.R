
library(superNetballR)

Season_id <- "11665"

game_register <- downloadMatch(Season_id,3,3)

#log = list(game_register)

log = append(log,list(game_register))

FEVvFIRR3M3 <- log

#saveRDS(FEVvFIRR3M3,file = "data/FEVvFIRR3M3.RDS")

safe_date <- possibly(.f = ~as.Date(.x) %>% lubridate::year(),otherwise = NA_real_)

tbl1 <- 
  tibble(player_stats = FEVvFIRR3M3 %>%
  map(~pluck(.x, "playerPeriodStats","player")),
  round = FEVvFIRR3M3 %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
  match = FEVvFIRR3M3 %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
  season = FEVvFIRR3M3 %>% map_chr(~pluck(.x,"matchInfo", "localStartTime",.default = NA_character_)) %>%
    safe_date(),
  periodSeconds = FEVvFIRR3M3 %>% map_int(~.x[["matchInfo"]][["periodSeconds"]])) %>%
  unnest(player_stats) %>% 
  unnest_wider(player_stats) %>% 
  select(season:match,round,period,playerId,squadId,periodSeconds,goalAttempts,goals,rebounds,generalPlayTurnovers) %>% 
  arrange(squadId,period,periodSeconds) %>% 
  group_by(playerId,periodSeconds) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  bind_rows(player_stats %>% 
      filter(round == rnd,season == ssn,match == mtch) %>% 
        select(season:match,period,playerId,squadId,goalAttempts,goals,rebounds,generalPlayTurnovers)) %>% 
  add_count(playerId, wt = goalAttempts) %>% 
  mutate(rebounds = if_else(n == 0,0L,rebounds)) %>% 
  group_by(squadId,period,periodSeconds) %>% 
  summarise(across(c(goalAttempts,goals,rebounds,generalPlayTurnovers),sum)) %>% 
  replace_na(list(periodSeconds = 900L)) %>% 
  arrange(squadId,period,periodSeconds) %>% 
  group_by(squadId,period) %>% 
  mutate(rebounds = case_when(
    squadId == 807 & period == 2 & periodSeconds == 560 ~ 2L,
    squadId == 807 & period == 2 & periodSeconds == 850 ~ 2L,
    squadId == 810 & period == 3 & periodSeconds == 850 ~ 1L,
    TRUE ~ rebounds),
    across(c(goalAttempts:generalPlayTurnovers),~if_else(is.na(lag(.x)),.x,.x - lag(.x)))) %>% 
  ungroup() %>% 
  left_join(FEVvFIRR3M3 %>%
              map_dfr(~pluck(.x, "playerSubs","player")) %>% 
              distinct(period,periodSeconds) %>% 
              filter(periodSeconds != 0) %>% 
              bind_rows(tibble(period = 1:4,periodSeconds = 900)), by = "period",
            suffix = c(".old","")) %>% 
  group_by(period,periodSeconds,squadId) %>%
  slice(which.min(abs(periodSeconds.old - periodSeconds))) %>% 
  select(-periodSeconds.old) %>% 
  ungroup()


  