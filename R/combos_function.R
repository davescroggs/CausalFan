
sum_points <- function(s,a,b,c,x,y,z){
  goals %>%
    filter(season == s,round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$%
    sum(scorepoints)}

count_attempts <- function(s,a,b,c,x,y,z){
  goals %>%
    filter(season == s,round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %>% 
    nrow()
  }

sum_other <- function(c,x,y,z,sum_val){

  tbl1 %>%
    filter(squadId == c,period == x,between(periodSeconds,y + 1,z)) %>% 
    pull(sym(sum_val)) %>% 
    sum()}

process_combos <- function(ssn, rnd, mtch){

  teams <- schedule %>% 
    filter(round == rnd,season == ssn,match == mtch) %>% 
    select(home,away) %>%
    as_vector()
  
    map(teams,function(x){
  
player_stats %>% 
  select(season,round,match,period,squadId,playerId,startingPositionCode) %>% 
  filter(round == rnd,season == ssn,match == mtch,squadId == x) %>% 
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
  ungroup() %>% 
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
              rename(squadId = home, opposition = away)) %>% 


# Squad combinations ------------------------------------------------------
  rename(start_time = time) %>%
  group_by(season,round,match,squadId,period,start_time,end_time,time_tot,opposition) %>%
  summarise(squad = paste(startingPositionCode, surname, sep = "@",collapse = "; ")) %>%
  mutate(minutes_played = time_tot/60,
         goalsFor = pmap_dbl(list(season,round,match,squadId,period,start_time,end_time), sum_points),
         goalsAgainst = pmap_dbl(list(season,round,match,opposition,period,start_time,end_time), sum_points),
         attemptsFor = pmap_dbl(list(season,round,match,squadId,period,start_time,end_time), count_attempts),
         attemptsAgainst = pmap_dbl(list(season,round,match,opposition,period,start_time,end_time), count_attempts),
         plus_minus = goalsFor - goalsAgainst,
         gptFor = pmap_dbl(list(squadId,period,start_time,end_time,"generalPlayTurnovers"), sum_other),
         gptAgainst = pmap_dbl(list(opposition,period,start_time,end_time,"generalPlayTurnovers"), sum_other),
         oRebFor = pmap_dbl(list(squadId,period,start_time,end_time,"rebounds"), sum_other),
         oRebAgainst = pmap_dbl(list(opposition,period,start_time,end_time,"rebounds"), sum_other)) %>% 
  # mutate(p5_min = (if_else(end_time <= 900,end_time,900) - if_else(start_time > 600,start_time,600)),
  #        p5_min = round(if_else(p5_min < 0,0,p5_min)/60)) %>% 
  #arrange(-minutes_played) %>% 
  group_by(squad) %>%
  summarise(
    squad = unique(squad),
    minutes_played = (sum(end_time - start_time)/60) %>% round(0),
    start_time = first(start_time),
    start_period = first(period),
    end_time = last(end_time),
    end_period = last(period),
    across(minutes_played:oRebAgainst,sum)) %>%
  ungroup() %>%
  mutate(across(c(start_time,end_time),function(x) {
    sec = x %% 60 %>% as.character() %>%  str_pad(width = 2,pad = "0")
    min = (x / 60) %>%  floor()
    minsec = paste(min, sec,sep = ":")
  }),
  start_time = paste("Qtr",start_period,start_time),
  end_time = paste("Qtr",end_period,end_time)) %>%
  select(-c(start_period,end_period)) %>%
  separate_rows(squad,sep = "; ") %>%
  tidyr::extract(squad,c("pos","name"),regex = "(.*?)@(.*)") %>%
  pivot_wider(names_from = pos,values_from = name,values_fn = ~paste(.x,collapse = " ")) %>%
  arrange(start_time) %>%
  janitor::adorn_totals(fill = "") %>%
  transmute(GK,GD,WD,C,WA,GA,GS,
            `Start time` = start_time,
            `End time` = end_time,
            `Minutes Played` = minutes_played,
            `Goals For` = goalsFor,
            `Goals Against` = goalsAgainst,
            `Plus Minus` = goalsFor - goalsAgainst,
            `TO For` = gptFor, `TO Against` = gptAgainst,
            `Offensive Rtg` = (goalsFor/(attemptsFor + gptFor - oRebFor)*100) %>% round(1),
            `Defensive Rtg` = (goalsAgainst/(attemptsAgainst + gptAgainst - oRebAgainst)*100) %>% round(1))
})
}


output = process_combos(ssn,rnd,mtch)

output[[1]] 
output[[2]] %>% knitr::kable() %>% 
  kableExtra::kable_classic()


