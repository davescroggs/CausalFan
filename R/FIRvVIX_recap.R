library(gt)

goals %<>% 
  mutate(periodSeconds = if_else(periodSeconds == 0L,1L,periodSeconds))

gpm_vix <- 
  squads %>% 
  mutate(time_on = (end_time - start_time)/60,
         goalsFor = pmap_dbl(list(round,match,squadId,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(scorepoints)}),
         attemptsFor = pmap_dbl(list(round,match,squadId,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %>% 
             nrow()}),
         rebsFor = pmap_dbl(list(round,match,squadId,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           FIRvVIX %>% 
             filter(squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(oRebs)}),
         gptFor = pmap_dbl(list(round,match,squadId,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           FIRvVIX %>% 
             filter(squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(generalPlayTurnovers)}),
         goalsAgainst = pmap_dbl(list(round,match,opposition,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(scorepoints)}),
         attemptsAgainst = pmap_dbl(list(round,match,opposition,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %>% 
             nrow()}),
         rebsAgainst = pmap_dbl(list(round,match,opposition,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           FIRvVIX %>% 
             filter(squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(oRebs)}),
         gptAgainst = pmap_dbl(list(round,match,opposition,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           FIRvVIX %>% 
             filter(squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(generalPlayTurnovers)}))


gpm_vix %>% 
    mutate(id = c(1,1,2,3,3,4,5)) %>% 
  group_by(id) %>% 
  summarise(
    squad = unique(squad),
    minutes_played = sum(end_time - start_time)/60,
    start_time = first(start_time),
    start_period = first(period),
    end_time = last(end_time),
    end_period = last(period),
    across(matches("For|Against"),sum)) %>% 
  ungroup() %>% 
  mutate(across(c(start_time,end_time),function(x) {
    sec = x %% 60 %>% as.character() %>%  str_pad(width = 2,pad = "0")
    min = (x / 60) %>%  floor() 
    minsec = paste(min, sec,sep = ":")
  }),
  start_time = paste("Qtr",start_period,start_time),
  end_time = paste("Qtr",end_period,end_time)) %>% 
  select(-c(id,start_period,end_period)) %>% 
  #select(squad,start_time,end_time,goalsFor,attemptsFor, rebsFor,gptFor) %>% 
  separate_rows(squad,sep = "; ") %>% 
  tidyr::extract(squad,c("pos","name"),regex = "(.*?)-(.*)") %>% 
  pivot_wider(names_from = pos,values_from = name,values_fn = ~paste(.x,collapse = " ")) %>% 
  select(GK:GS,start_time,end_time,minutes_played,everything()) %>% 
  janitor::adorn_totals(fill = "") %>% 
  mutate(oRtg = (goalsFor*100/(attemptsFor - rebsFor + gptFor)) %>% round(.,digits = 1),
         dRtg = (goalsAgainst*100/(attemptsAgainst - rebsAgainst + gptAgainst)) %>% round(.,digits = 1),
         minutes_played = round(minutes_played)) %>% 
  gt() %>% 
  tab_spanner(
    label = "For",
    columns = c(goalsFor,attemptsFor, rebsFor, gptFor)
  ) %>%
  tab_spanner(
    label = "Against",
    columns = c(goalsAgainst, attemptsAgainst, rebsAgainst, gptAgainst)
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = c(GK,goalsFor,goalsAgainst,oRtg),
      rows = everything()
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = 20,
      rows = everything()
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = 1
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = GK == "Total"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = GK == "Total"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = list(
      cells_column_labels(gt::everything())
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = list(
      cells_column_spanners(gt::everything())
    )
  ) %>%
  cols_label(
    start_time = "Start",
    end_time = "Finish",
    minutes_played = "Mins played",
    goalsFor = "Goals",
    goalsAgainst = "Goals",
    attemptsFor = "Attempts",
    attemptsAgainst = "Attempts",
    rebsFor = "Rebs",
    rebsAgainst = "Rebs",
    gptFor = "TOs",
    gptAgainst = "TOs",
    oRtg = "Off Rtg",
    dRtg = "Def Rtg"
  ) %>% 
  tab_source_note(
    source_note = "Data via Champion Data")
  

# -------------------------------------------------------------------------

gpm_fir <- 
  squads %>% 
  mutate(time_on = (end_time - start_time)/60,
         goalsFor = pmap_dbl(list(round,match,squadId,period,start_time,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(scorepoints)}),
         attemptsFor = pmap_dbl(list(round,match,squadId,period,start_time,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %>% 
             nrow()}),
         rebsFor = pmap_dbl(list(round,match,squadId,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           FIRvVIX %>% 
             filter(squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(oRebs)}),
         gptFor = pmap_dbl(list(round,match,squadId,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           FIRvVIX %>% 
             filter(squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(generalPlayTurnovers)}),
         goalsAgainst = pmap_dbl(list(round,match,opposition,period,start_time,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(scorepoints)}),
         attemptsAgainst = pmap_dbl(list(round,match,opposition,period,start_time,end_time), function(a,b,c,x,y,z){
           goals %>% 
             filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %>% 
             nrow()}),
         rebsAgainst = pmap_dbl(list(round,match,opposition,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           FIRvVIX %>% 
             filter(squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(oRebs)}),
         gptAgainst = pmap_dbl(list(round,match,opposition,period,start_time + 1,end_time), function(a,b,c,x,y,z){
           FIRvVIX %>% 
             filter(squadId == c,period == x,between(periodSeconds,y,z)) %$% 
             sum(generalPlayTurnovers)}))


gpm_fir %>% 
  mutate(id = c(1,1,2,3,4)) %>% 
  group_by(id) %>% 
  summarise(
    squad = unique(squad),
    minutes_played = (sum(end_time - start_time)/60) %>% round(0),
    start_time = first(start_time),
    start_period = first(period),
    end_time = last(end_time),
    end_period = last(period),
    across(matches("For|Against"),sum)) %>% 
  ungroup() %>% 
  mutate(across(c(start_time,end_time),function(x) {
    sec = x %% 60 %>% as.character() %>%  str_pad(width = 2,pad = "0")
    min = (x / 60) %>%  floor() 
    minsec = paste(min, sec,sep = ":")
  }),
  start_time = paste("Qtr",start_period,start_time),
  end_time = paste("Qtr",end_period,end_time)) %>% 
  select(-c(id,start_period,end_period)) %>% 
  #select(squad,start_time,end_time,goalsFor,attemptsFor, rebsFor,gptFor) %>% 
  separate_rows(squad,sep = "; ") %>% 
  tidyr::extract(squad,c("pos","name"),regex = "(.*?)-(.*)") %>% 
  pivot_wider(names_from = pos,values_from = name,values_fn = ~paste(.x,collapse = " ")) %>% 
  select(GK:GS,start_time,end_time,minutes_played,everything()) %>% 
  janitor::adorn_totals(fill = "") %>% 
  mutate(oRtg = (goalsFor*100/(attemptsFor - rebsFor + gptFor)) %>% round(.,digits = 1),
         dRtg = (goalsAgainst*100/(attemptsAgainst - rebsAgainst + gptAgainst)) %>% round(.,digits = 1),
         minutes_played = round(minutes_played)) %>% 
  gt() %>% 
  tab_spanner(
    label = "For",
    columns = c(goalsFor,attemptsFor, rebsFor, gptFor)
  ) %>%
  tab_spanner(
    label = "Against",
    columns = c(goalsAgainst, attemptsAgainst, rebsAgainst, gptAgainst)
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = c(GK,goalsFor,goalsAgainst,oRtg),
      rows = everything()
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = 20,
      rows = everything()
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = 1
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = GK == "Total"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = GK == "Total"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = list(
      cells_column_labels(gt::everything())
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = list(
      cells_column_spanners(gt::everything())
    )
  ) %>%
    cols_label(
      start_time = "Start",
      end_time = "Finish",
      minutes_played = "Mins played",
      goalsFor = "Goals",
               goalsAgainst = "Goals",
               attemptsFor = "Attempts",
               attemptsAgainst = "Attempts",
               rebsFor = "Rebs",
               rebsAgainst = "Rebs",
               gptFor = "TOs",
               gptAgainst = "TOs",
               oRtg = "Off Rtg",
               dRtg = "Def Rtg"
               ) %>% 
  tab_source_note(
    source_note = "Data via Champion Data")




# -------------------------------------------------------------------------

player_stats %>% 
  mutate(rebounds = if_else(goalAttempts == 0,0L,rebounds)) %>% 
  filter(round == 1, match == 3) %>% 
  group_by(squadNickname) %>% 
  summarise(across(c(goal1,goal2,goalAttempts,rebounds,generalPlayTurnovers),sum)) %>% 
transmute(squadNickname,
  goals  = (goal1 + goal2*2),
  possessions = (goalAttempts - rebounds + generalPlayTurnovers),
  oRtg = (goals/possessions*100) %>% round(.,digits = 2)) %>% 
  rename("Team" = "squadNickname","Goals" = "goals","Possessions" = "possessions","Off Rating" = "oRtg") %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic()

o_rtg  %>% 
  transmute(round,match,opponent = squadNickname,dPoss = possessions,dGoals = goals,dFeeds = feeds) %>% 
  full_join(o_rtg) %>% 
  filter(opponent != squadNickname) %>% 
  group_by(season, squadNickname) %>% 
  summarise(
    poss = sum(possessions)/n(), 
    oRtg = sum(goals)/sum(possessions)*100,
            dRtg = sum(dGoals)/sum(dPoss)*100,
            .groups = "drop_last") %>% 
  ungroup() %>% 
  mutate(across(c(poss,oRtg,dRtg),~round(.x,1))) %>% 
  arrange(squadNickname,season) %>% 
  filter(season %in% 2020:2021,squadNickname %in% c("Firebirds","Vixens")) %>% 
  rename("Season" = "season","Team" = "squadNickname","Possessions" = "poss","Off Rating" = "oRtg","Def Rating" = "dRtg") %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic()

goals %>% 
  
  left_join(schedule)
  mutate(score_difference = if_else(homeTeam == 1,score_difference,-score_difference),
         make_miss = if_else(str_detect(scoreName,"goal|Goal"),"make",scoreName),
         minutes = secondsElapsed/60)
  
  plot_data <- goals %>%
    filter(round == 1,match == 3) %>% 
    left_join(schedule, by = c("season", "round", "match")) %>% 
    group_by(season, round,match) %>% 
    mutate(SS_YN = if_else(str_detect(scoreName,"2"),"Super","Normal"),
      score_difference = if_else(squadId == home, scorepoints,-scorepoints) %>%
             cumsum(),
           make_miss = if_else(str_detect(scoreName,"goal|Goal"),"make",scoreName),
           minutes = ((period -1)*900 + periodSeconds)/60)

  game_names <- 
    goals %>% 
    filter(round == 1,match == 3) %>%
    left_join(schedule, by = c("season", "round", "match")) %>% 
    arrange(squadId == home) %>% 
    distinct(squadNickname) %>% 
      mutate(squadNickname = map_chr(squadNickname,~str_pad(.x,width = 35,side = "both"))) %$% 
    paste(squadNickname,collapse = "v") %>% 
    str_trim()

ggplot(data = plot_data, aes(x = minutes,y = score_difference,group = 1)) +
  geom_step(aes(col = SS_YN)) +
  #geom_point(data = filter(plot_data,make_miss %in% c("2pt Miss","miss")),aes(col = make_miss),shape = 1,size = 2) +
  # geom_point(data = plot_data %>% slice_min(score_difference,n = 1,with_ties = F),shape = 16,size = 4,col = "red") +
  # geom_point(data = plot_data %>% slice_max(score_difference,n = 1,with_ties = F),shape = 16,size = 4,col = "red") +
  geom_hline(yintercept = 0,linetype = "dashed") +
  geom_vline(xintercept = c(15,30,60)) +
  geomtextpath::geom_textvline(xintercept = (900*2+7*60+57)/60, label = "Wallam/Jenner sub off",linetype = "dashed",hjust = 0.8) +
  geomtextpath::geom_textvline(xintercept = 45, label = "Wallam/Jenner/Dunkley sub on",hjust = 0.8) +
  annotate(geom = "label", x = seq(7.5,52.5,15),y = 27,label = c("Qtr 1","Qtr 2","Qtr 3","Qtr 4")) +
  scale_colour_manual(values = c("Super" = "firebrick1","Normal" = "black")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black")) +
  scale_x_continuous(breaks = seq(0,60,5),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-25,25,5),limits = c(-28,28)) +
  labs(y = game_names,
       col = "Shot type",
       x = "Game minute",
       title = "Firebirds v Vixens",
       subtitle = "Round 1 score worm",
       caption = "Plot adapted from Champion Data Match Centre")

o_rtg  %>% 
  mutate(
    oRtg = (goals)/(possessions)*100) %>% 
  arrange(-oRtg) %>% View
