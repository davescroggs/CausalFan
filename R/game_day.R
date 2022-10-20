library(tidyverse)
library(gt)
library(gtExtras)
library(magrittr)
library(ggnewscale)
library(glue)

source("R/load_netball_data.R")
load_netball_data(2017:2022)

rnd = 16
ssn = 2022
mtch = 1

teams <- schedule %>% 
  filter(round == rnd,season == ssn,match == mtch) %>% 
  select(home,away) %>%
  as_vector() 

team_list = team_info %>% select(squadId,squadNickname) %>% deframe()
team_names <- map_chr(teams,~team_list[as.character(.x)])

#SquadName_Colours <- SquadName_Colours[names(SquadName_Colours) %in% team_names]



o_rtg <- 
  player_stats %>% 
  group_by(season,round,match,squadNickname) %>% 
  summarise(across(c(goals,goal1,goal2, goalAttempts, goalMisses, generalPlayTurnovers,turnovers,feeds),~sum(.,na.rm = T))) %>%
  left_join(player_stats %>% 
              group_by(playerId) %>% 
              filter(any(goals > 0)) %>% 
              ungroup() %>% 
              count(season,round, match, squadNickname,wt = rebounds,name = "offensiveRebounds"),
            by = c("round", "match", "squadNickname","season")) %>% 
  mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers)

# Season comparison -------------------------------------------------------

o_rtg  %>% 
  transmute(round,match,opponent = squadNickname,dPoss = possessions,dGoals = goals,dFeeds = feeds) %>% 
  full_join(o_rtg) %>% 
  filter(opponent != squadNickname) %>% 
  filter(season %in% 2022,squadNickname %in% team_names) %>% 
  group_by(season, squadNickname) %>% 
  summarise(
    Possessions = (sum(possessions)/n()) %>% round(0),
    `Offensive Rating` = (sum(goals)/sum(possessions)*100) %>% round(0),
    `Defensive Rating` = (sum(dGoals)/sum(dPoss)*100) %>% round(0)) %>%  
  arrange(squadNickname,season) %>% 
  mutate(measure = paste(season,"average")) %>% 
  # rename("Season" = "season","Team" = "squadNickname","Possessions per game" = "ppg","Off Rating" = "oRtg","Def Rating" = "dRtg") %>% 
  # knitr::kable() %>% 
  # kableExtra::kable_classic()
bind_rows(
o_rtg  %>% 
  filter(season == ssn,round == rnd,match == mtch) %>% 
  group_by(season,round, squadNickname) %>% 
  summarise(
    Possessions = (sum(possessions)/n()) %>% round(0),
    `Offensive Rating` = (sum(goals)/sum(possessions)*100) %>% round(0)) %>%
  full_join(.,.,by = c("season","round"), suffix = c("",".new")) %>% 
  filter(squadNickname != squadNickname.new) %>% 
  ungroup() %>% 
  mutate(measure = paste0(season," R",round)) %>% 
  select(everything(),-c(round,Possessions.new,squadNickname.new),`Defensive Rating` = `Offensive Rating.new`),
o_rtg  %>% 
  transmute(round,match,opponent = squadNickname,dPoss = possessions,dGoals = goals,dFeeds = feeds) %>% 
  full_join(o_rtg) %>% 
  filter(opponent != squadNickname) %>% 
  filter(season %in% 2022) %>% 
  group_by(season) %>% 
  summarise(
    Possessions = (sum(possessions)/n()) %>% round(0),
    `Offensive Rating` = (sum(goals)/sum(possessions)*100) %>% round(0),
    `Defensive Rating` = (sum(dGoals)/sum(dPoss)*100) %>% round(0)) %>%  
  mutate(squadNickname = "League",
    measure = paste(season,"average"))) %>% 
  rename("Team" = "squadNickname","Measure" = "measure") %>% 
  pivot_longer(-c(season, Team,Measure)) %>% 
  mutate(Team = factor(Team),
         Team = fct_relevel(Team,"League",after = 1),
         name = factor(name,levels = c("Offensive Rating", "Defensive Rating", "Possessions"),ordered = T),
         gor = str_detect(Measure,"R\\d")) %>%
  ggplot(aes(x = value, y = Team)) +
  geom_point(aes(fill = Measure,size = gor,shape = gor),col = "black") +
  geom_text(aes(label = value), col = "black") +
  scale_fill_manual(values = c("#1E90FF", "#FF3030")) +
  scale_size_manual(values = c("TRUE" = 9,"FALSE" = 7),guide = "none") +
  scale_shape_manual(values = c("TRUE" = 23,"FALSE" = 21),guide = "none") +
  facet_wrap(~name,ncol = 1) +
  expand_limits(x = c(60,90)) +
  theme_minimal() +
  labs(title = "Team rating",
    subtitle = glue("Possessions, Offensive and denfensive ratings\nfrom round {rnd}, 2021 and 2022 season so far"),
    x = "Rating/Count",
    fill = "Legend",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"),
    panel.grid.major.y = element_line(colour = "black")) + 
  guides(fill = guide_legend(override.aes = list(fill = c("#1E90FF", "#FF3030"),
                                                 col = rep("black",2),
                                                 shape = c(21,23),
                                                 size = c(3,5))))




# Game day O Rating -------------------------------------------------------

win_loss <- team_stats %>% 
  filter(season == 2022) %>% 
  group_by(season,round,match,squadId) %>% 
  summarise(goals = sum(goals)) %>% 
  full_join(.,.,by = c("season","round","match"),
            suffix = c("",".join")) %>% 
  filter(squadId != squadId.join) %>%
  mutate(wl = case_when(goals == goals.join ~ 0.5,
                        goals < goals.join ~ 0,
                        goals > goals.join ~ 1,
                        TRUE ~ NA_real_)) %>% 
  left_join(team_info) %>% 
  select(-goals,-contains(".join"),-squadId)

o_rtg %>% 
  ungroup() %>% 
  transmute(season,round,match,squadNickname,
    off_rtg = (goals/possessions*100) %>% round(1)) %>% 
  arrange(squadNickname,round) %>% 
  full_join(.,.,by = c("season", "round","match"),suffix = c("",".join")) %>% 
  filter(season == 2022,squadNickname %in% team_names,squadNickname != squadNickname.join) %>% 
  rename(def_rtg = off_rtg.join, opponent = squadNickname.join) %>% 
  left_join(win_loss, by = c("squadNickname", "season", "round")) %>% 
  transmute(squadNickname,Round = round,off_rtg,def_rtg,wl,Opponent = opponent) %>% 
  group_by(squadNickname) %>% 
  rename(`Off Rating` = off_rtg,
         `Def Rating` = def_rtg,
         `Win/Loss` = wl) %>% 
  #janitor::clean_names(case = "sentence")
  gt() %>% 
  gt::opt_table_lines(extent = "all") %>%
  gtExtras::gt_highlight_rows(columns = 5:6,
                              rows = `Win/Loss` == "Win",
                              fill = "#013369") %>% 
  gtExtras::gt_highlight_rows(columns = 5:6,
                              rows = `Win/Loss` == "Loss",
                              fill = "#D50A0A")

o_rtg %>% 
  ungroup() %>% 
  transmute(season,round,match,squadNickname,
            off_rtg = (goals/possessions*100) %>% round(1),
            goals) %>% 
  arrange(squadNickname,round) %>% 
  full_join(.,.,by = c("season", "round","match"),suffix = c("",".join")) %>% 
  filter(season == 2022,squadNickname != squadNickname.join) %>% 
  rename(def_rtg = off_rtg.join, opponent = squadNickname.join) %>% 
  left_join(win_loss, by = c("squadNickname", "season", "round")) %>% 
  transmute(Team = squadNickname,
            Round = round,
            off_rtg,
            def_rtg,wl,
            Opponent = opponent,
            goals,goals.join) %>% 
  group_by(Team) %>% 
  summarise(Wins = sum(wl),
            `Off rating` = list(off_rtg),
            `Def rating` = list(def_rtg),
            Outcomes = list(wl),
            goal_diff = sum(goals)/sum(goals.join)) %>% 
  arrange(-Wins,-goal_diff) %>% 
  select(-goal_diff) %>% 
  gt() %>% 
  gtExtras::gt_sparkline(`Off rating`) %>% 
  gtExtras::gt_sparkline(`Def rating`) %>% 
  gtExtras::gt_plt_winloss(Outcomes,max_wins = 7)


game_ortg <- player_stats %>%
  group_by(season,round,match,squadNickname,period) %>% 
  summarise(across(c(goals,goal1,goal2, goalAttempts, goalMisses, generalPlayTurnovers,turnovers,feeds),~sum(.,na.rm = T))) %>%
  left_join(player_stats %>% 
              group_by(playerId) %>% 
              filter(any(goals > 0)) %>% 
              ungroup() %>% 
              count(season,round, match, squadNickname,period,wt = rebounds,name = "offensiveRebounds"),
            by = c("round", "match", "squadNickname","season","period")) %>% 
  mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers)  %>% 
  filter(round == rnd,season == ssn,match == mtch) %>% 
  ungroup()

game_ortg %>% 
  filter(squadNickname == team_names[1]) %>% 
  transmute(Team = squadNickname,
            Period = as.character(period),
            Possessions = possessions,
            Turnovers = generalPlayTurnovers,
            Misses = goalMisses,
            `Off Rebounds` = offensiveRebounds, 
            goalAttempts,
            goals) %>% 
  janitor::adorn_totals() %>% 
            mutate(`Shooting (eff)` = paste0(goals,"/",goalAttempts),
                   `Off Rating` = round(goals/Possessions*100,1)) %>% 
  select(-goals,-goalAttempts) %>% 
  bind_rows(
    game_ortg %>% 
      filter(squadNickname == team_names[2]) %>% 
      transmute(Team = squadNickname,
                Period = as.character(period),
                Possessions = possessions,
                Turnovers = generalPlayTurnovers,
                Misses = goalMisses,
                `Off Rebounds` = offensiveRebounds, 
                goalAttempts,
                goals) %>% 
      janitor::adorn_totals() %>% 
      mutate(`Shooting (eff)` = paste0(goals,"/",goalAttempts),
             `Off Rating` = round(goals/Possessions*100,1)) %>% 
      select(-goals,-goalAttempts)) %>% 
  gt() %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(2.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = Team == "Total"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "black",
      weight = px(2.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = "Team",
      rows = everything()
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom","top"),
      color = "black",
      weight = px(2.5),
      style = "solid"
    ),
    locations = list(
      cells_column_labels(gt::everything())
    )
  ) %>% 
  gt::tab_footnote("Effective shooting % = (goals + supershot goals * 2)/attempts",cells_column_labels(
    columns = `Shooting (eff)`)) %>% 
  tab_source_note(
    source_note = "Data: Champion Data")
  

# Recent games + O Rating -------------------------------------------------



# Squads ------------------------------------------------------------------


combos <- player_stats %>% 
  select(season,round,match,period,squadId,playerId,startingPositionCode) %>% 
  filter(round == rnd,season == ssn,match == mtch) %>% 
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
  ungroup()

# Goals Per Minute -------------------------------------------------------

squads <- 
combos %>% 
  filter(match == mtch,squadId == team_info$squadId[team_info$squadNickname == team_names[2]]) %>%
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
              rename(squadId = home, opposition = away))

# gpm <- squads %>% 
#   mutate(time_on = (end_time - start_time)/60,
#          goalsFor = pmap_dbl(list(round,match,squadId,period,start_time,end_time), function(a,b,c,x,y,z){
#            goals %>% 
#              filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
#              sum(scorepoints)}),
#          goalsAgainst = pmap_dbl(list(round,match,opposition,period,start_time,end_time), function(a,b,c,x,y,z){
#            goals %>% 
#              filter(round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
#              sum(scorepoints)}),
#          plus_minus = goalsFor - goalsAgainst)
# 
# # Squad combinations ------------------------------------------------------
# 
# gpm %>% 
#   filter(match == 3,squadId == 8119) %>% 
#   mutate(minutes_played = round((end_time - start_time)/60),
#          p5_min = (if_else(end_time <= 900,end_time,900L) - if_else(start_time > 600,start_time,600L)),
#          p5_min = round(if_else(p5_min < 0,0L,p5_min)/60)) %>% 
#   group_by(squad) %>% 
#   summarise(across(c(minutes_played,p5_min, plus_minus),sum)) %>% 
#   arrange(-minutes_played) %>% 
#   separate_rows(squad,sep = "; ") %>% 
#   tidyr::extract(squad,c("pos","name"),regex = "(.*)-(.*)") %>% 
#   pivot_wider(names_from = pos,values_from = name) 
#   select(GK:GS,`Minutes Played` = minutes_played,`Power 5 minutes` = p5_min, `Plus Minus` = plus_minus) %>% 
#   knitr::kable() %>% 
#   kableExtra::kable_classic()
  
  
  squads %>% 
    rename(start_time = time) %>% 
    group_by(season,round,match,squadId,period,start_time,end_time,time_tot,opposition) %>% 
    summarise(squad = paste(startingPositionCode, surname, sep = "@",collapse = "; ")) %>% 
    mutate(minutes_played = time_tot/60,
           goalsFor = pmap_dbl(list(season,round,match,squadId,period,start_time,end_time), function(s,a,b,c,x,y,z){
             goals %>% 
               filter(season == s,round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
               sum(scorepoints)}),
           goalsAgainst = pmap_dbl(list(season,round,match,opposition,period,start_time,end_time), function(s,a,b,c,x,y,z){
             goals %>% 
               filter(season == s,round == a,match == b, squadId == c,period == x,between(periodSeconds,y,z)) %$% 
               sum(scorepoints)}),
           plus_minus = goalsFor - goalsAgainst) %>% 
    mutate(p5_min = (if_else(end_time <= 900,end_time,900L) - if_else(start_time > 600,start_time,600L)),
           p5_min = round(if_else(p5_min < 0,0L,p5_min)/60)) %>% 
    #arrange(-minutes_played) 
    group_by(squad) %>% 
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
    select(-c(start_period,end_period)) %>% 
    separate_rows(squad,sep = "; ") %>% 
    tidyr::extract(squad,c("pos","name"),regex = "(.*?)@(.*)") %>% 
    pivot_wider(names_from = pos,values_from = name,values_fn = ~paste(.x,collapse = " ")) %>% 
    arrange(start_time) %>% 
    transmute(GK,GD,WD,C,WA,GA,GS,start_time, end_time,`Minutes Played` = minutes_played, `Plus Minus` = goalsFor - goalsAgainst) %>%
    janitor::adorn_totals(fill = "") %>% 
    knitr::kable() %>% 
    kableExtra::kable_classic()

  

# Score worm --------------------------------------------------------------

  plot_data <- goals %>%
    filter(season == ssn,round == rnd,match == mtch) %>% 
    left_join(schedule, by = c("season", "round", "match")) %>% 
    group_by(season, round,match) %>% 
    mutate(SS_YN = if_else(str_detect(scoreName,"2"),"Super","Normal"),
           score_difference = if_else(squadId == home, scorepoints,-scorepoints) %>%
             cumsum(),
           make_miss = if_else(str_detect(scoreName,"goal|Goal"),"make",scoreName),
           minutes = ((period -1)*900 + periodSeconds)/60)
  
  game_names <- 
    rev(team_names) %>% 
    str_pad(width = 35,side = "both") %>% 
    paste(collapse = "v") %>% 
    str_trim()
  
  ggplot(data = plot_data, aes(x = minutes,y = score_difference,group = 1)) +
    #geom_step(aes(col = SS_YN)) +
    geom_line(aes(col = SS_YN)) +
    #geom_point(data = filter(plot_data,make_miss %in% c("2pt Miss","miss")),aes(col = make_miss),shape = 1,size = 2) +
    # geom_point(data = plot_data %>% slice_min(score_difference,n = 1,with_ties = F),shape = 16,size = 4,col = "red") +
    # geom_point(data = plot_data %>% slice_max(score_difference,n = 1,with_ties = F),shape = 16,size = 4,col = "red") +
    geom_hline(yintercept = 0,linetype = "dashed") +
    geom_vline(xintercept = c(15,30,45,60)) +
    # geomtextpath::geom_textvline(xintercept = 10, label = "Power 5",linetype = "dashed",hjust = 0.8) +
    # geomtextpath::geom_textvline(xintercept = 25, label = "Power 5",linetype = "dashed",hjust = 0.8) +
    # geomtextpath::geom_textvline(xintercept = 40, label = "Power 5",linetype = "dashed",hjust = 0.8) +
    # geomtextpath::geom_textvline(xintercept = 55, label = "Power 5",linetype = "dashed",hjust = 0.8) +
    #geomtextpath::geom_textvline(xintercept = 45, label = "Wallam/Jenner/Dunkley sub on",hjust = 0.8) +
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
         title = paste(team_names,collapse = " v "),
         subtitle = paste("Round",rnd, "score worm"),
         caption = "Plot adapted from Champion Data Match Centre")
  

# |- Score worm condensed ----------------------------------------------------
  game_names <- 
    rev(team_names) %>% 
    str_pad(width = 20,side = "both") %>% 
    paste(collapse = "v") %>% 
    str_trim() %>% paste("\n")
  
  ggplot(data = plot_data, aes(x = minutes,y = score_difference,group = 1)) +
    geom_smooth(method = 'loess', span = 0.05,se = F,col = "black") +
    geom_hline(yintercept = 0,) +
    geom_vline(xintercept = c(15,30,45,60)) +
    geom_point(data = ~tail(.,1),col = "red") +
    ggrepel::geom_text_repel(data = ~tail(.,1),aes(label = abs(score_difference)),nudge_x = 1) +
    scale_colour_manual(values = c("Super" = "firebrick1","Normal" = "black")) +
    theme_void() +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 1),
          #plot.background = element_rect(colour = "black"),
          axis.title.y = element_text(angle = 90),
          plot.margin = margin(15, 15, 15, 15,unit = "pt")) +
    scale_x_continuous(breaks = seq(0,60,5),expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(-25,25,5),limits = c(-28,28)) +
    labs(y = game_names,
         caption = "Data: Champion Data") +
    coord_cartesian(xlim = c(0,62),clip = "off")

# Shooters ----------------------------------------------------------------

player_stats %>%   
    filter(season == ssn,round == rnd,match == mtch,squadNickname == team_names[1]) %>%
    group_by(playerId) %>% 
    filter(any(attempt_from_zone1 > 0)) %>% 
    group_by(squadNickname,period) %>% 
    summarise(across(c(goals,goal1,goal2,attempt_from_zone1,attempt_from_zone2),sum)) %>% 
    ungroup() %>% 
    #inner_join(player_info %>% distinct(playerId,displayName),by = "playerId") %>% 
    mutate(period = as.character(period)) %>% 
    janitor::adorn_totals() %>% 
    mutate(normals = paste0(goal1,"/",attempt_from_zone1),
              normal_pct = scales::percent(goal1/attempt_from_zone1,1),
              supers = paste0(goal2,"/",attempt_from_zone2),
              super_pct = scales::percent(goal2/attempt_from_zone2,1),
              across(c(normal_pct,super_pct),replace_na,replace = "0%")) %>% 
    select(-c(goal1,goal2,attempt_from_zone1,attempt_from_zone2)) %>% 
    bind_rows(
      player_stats %>%   
        filter(season == ssn,round == rnd,match == mtch,squadNickname == team_names[2]) %>%
        group_by(playerId) %>% 
        filter(any(attempt_from_zone1 > 0)) %>% 
        group_by(squadNickname,period) %>% 
        summarise(across(c(goals,goal1,goal2,attempt_from_zone1,attempt_from_zone2),sum)) %>% 
        ungroup() %>% 
        #inner_join(player_info %>% distinct(playerId,displayName),by = "playerId") %>% 
        mutate(period = as.character(period)) %>% 
        janitor::adorn_totals() %>% 
        mutate(normals = paste0(goal1,"/",attempt_from_zone1),
               normal_pct = scales::percent(goal1/attempt_from_zone1,1),
               supers = paste0(goal2,"/",attempt_from_zone2),
               super_pct = scales::percent(goal2/attempt_from_zone2,1),
               across(c(normal_pct,super_pct),replace_na,replace = "0%")) %>% 
        select(-c(goal1,goal2,attempt_from_zone1,attempt_from_zone2))) %>% 
    gt() %>%
    tab_style(
      style = cell_borders(
        sides = c("bottom","top"),
        color = "black",
        weight = px(2.5),
        style = "solid"
      ),
      locations = cells_body(
        columns = everything(),
        rows = squadNickname == "Total"
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "right",
        color = "black",
        weight = px(2.5),
        style = "solid"
      ),
      locations = cells_body(
        columns = "squadNickname",
        rows = everything()
      )
    ) %>%
    cols_label(
      goals = "Goals",
      squadNickname = "Team",
      period = "Period",
      normals = "Normals",
      normal_pct = "%",
      supers = "Supers",
      super_pct = "%"
    )  %>%
    tab_style(
      style = cell_borders(
        sides = c("bottom","top"),
        color = "black",
        weight = px(2.5),
        style = "solid"
      ),
      locations = list(
        cells_column_labels(gt::everything())
      )
    ) %>% 
    tab_source_note(
      source_note = "Data: Champion Data")

tName <- team_names[1]
  
player_stats %>% 
  filter(season > 2019,squadNickname == tName) %>% 
  group_by(season,round) %>% 
  summarise(across(c(goals,goalAttempts),sum)) %>% 
  mutate(pct = goals/goalAttempts,
         roi = if_else(season == 2022 & round == 2,"Yes","No")) %>% 
  ggplot(aes(x = round,y = pct,group = season)) +
  geom_line(aes(col = factor(season)), size = 1.5) +
  scale_colour_brewer(palette = "Set1") +
  geom_point(aes(fill = roi),size = 3,col = "black",shape = 21) +
  scale_fill_manual(values = c("Yes" = "black","No" = "#8A8A8A"),guide = "none") +
  scale_x_continuous(breaks = 1:14) +
  scale_y_continuous(labels = ~scales::percent(.x,accuracy = 1), limits = c(0.75,1)) +
  # annotate("segment", x = 2.5, xend = 4, y = 0.779, yend = 0.779,
  #          arrow = arrow(ends = "first", angle = 45, type = "closed", length = unit(.2,"cm"))) +
  # annotate("text",x = 4.3, y = 0.779,label = "R2: Fever v Magpies",hjust = 0) +
  theme_bw() +
  labs(title = glue("{tName} shooting accuracy"),
    subtitle = "Effective shooting percentage in the 2020, 2021 and 2022 seasons",
    y = "Effective shooting percentage",
    x = "Round",
    colour = "Season",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))

goals %>% 
  filter(season == ssn,squadNickname %in% team_names,round == rnd) %>%
  mutate(minute = floor(periodSeconds/60),
         SS = if_else(str_detect(scoreName,"2"),"Super","Normal")) %>% 
  count(squadNickname,round,period,minute,SS,wt = scorepoints) %>% 
  ggplot(aes(x = minute, y = n,fill = fct_rev(SS))) +
  geom_col() +
  scale_fill_manual(values = c("Normal" = "#1874CD", "Super" = "#FF3030")) +
  labs(title = "Scoring consistency",
    subtitle = "Goals scored in each minute of each quarter of the game",
    fill = "Score type",
    y = "Goals",
    x = "Minute",
    caption = "Data: Champion Data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black")) + 
  facet_grid(squadNickname ~ paste("Qtr -",period))


goals %>% 
  filter(season == ssn,squadNickname %in% team_names,round == rnd) %>%
  mutate(periodSeconds = if_else(periodSeconds == 900, 899L,periodSeconds),
    minute = floor(periodSeconds/60),
         shottype = case_when(
           scoreName == "2pt Goal" ~ "Super made",
           scoreName == "2pt Miss" ~ "Super missed",
           scoreName == "goal" ~ "Normal made",
           scoreName == "miss" ~ "Normal missed",
         ),
         shottype = factor(shottype,levels = c("Normal missed", "Super missed","Super made","Normal made"),ordered = T,
)) %>% 
  count(squadNickname,round,period,minute,shottype) %>% 
  mutate(n = if_else(shottype %in% c("Super missed","Super made"),2L * n,n)) %>%
  ggplot(aes(x = minute, y = n,fill = shottype)) +
  geom_col(aes(col = shottype)) +
  scale_fill_manual(values = c("Normal made" = "#1874CD","Super made" = "#FF3030", "Normal missed" = "white", "Super missed" = "white")) +
  scale_colour_manual(values = c("Normal made" = "#1874CD","Super made" = "#FF3030", "Normal missed" = "black", "Super missed" = "black"),guide = "none") +
  labs(title = "Scoring consistency",
       subtitle = "Goals scored in each minute of each quarter of the game",
       fill = "Score type",
       y = "Goals",
       x = "Minute",
       caption = "Data: Champion Data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"),
        strip.text = element_text(size = 10)) +
  guides(fill = guide_legend(override.aes = list(col = c("#1874CD", "#FF3030","black","black")))) +
  facet_grid(squadNickname ~ paste("Qtr -",period))

# Possession outcomes -----------------------------------------------------

plot_dat <- 
  o_rtg %>% 
  ungroup() %>% 
  filter(season == ssn,squadNickname %in% team_names,round == rnd) %>%
  mutate(effective_misses = goalMisses - offensiveRebounds) %>% 
  select(-c(season,goals,turnovers,feeds,goalMisses,offensiveRebounds,goalAttempts)) %>% 
  pivot_longer(-c(round,match,squadNickname,possessions)) %>% 
  mutate(name = case_when(
    name == "effective_misses" ~ "Effective misses",
    name == "generalPlayTurnovers" ~ "Turnovers",
    name == "goal1" ~ "Normals",
    name == "goal2" ~ "Supers"),
    name = factor(name,levels = c("Turnovers", "Effective misses", "Supers", "Normals"),ordered = T))



plot_dat %>% 
  ggplot(aes(value,squadNickname)) +
  geom_col(aes(fill = name), col = "black") +
  geom_text(data = plot_dat %>% count(squadNickname,wt = value),aes(x = n,label = n),hjust = 0,size = 6,nudge_x = 1) +
  geom_text(aes(label = value,group = name),position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(0,100,10)) +
  theme_bw() +
  labs(title = paste("Round",rnd,"possession outcomes"),
    subtitle = "The outcome of each team's possessions",
    y = "",
    x = "Possession outcome count",
    fill = "Stat type",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom") +
  expand_limits(x = plot_dat %>% count(squadNickname,wt = value) %$% (max(n) + 3))

# Offensive outcomes

plot_dat %>% 
  ggplot(aes(value,squadNickname)) +
  geom_col(aes(fill = name), col = "black") +
  geom_text(data = plot_dat %>% add_count(squadNickname,wt = value),aes(x = n,label = possessions),hjust = 0,size = 6,nudge_x = 1) +
  geom_text(aes(label = value,group = name),position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  #scale_x_continuous(breaks = seq(0,100,10)) +
  theme_bw() +
  labs(title = paste("Round",rnd,"goal outcomes"),
       y = "",
       x = "Goal outcome count",
       fill = "Stat type",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  expand_limits(x = plot_dat %>% count(squadNickname,wt = value) %$% (max(n) + 3))

player_stats %>% 
  group_by(season,squadNickname,round) %>% 
  summarise(goals = sum(goals),
            n = n()) %>% 
  mutate(goals = if_else(n>40,goals/2,as.numeric(goals))) %>% 
  arrange(-goals)

o_rtg %>% 
  group_by(season,round,match) %>% 
  filter(all(squadNickname %in% c("Fever","Magpies"))) %>% 
  group_by(season,round,match,squadNickname) %>% 
  summarise(
    ppg = (sum(possessions)/n()) %>% round(1),
    oRtg = (sum(goals)/sum(possessions)*100) %>% round(1)) %>% 
  arrange(-season)

o_rtg %>% 
  filter(season == 2022) %>%
  mutate(oRtg = goals/possessions*100) %>% 
  ggplot(aes(round,generalPlayTurnovers,col = squadNickname,group = squadNickname)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = SquadName_Colours) + 
  expand_limits(y = 0)

player_stats %>% 
  filter(season == 2022,round == 5) %>% 
  group_by(playerId) %>% 
  mutate(shooter = any(attempts1 > 0),
         rebounds = if_else(shooter,rebounds,0L)) %>% 
  group_by(squadNickname,match) %>% 
  summarise(across(c(goals,goalAttempts,rebounds,generalPlayTurnovers),sum)) %>% 
  arrange(match)

o_rtg %>% 
  filter(season == 2022) %>% 
  group_by(round,match) %>% 
  summarise(pDiff = max(possessions) - min(possessions),
            squadNickname[possessions == max(possessions)]) %>% 
  arrange(-pDiff)


o_rtg %>% 
  ungroup() %>% 
  transmute(season,round,match,squadNickname,
            off_rtg = (goals/possessions*100) %>% round(1)) %>% 
  arrange(squadNickname,round) %>% 
  full_join(.,.,by = c("season", "round","match"),suffix = c("",".join")) %>% 
  filter(season == 2022,squadNickname != squadNickname.join) %>% 
  rename(def_rtg = off_rtg.join, opponent = squadNickname.join) %>% 
  group_by(squadNickname) %>% 
  summarise(across(c(off_rtg,def_rtg),mean)) %>% 
  mutate(off_rtg = dense_rank(desc(off_rtg)),
         def_rtg = dense_rank(def_rtg)) %>% 
  arrange(off_rtg)


o_rtg %>% 
  ungroup() %>% 
  transmute(season,round,match,squadNickname,
            off_rtg = (goals/possessions*100) %>% round(1)) %>% 
  arrange(squadNickname,round) %>% 
  full_join(.,.,by = c("season", "round","match"),suffix = c("",".join")) %>% 
  filter(season == 2022,squadNickname != squadNickname.join) %>% 
  rename(def_rtg = off_rtg.join, opponent = squadNickname.join) %>% 
  mutate(net_rtg = off_rtg - def_rtg) %>% 
  group_by(squadNickname) %>% 
  summarise(across(c(off_rtg,def_rtg,net_rtg),mean)) %>% 
  arrange(-net_rtg)


player_stats %>% 
  ungroup() %>% 
  filter(season == 2022) %>% 
  count(playerId, wt = generalPlayTurnovers, sort = T) %>% 
  left_join(player_info %>% distinct(playerId,.keep_all = T))

team_stats %>% 
  filter(season == 2022) %>% 
  group_by(squadId) %>% 
  summarise(across(c(goals,goalAttempts,generalPlayTurnovers),sum)) %>% 
  mutate(acc = goals/goalAttempts) %>% 
  left_join(team_info) %>% 
  arrange(acc)
