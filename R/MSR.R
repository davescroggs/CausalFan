library(tidyverse)
library(geomtextpath)
library(gt)
library(gtExtras)


source("R/load_netball_data.R")
load_netball_data(2017:2022)

SquadName_Colours <- c("#FDE725FF", "#73D055FF", "#27AD81FF", 
                       "#7E4E90FF", "#CC6A70FF", "#2D708EFF", "#000000", "#F68F46FF")
names(SquadName_Colours) <- c("Lightning", "Fever", "Vixens", 
                              "Firebirds", "Thunderbirds", "Swifts", "Magpies", "GIANTS")  

o_rtg <- 
  team_stats %>% 
  left_join(team_info) %>% 
  group_by(season,round,match,squadNickname) %>% 
  summarise(across(c(goals,goal1,attempt_from_zone1,attempt_from_zone2,goal2, goalAttempts, goalMisses, goalsFromGain, generalPlayTurnovers,turnovers,feeds),~sum(.,na.rm = T))) %>%
  left_join(player_stats %>% 
              group_by(playerId) %>% 
              filter(any(goals > 0)) %>% 
              ungroup() %>% 
              count(season,round, match, squadNickname,wt = rebounds,name = "offensiveRebounds"),
            by = c("round", "match", "squadNickname","season")) %>% 
  mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers,
         off_rtg = (goals/possessions*100)) %>% 
  ungroup()


# Ladder -------------------------------------------------------------------------

win_loss <- team_stats %>% 
  filter(season == 2022) %>% 
  group_by(season,round,match,squadId) %>% 
  summarise(goals = sum(goals),.groups = "drop") %>% 
  full_join(.,.,by = c("season","round","match"),
            suffix = c("",".join")) %>% 
  filter(squadId != squadId.join) %>%
  mutate(wl = case_when(goals == goals.join ~ 0.5,
                        goals < goals.join ~ 0,
                        goals > goals.join ~ 1,
                        TRUE ~ NA_real_)) %>% 
  left_join(team_info)

o_rtg %>% 
  ungroup() %>% 
  transmute(season,round,match,squadNickname,
            off_rtg = (goals/possessions*100) %>% round(1),
            goals) %>% 
  arrange(squadNickname,round) %>% 
  full_join(.,.,by = c("season", "round","match"),suffix = c("",".join")) %>% 
  filter(season == 2022,squadNickname != squadNickname.join) %>% 
  rename(def_rtg = off_rtg.join, opponent = squadNickname.join) %>% 
  left_join(win_loss %>% select(-goals,-goals.join), by = c("squadNickname", "season", "round")) %>% 
  transmute(Team = squadNickname,
            Round = round,
            off_rtg,
            def_rtg,
            net_rtg = off_rtg - def_rtg,
            wl,
            Opponent = opponent,
            goals,goals.join) %>% 
  group_by(Team) %>% 
  summarise(Wins = sum(wl),
            Losses = 7 - Wins,
            `Off rating` = list(off_rtg),
            `Def rating` = list(def_rtg),
            `Net rating` = list(net_rtg),
            Outcomes = list(wl),
            `Goal Percentage` = (sum(goals)/sum(goals.join)) %>% scales::percent()) %>% 
  arrange(-Wins,desc(`Goal Percentage`)) %>% 
  select(-contains("rating")) %>% 
  gt::gt() %>% 
  # gtExtras::gt_sparkline(`Off rating`) %>% 
  # gtExtras::gt_sparkline(`Def rating`) %>% 
  # gtExtras::gt_sparkline(`Net rating`) %>% 
  gtExtras::gt_plt_winloss(Outcomes,max_wins = 7)


# Offensive/Defensive Ratings ---------------------------------------------


o_rtg %>% 
  ungroup() %>% 
  transmute(season,round,match,squadNickname,
            off_rtg = round(off_rtg,1)) %>% 
  arrange(squadNickname,round) %>% 
  full_join(.,.,by = c("season", "round","match"),suffix = c("",".join")) %>% 
  rename(def_rtg = off_rtg.join, opponent = squadNickname.join) %>% 
  filter(squadNickname != opponent) %>% 
  group_by(season,squadNickname) %>% 
  summarise(across(c(off_rtg,def_rtg),mean)) %>% 
  ggplot(aes(def_rtg,off_rtg)) +
  geom_abline() +
  annotate("textsegment",x = 70, xend = 75, y = 70, yend = 70, label = "Net negative",
           arrow = arrow(type = "closed",length = unit(5,"pt")),
           linetype = "dashed",col = "grey40") +
  annotate("textsegment",x = 70, xend = 70, y = 70, yend = 75, label = "Net positive",
           arrow = arrow(type = "closed",length = unit(5,"pt")),
           linetype = "dashed",col = "grey40") +
  geom_point(data = ~filter(.,season < 2022),aes(col = factor(season))) +
  geom_point(data = ~filter(.,season == 2022), aes(fill = squadNickname),
            size = 4,col = "black",shape = 21,show.legend = FALSE) +
  geom_point(data = tibble(x = 72.7,y = 72.7),aes(x,y),
             shape = 4,size = 4,stroke = 3, col = "red",
             inherit.aes = F) +
  ggrepel::geom_text_repel(data = ~filter(.,season == 2022), aes(label = squadNickname),
                           min.segment.length = 0) +
  scale_fill_manual(values = SquadName_Colours) +
  scale_color_brewer(palette = "Spectral") +
  coord_equal(xlim = c(58,81),ylim = c(58,81)) +
  #theme_bw() +
  labs(title = "Mid-season Team perfomance",
    subtitle = "Offensive and defensive ratings after round 7 of 2022 SSN season",
    x = "Defensive rating (goals against per 100 possessions)",
    y = "Offensive rating (goals for per 100 possessions)",
    col = "Season",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))



# -------------------------------------------------------------------------

win_loss %>% 
  group_by(squadNickname) %>% 
summarise(across(c(goals,goals.join,wl),sum)) %>% 
  mutate(pct = goals/goals.join) %>% 
  arrange(-wl,-pct) %>% 
  mutate(pos = 1:n())

stat_rename <- 
tibble::tribble(
            ~Axis_label,                  ~Stat,
                 "Team",        "squadNickname",
                "Goals",                "goals",
   "Effective accuracy",              "eff_acc",
         "Normal goals",                "goal1",
      "Normal attempts",   "attempt_from_zone1",
      "Normal accuracy",            "goal1_acc",
          "Super goals",                "goal2",
       "Super attempts",   "attempt_from_zone2",
       "Super accuracy",            "goal2_acc",
     "Goals from gains",        "goalsFromGain",
          "Possessions",          "possessions",
                "Feeds",                "feeds",
         "Off Rebounds",    "offensiveRebounds",
            "Turnovers", "generalPlayTurnovers"
   ) %>% deframe()


o_rtg %>% 
  filter(season == 2022) %>% 
  group_by(squadNickname) %>% 
  summarise(across(c(goals:possessions,-turnovers),mean),.groups = "drop") %>% 
  mutate(goal1_acc = goal1/attempt_from_zone1,
         goal2_acc = goal2/attempt_from_zone2,
         eff_acc = goals/goalAttempts) %>% 
  select(squadNickname,goals,eff_acc,goal1,attempt_from_zone1,goal1_acc,goal2,attempt_from_zone2,goal2_acc,goalsFromGain,possessions,feeds,offensiveRebounds,generalPlayTurnovers) %>% 
  rename(stat_rename) %>% 
    mutate(across(-Team,list(ave_diff = ~(.x - mean(.x))/mean(.x)),.names = "{.col}.{.fn}"),
    #across(2:13,round,digits = 1),
    across(c(3,6,9),~scales::percent(.x,0.1)),
    across(c(2:14,-3,-6,-9),~round(.x,digits = 1) %>% as.character())) %>% 
  rename_at(2:14,~paste0(.x,".sqdmean")) %>% 
  pivot_longer(-Team,names_to = c("Stat",".value"),names_sep = "\\.") %>% 
  inner_join(win_loss %>% 
               rename(Team = squadNickname) %>% 
               group_by(Team) %>% 
               summarise(across(c(goals,goals.join,wl),sum)) %>% 
               mutate(pct = goals/goals.join) %>% 
               arrange(-wl,-pct) %>% 
               mutate(pos = 1:n())) %>% 
  ggplot(aes(x = fct_inorder(Stat),y = fct_reorder(Team,-pos))) +
  geom_tile(aes(fill = ave_diff)) +
  geom_text(aes(label = sqdmean)) +
  scale_fill_distiller(palette = "RdYlGn",labels = scales::percent,limit = c(-0.5,0.5),direction = 1) +
  theme_bw() +
  labs(title = "Key statistics - For team",
    subtitle = "Relevant team statistics accumulated FOR listed team\nRounds 1-7 season 2022",
    y = "",
    x = "Stat",
    fill = "% difference\nfrom mean",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"),
    axis.text.x = element_text(angle = 45,hjust = 1))



# Defensive outcomes ------------------------------------------------------

stat_rename_opp <- 
  tibble::tribble(
    ~Axis_label,                  ~Stat,
    "Team",        "opponent",
    "Goals",                "goals",
    "Effective accuracy",              "eff_acc",
    "Normal goals",                "goal1",
    "Normal attempts",   "attempt_from_zone1",
    "Normal accuracy",            "goal1_acc",
    "Super goals",                "goal2",
    "Super attempts",   "attempt_from_zone2",
    "Super accuracy",            "goal2_acc",
    "Goals from gains",        "goalsFromGain",
    "Possessions",          "possessions",
    "Feeds",                "feeds",
    "Off Rebounds",    "offensiveRebounds",
    "Turnovers", "generalPlayTurnovers"
  ) %>% deframe()


o_rtg %>% 
  filter(season == 2022) %>% 
  full_join(o_rtg %>% 
              filter(season == 2022) %>% 
              select(season,round,match,opponent = squadNickname),
            by = c("season", "round", "match")) %>% 
  filter(squadNickname != opponent) %>% 
  group_by(opponent) %>% 
  summarise(across(c(goals:possessions,-turnovers),mean),.groups = "drop") %>% 
  mutate(goal1_acc = goal1/attempt_from_zone1,
         goal2_acc = goal2/attempt_from_zone2,
         eff_acc = goals/goalAttempts) %>% 
  select(opponent,goals,eff_acc,goal1,attempt_from_zone1,goal1_acc,goal2,attempt_from_zone2,goal2_acc,goalsFromGain,possessions,feeds,offensiveRebounds,generalPlayTurnovers) %>% 
  rename(stat_rename_opp) %>% 
  mutate(across(-Team,list(ave_diff = ~(.x - mean(.x))/mean(.x)),.names = "{.col}.{.fn}"),
         #across(2:13,round,digits = 1),
         across(c(3,6,9),~scales::percent(.x,0.1)),
         across(c(2:14,-3,-6,-9),~round(.x,digits = 1) %>% as.character())) %>% 
  rename_at(2:14,~paste0(.x,".sqdmean")) %>% 
  pivot_longer(-Team,names_to = c("Stat",".value"),names_sep = "\\.") %>% 
  inner_join(win_loss %>% 
               rename(Team = squadNickname) %>% 
               group_by(Team) %>% 
               summarise(across(c(goals,goals.join,wl),sum)) %>% 
               mutate(pct = goals/goals.join) %>% 
               arrange(-wl,-pct) %>% 
               mutate(pos = 1:n())) %>% 
  ggplot(aes(x = fct_inorder(Stat),y = fct_reorder(Team,-pos))) +
  geom_tile(aes(fill = ave_diff)) +
  geom_text(aes(label = sqdmean)) +
  scale_fill_distiller(palette = "RdYlGn",labels = scales::percent,limit = c(-0.5,0.5),direction = 1) +
  theme_bw() +
  labs(title = "Key statistics - Against team",
       subtitle = "Relevant team statistics accumulated AGAINST listed team\nRounds 1-7 season 2022",
       y = "",
       x = "Stat",
       fill = "% difference\nfrom mean",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"),
        axis.text.x = element_text(angle = 45,hjust = 1))


# Possession differential -------------------------------------------------

poss_differential <- o_rtg %>% 
  filter(season == 2022) %>% 
  select(season,round,match,squadNickname, possessions) %>% 
  full_join(.,.,by = c("season","round","match"),
            suffix = c("",".against")) %>% 
  filter(squadNickname != squadNickname.against) %>% 
  count(squadNickname, wt = (possessions - possessions.against),name = "poss_differential")

p1 <- poss_differential %>% 
  ggplot(aes(y = poss_differential,x = fct_reorder(squadNickname,poss_differential))) +
  geom_col(aes(fill = squadNickname),col = "black") +
  geom_hline(yintercept = 0) +
  geom_label(aes(label = poss_differential)) +
  scale_fill_manual(values = SquadName_Colours) +
  theme_minimal() +
  labs(subtitle = "Possession differential - for and against team",
    y = "Possession differential",
    x = "") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

p2 <- team_stats %>% 
  filter(season == 2022) %>% 
  left_join(team_info) %>% 
  select(season,round,match,squadNickname, goals) %>% 
  full_join(.,.,by = c("season","round","match"),
            suffix = c("",".against")) %>% 
  filter(squadNickname != squadNickname.against) %>% 
  count(squadNickname, wt = (goals - goals.against),name = "goal_differential") %>% 
ggplot(aes(y = goal_differential,x = fct_reorder(squadNickname,goal_differential))) +
  geom_col(aes(fill = squadNickname),col = "black") +
  geom_hline(yintercept = 0) +
  geom_label(aes(label = goal_differential)) +
  scale_fill_manual(values = SquadName_Colours) +
  theme_minimal() +
  labs(subtitle = "Goal differential - for and against team",
       y = "Goal differential",
       x = "",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
library(patchwork)
p1 / p2 + plot_layout(guides = "collect") + 
  plot_annotation(title = 'Season 2022 goal and possession differential')

# -------------------------------------------------------------------------
most_common_position <- 
  player_stats %>% 
  group_by(surname) %>% 
  filter(startingPositionCode != "I") %>% 
  count(surname,startingPositionCode,sort = T) %>% 
  slice_head() %>% 
  select(-n)


# Stat normalisation ------------------------------------------------------


plot_data_pre <- player_stats %>%
  filter(season == 2022) %>% 
  select(-startingPositionCode) %>% 
  left_join(most_common_position) %>% 
  group_by(surname,startingPositionCode,squadNickname) %>%
  filter(startingPositionCode %in% c("WA","C","WD"),sum(minutesPlayed) > 150) %>% 
  summarise(across(c(feeds,feedWithAttempt,goalAssists,pickups,gain,generalPlayTurnovers,minutesPlayed),sum)) %>% 
  mutate(feed_per_gpt = feeds/generalPlayTurnovers) %>% 
  arrange(startingPositionCode,-gain) %>% 
  select(-minutesPlayed) %>% 
  rename("Feeds" = "feeds",
         "Feeds w attempt" = "feedWithAttempt",
         "Goal assists" = "goalAssists",
         "Pickups" = "pickups",
         "Gain" = "gain",
         "Turnovers" = "generalPlayTurnovers",
         "Feed per turnover" = "feed_per_gpt")

plot_data_post <- player_stats %>%
  select(-startingPositionCode) %>% 
  left_join(most_common_position) %>% 
  group_by(surname,startingPositionCode,squadNickname) %>%
  filter(startingPositionCode %in% c("WA","C","WD"),sum(minutesPlayed) > 150) %>% 
  summarise(across(c(feeds,feedWithAttempt,goalAssists,pickups,gain,generalPlayTurnovers),~sum(.x)/sum(minutesPlayed)*60)) %>% 
  left_join(o_rtg %>%
              select(squadNickname,poss_wt)) %>%
  group_by(surname,startingPositionCode,squadNickname) %>%
  mutate(across(c(feeds,feedWithAttempt,goalAssists,pickups,gain,generalPlayTurnovers),~.x/poss_wt)) %>%
  mutate(feed_per_gpt = feeds/generalPlayTurnovers) %>% 
  arrange(startingPositionCode,-gain) %>% 
  select(-poss_wt) %>% 
  rename("Feeds" = "feeds",
         "Feeds w attempt" = "feedWithAttempt",
         "Goal assists" = "goalAssists",
         "Pickups" = "pickups",
         "Gain" = "gain",
         "Turnovers" = "generalPlayTurnovers",
         "Feed per turnover" = "feed_per_gpt")


# COVID in/outs -----------------------------------------------------------

minutes_last_14 <- player_stats %>% 
  group_by(playerId) %>% 
  slice_tail(n = 14*4) %>% 
  count(playerId, wt = minutesPlayed/14) %>% 
  left_join(player_info %>% distinct(playerId,displayName)) %>% 
  filter(playerId %in% (player_stats %>%
           filter(season == 2022) %>%
             distinct(playerId) %>%
             pull(playerId)))

full_squad <- 
 tibble::tribble(
    ~squadNickname, ~full_squad_round,
           "Fever",                1L,
       "Firebirds",                1L,
          "GIANTS",                3L,
       "Lightning",                1L,
         "Magpies",                2L,
          "Swifts",                4L,
    "Thunderbirds",                1L,
          "Vixens",                1L
    ) %>% 
  right_join(player_stats %>%
               filter(season == 2022) %>% 
               group_by(squadNickname,round) %>%
               summarise(primary_squad = list(unique(displayName)))) %>% 
  filter(round == full_squad_round) %>% 
  select(-c(full_squad_round,round))
 

missing_minutes <- 
  player_stats %>%
  filter(season == 2022) %>% 
group_by(squadNickname,round) %>%
summarise(squad = list(unique(displayName))) %>%
  left_join(full_squad) %>% 
mutate(outs = map2(squad,primary_squad, ~setdiff(.y,.x)),
       #outs = if_else(squadNickname == "Swifts" & round == 1,list(character(0)),outs),
       n = map_int(outs,length)) %>% 
  #unnest(outs,keep_empty = T) %>% 
  # left_join(minutes_last_14,by = c("outs" = "displayName")) %>% 
  # count(squadNickname,round, wt = n) %>% 
#   group_by(squadNickname) %>%
# complete(round = 1:7) %>%
#   replace_na(list(n = 0)) %>%
#  ungroup() %>%
    left_join(schedule %>% 
                filter(season == 2022) %>% 
                pivot_longer(cols = c(home,away),values_to = "squadId") %>% 
                left_join(team_info) %>% 
                select(-squadId,-season))
  
  missing_minutes %>% 
    select(-c(squad:outs)) %>% 
    left_join(win_loss %>% select(round,match,squadNickname,wl)) %>% 
    pivot_wider(names_from = name,values_from = c(n,squadNickname,wl)) %>%
    mutate(Winner = if_else(wl_home == 1,squadNickname_home,squadNickname_away),
           less_changes = case_when(
             n_home == n_away ~ "-",
             n_home < n_away ~ squadNickname_home,
             n_home > n_away ~ squadNickname_away),
           Match = paste("Round",round,squadNickname_home,"v",squadNickname_away)) %>% 
    filter(n_home > 0 | n_away > 0) %>% 
    arrange(round) %>% 
    transmute(Match, `Home outs` = n_home,`Away outs` = n_away, Winner, `Fewer outs` = less_changes,
              `Out affected` = if_else(Winner == `Fewer outs`,1,0)) %>% 
    janitor::adorn_totals(fill = "") %>% 
    gt() %>% 
    gtExtras::gt_color_rows(columns = 2:3,
                            domain = c(0,4),
                            palette = "RColorBrewer::Blues",
                            na.color = "white") %>% 
    gtExtras::gt_color_rows(columns = 6,
                            type = "discrete",
                            palette = "RColorBrewer::Greys", 
                            domain = c(0,1),
                            na.color = "white")
    
  missing_minutes %>%
    ungroup() %>% 
    select(-c(squad,primary_squad)) %>% 
    mutate(outs = map_chr(outs,~paste0(.x,collapse =  ", "))) %>% 
    left_join(win_loss %>% select(round,match,squadNickname,wl)) %>% 
    pivot_wider(names_from = name,values_from = c(n,outs,squadNickname,wl)) %>% 
    mutate(Winner = if_else(wl_home == 1,squadNickname_home,squadNickname_away),
           less_changes = case_when(
             n_home == n_away ~ "-",
             n_home < n_away ~ squadNickname_home,
             n_home > n_away ~ squadNickname_away),
           Match = paste("Round",round,squadNickname_home,"v",squadNickname_away)) %>% 
    filter(n_home > 0 | n_away > 0) %>% 
    arrange(round) %>% 
    transmute(`Match (home v away)` = Match, `Home outs` = outs_home,`Away outs` = outs_away, Winner, `Fewer outs` = less_changes,
              `Out affected` = if_else(Winner == `Fewer outs`,1,0)) %>% 
    janitor::adorn_totals(fill = "") %>% 
    gt() %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels(everything())
    ) %>% 
    gt_add_divider(columns = everything(),sides = "all") %>% 
    gtExtras::gt_color_rows(columns = 6,
                            type = "discrete",
                            palette = "RColorBrewer::Greys", 
                            domain = c(0,1),
                            na.color = "white") %>% 
    tab_options(heading.title.font.size = 25,
                heading.align = "left",
                table.border.top.color = "black",
                table.border.bottom.color = "black",
                column_labels.border.bottom.color = "black",
                column_labels.border.bottom.width= px(3)) %>% 
    tab_header(title = md("**Player outs during rounds 1-7 in season 2022**"))
  
  
  missing_minutes %>%
    ungroup() %>% 
    select(-c(squad,primary_squad)) %>% 
    mutate(outs = map_chr(outs,~paste0(.x,collapse =  ", "))) %>% 
    left_join(win_loss %>% select(round,match,squadNickname,wl)) %>%
    group_by(Team = squadNickname) %>% 
    summarise(`Games affected` = sum(n > 0),
              `Total player outs` = sum(n)) %>% 
    arrange(-`Games affected`) %>% 
    gt() %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels(everything())
    ) %>% 
    gt_add_divider(columns = everything(),sides = "all") %>% 
    tab_options(heading.title.font.size = 25,
                heading.align = "left",
                table.border.top.color = "black",
                table.border.bottom.color = "black",
                column_labels.border.bottom.color = "black",
                column_labels.border.bottom.width= px(3)) %>% 
    tab_header(title = md("**Player out totals**"))
  
  o_rtg %>% 
    ungroup() %>% 
    transmute(season,round,match,squadNickname,
              off_rtg = round(off_rtg,1)) %>% 
    arrange(squadNickname,round) %>% 
    full_join(.,.,by = c("season", "round","match"),suffix = c("",".join")) %>% 
    rename(def_rtg = off_rtg.join, opponent = squadNickname.join) %>% 
    filter(squadNickname != opponent) %>% 
    group_by(season,squadNickname) %>% 
    summarise(across(c(off_rtg,def_rtg),mean)) %>% 
    filter(season == 2022) %>% 
    mutate(across(c(off_rtg),mean,.names = "{.col}_mean"),
           net = off_rtg - def_rtg)
  