# The possession game

library(tidyverse)
library(superNetballR)

# Create data -------------------------------------------------------------

squadCols <- tribble(
  ~squadNickname,	~Primary,	~Secondary,
  "GIANTS"	      , "#f47920",	"#0082ca",
  "Thunderbirds"	, "#ef5091",	"#ffffff",
  "Vixens"	      , "#00a88f",	"#e50e63",
  "Magpies"	  , "#000000",	"#ffffff",
  "Swifts"      , "#ee3124",	"#0093d0",
  "Firebirds"	    , "#381460",	"#fdb515",
  "Lightning"	    , "#782b8f",	"#fdb71d",
  "Fever"	        , "#00953b",	"#000000",
  "No Team",       "#999999", "#595959")

squadPrimaryCols <- squadCols %>%   
  select(-Secondary) %>% 
  deframe()

dat <- map(paste("data/",2009:2021,"_data.RDS",sep = ""),read_rds) %>% 
  flatten()

safe_date <- possibly(.f = ~as.Date(.x) %>% lubridate::year(),otherwise = NA_real_)

extracted_metrics <- 
  tibble(scores = dat %>%
           map(~pluck(.x,"scoreFlow",1)),
         team_stats = dat %>%
           map(~pluck(.x, "teamStats","team")),
         player_stats = dat %>%
           map(~pluck(.x, "playerPeriodStats","player")),
         teamPeriodStats = dat %>%
           map(~pluck(.x, "teamPeriodStats","team")),
         team_info = dat %>%
           map(~pluck(.x, "teamInfo","team")),
         player_info = dat %>%
           map(~pluck(.x, "playerInfo","player")),
         round = dat %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
         match = dat %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
         season = dat %>% map_chr(~pluck(.x,"matchInfo", "localStartTime",.default = NA_character_)) %>%
           safe_date()) %>% 
  filter(!is.na(season),!map_lgl(scores,is.null))

unnest_specific <- function(df,tbl){
  df %>% 
    select(season, round, match, {{tbl}}) %>%
    unnest_longer({{tbl}}) %>%
    unnest_wider({{tbl}})
}

team_info <- extracted_metrics %>% 
  unnest_specific(team_info) %>% 
  group_by(squadId) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(squadId,squadNickname,squadCode) %>% 
  mutate(country = if_else(squadId %in% c(801L,804L,806L,807L,810L,8117L,8118L,8119L),"Aus","NZ"))

player_info <- extracted_metrics %>% 
  unnest_specific(player_info) %>% 
  group_by(playerId) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(playerId,surname,displayName)

shooting_stats <- extracted_metrics %>% 
  unnest_specific(scores)

team_stats <- extracted_metrics %>% 
  unnest_specific(team_stats)

player_stats <- extracted_metrics %>% 
  unnest_specific(player_stats)

team_period_stats <- extracted_metrics %>%
  unnest_specific(teamPeriodStats) %>% 
  inner_join(team_info %>%
               select(squadId,squadNickname))

o_poss <- player_stats %>%
  filter(period < 5) %>%
  select(season,round,match,squadId,goals,goal1,goal2, goalAttempts, goalMisses, generalPlayTurnovers,turnovers) %>%
  group_by(season,round,match,squadId) %>% 
  summarise(across(c(goals,goal1,goal2, goalAttempts, goalMisses, generalPlayTurnovers,turnovers),~sum(.,na.rm = T))) %>% 
  left_join(player_stats %>% 
              group_by(playerId) %>% 
              filter(any(goals > 0)) %>% 
              ungroup() %>% 
              count(season,round, match, squadId,wt = rebounds,name = "offensiveRebounds"),
            by = c("round", "match", "squadId","season")) %>% 
  mutate(goal1 = if_else(goal1 == 0,goals,goal1),
         generalPlayTurnovers = if_else(generalPlayTurnovers == 0,turnovers,generalPlayTurnovers)) %>% 
  replace_na(list(goal1 = 0,goal2 = 0)) %>% 
  mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers,
         goals = goal1 + goal2*2) %>% 
  left_join(team_info)

o_poss_by_season <- o_poss  %>% 
  transmute(round,match,opponent = squadNickname,dPoss = possessions,dGoals = goals) %>% 
  full_join(o_poss) %>% 
  filter(opponent != squadNickname) %>% 
  group_by(season,squadNickname) %>% 
  summarise(oPoss = sum(possessions)/n(),
            dPoss = sum(dPoss)/n(),
            tPoss = oPoss + dPoss,
            games = n(),
            Net_poss = oPoss - dPoss,
            .groups = "drop_last") %>% 
  mutate(mPoss = mean(oPoss))


o_poss_by_season %>% 
  left_join(o_rtg_by_season, by = c("squadNickname", "season")) %>% 
  arrange(-oPoss)



o_poss_by_season %>% 
  ggplot(aes(x = oPoss,y = season)) +
  geom_point(aes(col = "Team pace")) +
  geom_point(aes(x = mPoss, col = "Season ave pace")) +
  scale_y_continuous(breaks = 2009:2021) +
  scale_colour_manual(values = c("Team pace" = "black", "Season ave pace" = "red")) +
  scale_x_continuous(breaks = seq(60,100,5)) +
  ggrepel::geom_text_repel(data = o_poss_by_season %>% 
                             group_by(season) %>% 
                             arrange(oPoss) %>% 
                             slice(1),
                           aes(label = squadNickname,family = "Courier"),
                           nudge_x = -3) +
  ggrepel::geom_text_repel(data = o_poss_by_season %>% 
                             group_by(season) %>% 
                             arrange(-oPoss) %>% 
                             slice(1),
                           aes(label = squadNickname,family = "Courier"),
                           nudge_x = 3) +
  geomtextpath::geom_texthline(yintercept = 2016.5,
                               label = " Suncorp Supernetball competition start ",hjust = 0.5,
                               linetype = "dashed",
                               family = "Courier") +
  geomtextpath::geom_texthline(yintercept = 2019.5,
                               label = " Substitutions rule change ",hjust = 0.5,
                               linetype = 3,
                               family = "Courier") +
    theme_bw() +
  labs(title = "Game pace 2009 - 2021",
    subtitle = "Season average possessions per game.\nRed dots indicate competition season average.",
    caption = "Data: Champion Data",
    y = "Season",
    x = "Team possessions",
    colour = "Legend") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black")) +
  expand_limits(x = c(60,100))

# Tactix in 2013 have the 4th most possessions in time period, but finished 2nd last on the ladder. High possessions are not necessarily a good thing. They can mean a team gets scored on quickly.


player_stats %>%
  filter(period < 5,goalAttempts > 0) %>% 
  group_by(season) %>% 
  summarise(across(c(goalAttempts, generalPlayTurnovers,turnovers),~sum(.,na.rm = T)),
            n = n()/4) %>% 
  left_join(player_stats %>% 
              group_by(playerId) %>% 
              filter(any(goals > 0)) %>% 
              ungroup() %>% 
              count(season,wt = rebounds,name = "offensiveRebounds"),
            by = c("season")) %>% 
  mutate(generalPlayTurnovers = if_else(generalPlayTurnovers == 0,turnovers,generalPlayTurnovers),
         season = factor(season),
         `Effective attempts` = goalAttempts - offensiveRebounds,
         Turnovers = generalPlayTurnovers) %>% 
  select(season,`Effective attempts`,Turnovers,n) %>% 
  pivot_longer(cols = -c(season,n),values_to = "count",names_to = "metric") %>% 
  group_by(season) %>% 
    mutate(count = count/n,
      pct = scales::percent(count/sum(count)),
      count = round(count,digits = 1),
      label = paste(count," (",pct,")",sep = "")) %>%
  ungroup() %>% 
  ggplot(aes(x = count, y = season, fill = metric)) +
  geom_col(colour="black", width=1) +
  scale_x_continuous(breaks = seq(0,100,10),
                     minor_breaks = seq(0,100,5)) +
  scale_fill_brewer(palette = 9) +
  geom_text(aes(label = label),
            position = position_stack(vjust = .5)) +
  theme_bw() +
  labs(title = "Average pace by season",
    subtitle = "Break-down of pace contributions by season",
    caption = "Data: Champion Data",
    x = "Count",
    y = "Season",
    fill = "Legend") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))


# Matchups ----------------------------------------------------------------

season_id = 2021

o_rtg %>% 
  ungroup() %>% 
  filter(season == season_id) %>% 
  select(season,round,match,squadNickname,goals,possessions) %>% 
  group_by(squadNickname) %>% 
  mutate(mPossessions = mean(possessions)) %>% 
  full_join(.,.,by = c("season","round","match"),suffix = c("","_opp")) %>% 
  filter(squadNickname != squadNickname_opp) %>% 
  mutate(poss_mp = mPossessions + mPossessions_opp,
         game_pos = possessions + possessions_opp,
         team_ave = mPossessions * 2,
         faster = if_else(mPossessions > mPossessions_opp,"Faster","Slower"),
         win_loss = goals > goals_opp) %>% 
  ggplot(aes(round, game_pos)) +
  geom_point(aes(col = win_loss)) +
  geom_point(aes(y = poss_mp,shape = faster,fill = faster),size = 4) +
  scale_shape_manual(values = c("Faster" = 24, "Slower" = 25)) +
  scale_fill_manual(values = c("Faster" = "red", "Slower" = "green")) +
  scale_x_continuous(breaks = 1:14) +
  ggrepel::geom_text_repel(aes(label = squadNickname_opp),
                           min.segment.length = unit(0, 'lines'),
                           hjust = 0.5) +
  geom_hline(aes(yintercept = team_ave)) +
  labs(title = season_id) +
  facet_wrap(~fct_reorder(squadNickname,mPossessions,max, .desc = TRUE))


o_rtg %>% 
  ungroup() %>% 
  filter(season == 2022) %>% 
  select(season,round,match,squadNickname,goals,possessions) %>% 
  full_join(.,.,by = c("season","round","match"),suffix = c("","_opp")) %>% 
  filter(squadNickname != squadNickname_opp) %>%
  mutate(win_loss = if_else(goals > goals_opp,"Win","Loss"),
         goal_diff = goals - goals_opp) %>% 
  ggplot(aes(possessions,fct_reorder(squadNickname,possessions,mean))) +
  geom_boxplot() +
  geom_jitter(aes(col = squadNickname_opp,shape = win_loss),size = 6,
              position = position_jitter(seed = 1)) +
  geom_text(aes(label = goal_diff),
            position = position_jitter(seed = 1),col = "white") +
  scale_shape_manual(values = c("Loss" = 17,"Win" = 19)) +
  scale_color_manual(values = squadPrimaryCols) +
  labs(title = glue("Season {season_id}"))


o_rtg %>% 
  filter(season == 2020) %>%
  group_by(squadNickname) %>% 
  summarise(sd = sd(possessions)) %>% 
  arrange(sd)
