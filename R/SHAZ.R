# SHAZ
library(tidyverse)
library(lubridate)
library(superNetballR)

source("R/load_netball_data.R")
load_netball_data(2017:2022)

# Curate data -------------------------------------------------------------


team_ratings <- player_stats %>%
  group_by(season,round,match,squadNickname) %>% 
  summarise(across(c(goals,goal1,goal2, goalAttempts,
                     goalMisses, generalPlayTurnovers,feeds),
                   function(col) sum(col,na.rm = T)),
            .groups = "drop") %>%
  left_join(player_stats %>% 
              group_by(playerId) %>% 
              filter(any(goals > 0)) %>% 
              ungroup() %>% 
              count(season,round, match, squadNickname,wt = rebounds,name = "offensiveRebounds"),
            by = c("round", "match", "squadNickname","season")) %>% 
  mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers,
         oRtg = goals/possessions * 100) %>% 
  # Join and rename opponent's stats
  {full_join(transmute(.data = .,
                       season,round,match,
                       across(c(squadNickname, goals, generalPlayTurnovers, feeds, possessions),
                              function(x) x,
                              .names = "{.col}_for"),
                       oRtg),
             transmute(.data = .,
                       season,round,match,
                       across(c(squadNickname, goals, generalPlayTurnovers, feeds, possessions),
                              function(x) x,
                              .names = "{.col}_against"),
                       dRtg = oRtg),
             by = c('season','round','match'))} %>% 
  filter(squadNickname_for != squadNickname_against) %>% 
  group_by(season, round) %>% 
  mutate(feed_differential = feeds_for/feeds_against,
         def_round_pts = (2 - dRtg/mean(dRtg)) * 100,
         off_round_pts = oRtg/mean(dRtg) * 100,
         mid_round_pts = feed_differential/mean(feed_differential) * 100) %>% 
  ungroup() %>% 
  select(season, round, match, squadNickname = squadNickname_for, def_round_pts, off_round_pts, mid_round_pts)


# Team Value --------------------------------------------------------------

## Defence -----

team_ratings %>%
  mutate(season = factor(season)) %>% 
  ggplot(aes(x = round, y = def_round_pts, col = season, group = season)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 100) +
  facet_wrap(~squadNickname_for)

## Mid-court -----

team_ratings %>% 
  select(season:squadNickname_for,squadNickname_against,feeds_for, feeds_against, feed_differential) %>% 
  group_by(season, round) %>% 
  mutate(mid_round_pts = feed_differential/mean(feed_differential) * 100,
         mean_diff = mean(feed_differential)) %>% View
  
## Offence -----


# Team Offence: (Team Points/Team Inside-50s) / League Average


# Player value ------------------------------------------------------------

## Player position

player_pos <- player_stats %>% 
  group_by(season, playerId, currentPositionCode) %>% 
  summarise(count = n(),
            .groups = "drop_last") %>% 
  slice_max(count, n = 1) %>% 
  ungroup() %>% 
  select(season, playerId, most_common_pos = currentPositionCode)

## Offense ----

mc_weights <-  tibble(generalPlayTurnovers = 0,
                      feeds = 1,
                      intercepts = 5,
                      penalties = 0.5,
                      gain = 5)

mc_calc <- player_stats %>% 
  left_join(positions_per_time, 
            by = c("season", "round", "playerId", "period"))
  group_by(season, round, match, squadNickname, playerId, displayName) %>% 
  summarise(across(c(generalPlayTurnovers, feeds, intercepts, penalties, gain, minutesPlayed),
                   function(col) sum(col)),
            .groups = "drop") %>% 
  filter(season == 2022, round == 2, match == 4) %>% 
  bind_cols(mc_weights %>% 
              rename_all(~paste0(.x, ".wt")))

map_dfc(colnames(mc_weights), ~mc_calc %>% 
          select(starts_with(.x)) %>%  
          reduce(.,`*`)) %>% 
  rename_all(~colnames(mc_weights)) %>%
  bind_cols(mc_calc %>% 
              select(season:displayName), .) %>% 
  mutate(plyr_game_score = generalPlayTurnovers + feeds + intercepts + penalties + gain)

## Mid-court ----

# Mid-court allocation is going to be difficult when looking at anything other than feeds themselves
# Unless you look at mid-courters only, which is problematic too.
# Turnovers, gains, intercepts can be obtained in any part of the court.

mc_weights <-  tibble(generalPlayTurnovers = -10,
                      feeds = 10,
                      intercepts = 0,
                      penalties = -0.5,
                      gain = 15)

mc_calc <-
  player_stats %>% 
  group_by(season, round, match, squadNickname, playerId, displayName) %>% 
  summarise(across(c(generalPlayTurnovers, feeds, intercepts, penalties, gain, minutesPlayed),
                   function(col) sum(col)),
            .groups = "keep") %>% 
    select(all_of(colnames(mc_weights))) %>% 
    pivot_longer(cols = all_of(colnames(mc_weights)), names_to = "stat") %>% 
    left_join(mc_weights %>% pivot_longer(cols = everything(), names_to = "stat", values_to = "weight")) %>% 
    summarise(mid_player_score = sum(value * weight)) %>% 
  group_by(season, round, match, squadNickname) %>% 
  mutate(mid_player_score = if_else(mid_player_score < 0, 0, mid_player_score),
         mid_team_pct = mid_player_score/sum(mid_player_score)) %>% 
  left_join(team_ratings) %>% 
  mutate(shas = mid_team_pct * mid_round_pts) %>% 
  ungroup()

mc_calc %>% 
  group_by(season, squadNickname, playerId, displayName) %>% 
  summarise(shas = sum(shas)) %>% 
  group_by(season) %>% 
  slice_max(shas, n = 10) %>% 
  left_join(POY %>% select(-displayName),
            by = c("season", "playerId")) %>% 
  mutate(displayName = tidytext::reorder_within(displayName, shas, season)) %>% 
  ggplot(aes(x = shas, y = displayName, fill = position)) +
  geom_col() +
  tidytext::scale_y_reordered() +
  facet_wrap(~season, scales = "free_y")
  

# Shooters ----------------------------------------------------------------

shooters <- player_stats %>% 
  group_by(season,playerId,squadId) %>% 
  summarise(across(c(generalPlayTurnovers,intercepts,goals, goalAssists, feeds, rebounds,gain,minutesPlayed,goalAttempts),~sum(.x)/n()),
            fav_pos = modes(startingPositionCode)) %>% 
  left_join(POY %>% mutate(POY = TRUE)) %>% 
  replace_na(list(POY = FALSE)) %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>% 
  left_join(player_info_all,by = c("season", "playerId")) %>% 
  left_join(o_rtg_by_season) %>% 
  filter(goals > 0) %>%
  mutate(persRtg = goals + goalAssists*0.8 + 0.65*feeds - 0.8*generalPlayTurnovers + intercepts) %>% 
  filter(season == 2021) %>% 
  arrange(-persRtg) %>% 
  group_by(season,squadNickname) %>% 
  mutate(persRtgPct = persRtg/sum(persRtg)) %>% 
  select(season,surname,squadNickname,persRtg,persRtgPct,generalPlayTurnovers,intercepts,goals, goalAttempts, goalAssists, feeds, rebounds,gain,minutesPlayed,oRtg,POY,fav_pos)


shooters %>% 
  select(surname,goals,goalAssists,goalAttempts,feeds,rebounds) %>% 
  filter(squadNickname %in% c("Swifts", "Firebirds")) %>% 
  arrange(squadNickname)



# Assessment --------------------------------------------------------------

## Player of the year recipients -------------------------------------------


POY <- tibble::tribble(
  ~season, ~position,                 ~Name,   ~Nationality,                      ~Team,
  2017L,      "GS",       "Mwai Kumwenda",       "Malawi",         "Melbourne Vixens",
  2017L,      "GA",        "Tegan Philip",    "Australia",         "Melbourne Vixens",
  2017L,      "WA",          "Liz Watson",    "Australia",         "Melbourne Vixens",
  2017L,       "C",       "Laura Langman",  "New Zealand", "Sunshine Coast Lightning",
  2017L,      "WD",        "Gabi Simpson",    "Australia",     "Queensland Firebirds",
  2017L,      "GD",           "Jo Weston",    "Australia",         "Melbourne Vixens",
  2017L,      "GK",         "Geva Mentor",      "England", "Sunshine Coast Lightning",
  2018L,      "GS",     "Jhaniele Fowler",      "Jamaica",         "West Coast Fever",
  2018L,      "GA",      "Gretel Tippett",    "Australia",     "Queensland Firebirds",
  2018L,      "WA",      "Liz Watson",    "Australia",         "Melbourne Vixens",
  2018L,       "C",  "Serena Guthrie",      "England",           "Giants Netball",
  2018L,      "WD",        "Jess Anstiss",    "Australia",         "West Coast Fever",
  2018L,      "GD",     "Karla Pretorius", "South Africa", "Sunshine Coast Lightning",
  2018L,      "GK",     "Geva Mentor",      "England", "Sunshine Coast Lightning",
  2019L,      "GS", "Jhaniele Fowler",      "Jamaica",         "West Coast Fever",
  2019L,      "GA",  "Gretel Tippett",    "Australia",     "Queensland Firebirds",
  2019L,      "WA",      "Liz Watson",    "Australia",         "Melbourne Vixens",
  2019L,       "C",        "Kate Moloney",    "Australia",         "Melbourne Vixens",
  2019L,      "WD",        "Renae Ingles",    "Australia",         "Melbourne Vixens",
  2019L,      "GD", "Karla Pretorius", "South Africa", "Sunshine Coast Lightning",
  2019L,      "GK",        "Emily Mannix",    "Australia",         "Melbourne Vixens",
  2020L,      "GS", "Jhaniele Fowler",      "Jamaica",         "West Coast Fever",
  2020L,      "GA",        "Kiera Austin",    "Australia",           "Giants Netball",
  2020L,      "WA",      "Liz Watson",    "Australia",         "Melbourne Vixens",
  2020L,       "C",    "Kate Moloney",    "Australia",         "Melbourne Vixens",
  2020L,      "WD",    "Gabi Simpson",    "Australia",     "Queensland Firebirds",
  2020L,      "GD", "Karla Pretorius", "South Africa", "Sunshine Coast Lightning",
  2020L,      "GK",     "Geva Mentor",      "England",      "Collingwood Magpies"
) %>% select(-Nationality) %>% 
  bind_rows(
    tibble::tribble(
      ~season,  ~position,              ~Name,                      ~Team,
      2018L, "Reserves",        "Jo Harten",           "Giants Netball",
      2018L, "Reserves",  "Kimberlee Green",           "Giants Netball",
      2018L, "Reserves",      "Laura Geitz",     "Queensland Firebirds",
      2019L, "Reserves",      "Cara Koenen", "Sunshine Coast Lightning",
      2019L, "Reserves",     "Paige Hadley",   "New South Wales Swifts",
      2019L, "Reserves", "Shamera Sterling",    "Adelaide Thunderbirds",
      2020L, "Reserves", "Samantha Wallace",   "New South Wales Swifts",
      2020L, "Reserves", "Ashleigh Brazill",      "Collingwood Magpies",
      2020L, "Reserves", "Shamera Sterling",    "Adelaide Thunderbirds",
      2017L, "Reserves", "Caitlin Thwaites",      "Collingwood Magpies",
      2017L, "Reserves",   "Serena Guthrie",           "Giants Netball",
      2017L, "Reserves",   "Rebecca Bulley",           "Giants Netball"
    )
  ) %>% 
  mutate(Name = str_remove(Name,"\\s?\\(.\\)")) %>% 
  separate(Name,c("firstname","surname"),sep = " ",remove = F) %>% 
  fuzzyjoin::stringdist_left_join(player_info %>%
                                    distinct(playerId, firstname, surname),
                                  distance_col = "n_dist",
                                  max_dist = 7,
                                  by = c("firstname","surname")) %>% 
  group_by(firstname.x, surname.x) %>% 
  slice_min(order_by = (firstname.n_dist + surname.n_dist), n = 1) %>% 
  ungroup() %>% 
  select(season, playerId, displayName = Name, position)



# Improvements ------------------------------------------------------------

# - location of the event, turnover
# - Inside forward third
