# SHAZ
library(tidyverse)
library(lubridate)
library(superNetballR)

dat <- map(paste("data/",2017:2021,"_data.RDS",sep = ""),read_rds) %>% 
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
  select(squadId,squadNickname,squadCode)

player_info <- extracted_metrics %>% 
  unnest_specific(player_info) %>% 
  group_by(playerId) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(playerId,surname,displayName)


shooting_stats <- extracted_metrics %>% 
  unnest_specific(scores)

player_info_all <- extracted_metrics %>% 
  unnest_specific(player_info) %>% 
  group_by(season,playerId) %>% 
  summarise(across(c(firstname, surname,displayName),first)) %>% 
  mutate(fullname = paste(firstname,surname))

team_stats <- extracted_metrics %>% 
  unnest_specific(team_stats) %>% 
  mutate(goal1 = if_else(goal1 == 0 | is.na(goal1),goals,goal1)) %>% 
  replace_na(list(goal1 = 0,goal2 = 0)) %>% 
         mutate(goals = goal1 + goal2*2)

player_stats <- extracted_metrics %>% 
  unnest_specific(player_stats) %>% 
  mutate(goal1 = if_else(goal1 == 0 | is.na(goal1),goals,goal1),
         generalPlayTurnovers = if_else(is.na(generalPlayTurnovers) ,turnovers,generalPlayTurnovers)) %>%
  replace_na(list(goal1 = 0,goal2 = 0)) %>% 
  mutate(goals = goal1 + goal2*2)

team_period_stats <- extracted_metrics %>%
  unnest_specific(teamPeriodStats) %>% 
  inner_join(team_info %>%
               select(squadId,squadNickname))

# Curate data -------------------------------------------------------------



game_results <- 
  team_stats %>% 
    transmute(season,round,match,opponent = squadId,goalsAgainst = goals,feedsAgainst = feeds) %>% 
    full_join(team_stats %>% transmute(season,round,match,squadId,goalsFor = goals,feedsFor = feeds),
              by = c("season","round","match")) %>% 
    filter(squadId != opponent) %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>% 
  left_join(team_info %>% transmute(opponent = squadId,opponentName = squadNickname)) %>% 
    select(season,round,match,squadId,squadNickname,goalsFor,feedsFor,opponent,opponentName,goalsAgainst, feedsAgainst)
  
# Team Value --------------------------------------------------------------

 # - Define how the output is verified

# What is mid-court effect and what is offensive effect?

## Defence -----

## Mid-court -----

#For team

game_results %>% 
  group_by(season,squadNickname) %>% 
  summarise(across(c(goalsFor,feedsFor,goalsAgainst,feedsAgainst),sum)) %>% 
  mutate(off = goalsFor/feedsFor,
         mid = feedsFor/feedsAgainst,
         def = goalsAgainst/feedsAgainst,
         meanOff = mean(off),
         meanMid = mean(mid),
         meanDef = mean(def),
         offAdj = off/meanOff,
         midAdj = mid/meanMid,
         defAdj = def/meanDef) %>% View()


  
 ## Offence -----

team_round_data %>% 
  inner_join(team_info) %>% 
  group_by(season,squadNickname) %>% 
  summarise(points = sum(points),
            goalAttempts = sum(goalAttempts),
            feeds = sum(feeds)) %>% 
  inner_join(p1 %>% select(season,squadNickname,OP,TAP)) %>% 
  ungroup() %>% 
  mutate(squadNickname = paste(season,squadNickname)) %>% 
  select(-season) %>% 
  GGally::ggpairs(aes(col = squadNickname,alpha = 0.5))

team_round_data %>% 
  inner_join(team_info) %>% 
  select(season,squadNickname,goalAssists,goalAttempts,points,feeds,feedWithAttempt) %>% 
  mutate(goa)
  ggplot(aes(goalAssists,points,col = factor(season))) +
  geom_point()
  
team_round_data %>% 
  inner_join(team_info) %>% 
  group_by(season,squadNickname) %>% 
  summarise(points = sum(points),
            goalAttempts = sum(goalAttempts),
            feeds = sum(feeds)) %>% 
  inner_join(p1 %>% select(season,squadNickname,OP,TAP)) %>% 
  pivot_longer(cols = -c(season,squadNickname)) %>% 
  ggplot(aes(season,value,col = squadNickname)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name,nrow = 1,scales = "free_y")
# Team Offence: (Team Points/Team Inside-50s) / League Average

p1 <- team_round_data %>% 
  group_by(season,squadId) %>% 
  summarise(team_ave_feeds = mean(feeds),
            team_ave_points = mean(points),
            OP_team = team_ave_points/team_ave_feeds,.groups = "drop") %>% 
    group_by(season) %>% 
    mutate(season_OP = mean(OP_team),
           season_TAP = mean(team_ave_points),
      OP = OP_team/season_OP * 100,
      TAP = team_ave_points/season_TAP) %>% 
    inner_join(team_info,by = "squadId") %>% 
    arrange(-OP)
# Player value ------------------------------------------------------------




# Player of the year recipients -------------------------------------------


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
    2018L,      "WA",      "Liz Watson (2)",    "Australia",         "Melbourne Vixens",
    2018L,       "C",  "Serena Guthrie (2)",      "England",           "Giants Netball",
    2018L,      "WD",        "Jess Anstiss",    "Australia",         "West Coast Fever",
    2018L,      "GD",     "Karla Pretorius", "South Africa", "Sunshine Coast Lightning",
    2018L,      "GK",     "Geva Mentor (2)",      "England", "Sunshine Coast Lightning",
    2019L,      "GS", "Jhaniele Fowler (2)",      "Jamaica",         "West Coast Fever",
    2019L,      "GA",  "Gretel Tippett (2)",    "Australia",     "Queensland Firebirds",
    2019L,      "WA",      "Liz Watson (3)",    "Australia",         "Melbourne Vixens",
    2019L,       "C",        "Kate Moloney",    "Australia",         "Melbourne Vixens",
    2019L,      "WD",        "Renae Ingles",    "Australia",         "Melbourne Vixens",
    2019L,      "GD", "Karla Pretorius (2)", "South Africa", "Sunshine Coast Lightning",
    2019L,      "GK",        "Emily Mannix",    "Australia",         "Melbourne Vixens",
    2020L,      "GS", "Jhaniele Fowler (3)",      "Jamaica",         "West Coast Fever",
    2020L,      "GA",        "Kiera Austin",    "Australia",           "Giants Netball",
    2020L,      "WA",      "Liz Watson (4)",    "Australia",         "Melbourne Vixens",
    2020L,       "C",    "Kate Moloney (2)",    "Australia",         "Melbourne Vixens",
    2020L,      "WD",    "Gabi Simpson (2)",    "Australia",     "Queensland Firebirds",
    2020L,      "GD", "Karla Pretorius (3)", "South Africa", "Sunshine Coast Lightning",
    2020L,      "GK",     "Geva Mentor (3)",      "England",      "Collingwood Magpies"
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
  left_join(player_info_all,by = c("season", "surname","firstname")) %>% 
  select(season,playerId, displayName,position)



# What's good WD? ---------------------------------------------------------

modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

player_stats %>% 
  group_by(season,playerId,squadId) %>% 
  summarise(across(c(generalPlayTurnovers,intercepts,penalties,contactPenalties,obstructionPenalties,gain,minutesPlayed),~sum(.x)/n()),
            fav_pos = modes(startingPositionCode)) %>% 
  left_join(POY %>% mutate(POY = TRUE)) %>% 
  replace_na(list(POY = FALSE)) %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>% 
  left_join(o_rtg_by_season) %>% 
  filter(fav_pos == "GK")
  select(-c(displayName,position,fav_pos)) %>% 
  pivot_longer(cols = 4:10) %>% 
  ggplot(aes(value,1, fill = POY)) +
  geom_dotplot()  +
  facet_wrap(~name, scales = "free")


o_rtg_by_season %>% 
  filter(season >= 2017) %>% 
  mutate(dPts = 100*(2 - dRtg/mdRtg),
         oPts = 100*oRtg/moRtg,
         midPts = feedDiff*100) %>% pivot_longer(c(dPts,oPts,midPts)) %>% 
  ggplot(aes(x = value, y = squadNickname, col = name)) +
  geom_point() +
  facet_wrap(~season)


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

         