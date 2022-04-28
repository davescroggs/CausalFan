library(tidyverse)
library(ggnewscale)

source("R/load_netball_data.R")
load_netball_data(2017:2022)

o_rtg <- player_stats %>%
  group_by(season,round,match,squadNickname) %>% 
  summarise(across(c(goals,goal1,goal2, goalAttempts, goalMisses, generalPlayTurnovers,turnovers,feeds),~sum(.,na.rm = T))) %>%
  left_join(player_stats %>% 
  group_by(playerId) %>% 
  filter(any(goals > 0)) %>% 
  ungroup() %>% 
  count(season,round, match, squadNickname,wt = rebounds,name = "offensiveRebounds"),
  by = c("round", "match", "squadNickname","season")) %>% 
  mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers)



o_rtg_by_season <- o_rtg  %>% 
  transmute(round,match,opponent = squadNickname,dPoss = possessions,dGoals = goals,dFeeds = feeds) %>% 
  full_join(o_rtg) %>% 
  filter(opponent != squadNickname) %>% 
  group_by(season, squadNickname) %>% 
  summarise(oRtg = sum(goals)/sum(possessions)*100,
            dRtg = sum(dGoals)/sum(dPoss)*100,
            feedDiff = sum(feeds)/sum(dFeeds),
            Net = oRtg - dRtg,.groups = "drop_last") %>% 
  mutate(moRtg = mean(oRtg),
         mdRtg = mean(dRtg),
         mNet = mean(Net),
         netRank = dense_rank(desc(Net))) %>% 
  #left_join(team_info %>% select(squadNickname,country)) %>% 
  ungroup()
  
{o_rtg_by_season %>% 
ggplot(aes(dRtg,oRtg,col = factor(season),group = squadNickname)) +
    geom_abline() +
  geom_point() +
  coord_equal()} %>% plotly::ggplotly()

{o_rtg_by_season  %>% 
  ggplot(aes(y = season, x = Net,group = squadNickname)) +
  geom_point(aes(col = country)) +
  scale_color_manual(values = c("Australia" = "#FFCD00","New Zealand" = "black")) +
  scale_y_continuous(breaks = 2009:2021)} %>% 
  plotly::ggplotly()

o_rtg_by_season %>% 
  select(squadNickname,season,oRtg,dRtg,Net) %>% 
  arrange(-Net)

# Possession outcomes -----------------------------------------------------

o_rtg %>%
  group_by(season,squadNickname) %>% 
  summarise(across(c(goalAttempts,generalPlayTurnovers,offensiveRebounds,possessions),sum),.groups = "keep") %>% 
  group_by(season) %>% 
  mutate(plot_rank = dense_rank(goalAttempts)) %>%
  pivot_longer(cols = c(goalAttempts,generalPlayTurnovers,offensiveRebounds)) %>% 
  group_by(season,squadNickname) %>% 
  mutate(pct = value/sum(value)) %>% 
  ungroup() %>% 
  mutate(squadNickname = tidytext::reorder_within(squadNickname,pct,list(season,plot_rank))) %>%
  ggplot(aes(y = squadNickname, x = pct,fill = name)) +
  geom_col(position = "stack") +
  #tidytext::scale_y_reordered() +
    facet_wrap(~season, scales = "free_y")

team_stats %>% 
  filter(season == 2014) %>% 
  select(season,round,match,squadId) %>% 
  full_join(.,transmute(.,season,round,match,opponent = squadId)) %>% 
  filter(squadId == 804L,squadId != opponent) %>% 
  left_join(team_info %>% rename("opponent" = "squadId")) %>% 
  count(country,squadNickname)
  

# O Rating - Players ------------------------------------------------------

player_positions_off <- 
  player_period_stats %>% 
  mutate(startingPositionCode = if_else(startingPositionCode == "-",currentPositionCode,startingPositionCode)) %>%
  group_by(season,squadId,startingPositionCode,playerId) %>% 
  summarise(n = n(),.groups = "keep") %>% 
  filter(any(startingPositionCode != "I" & n > 19)) %>% 
  ungroup() %>% 
  bind_rows(player_stats %>% 
              filter(season == 2014,startingPositionCode != "-") %>% 
              group_by(season,squadId,startingPositionCode,playerId) %>% 
              summarise(n = n(),.groups = "keep") %>% 
              filter(any(startingPositionCode != "I" & n > 3)) %>% 
              ungroup()) %>% 
  left_join(player_info_all) %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>%
  filter(startingPositionCode %in% c("GS","GA","WA","C")) %>% 
  group_by(season,squadNickname) %>% 
  summarise(players = paste(unique(fullname), collapse = ", "))

player_positions_def <- 
  player_period_stats %>% 
  mutate(startingPositionCode = if_else(startingPositionCode == "-",currentPositionCode,startingPositionCode)) %>%
  group_by(season,squadId,startingPositionCode,playerId) %>% 
  summarise(n = n(),.groups = "keep") %>% 
  filter(any(startingPositionCode != "I" & n > 19)) %>% 
  ungroup() %>% 
  bind_rows(player_stats %>% 
              filter(season == 2014,startingPositionCode != "-") %>% 
              group_by(season,squadId,startingPositionCode,playerId) %>% 
              summarise(n = n(),.groups = "keep") %>% 
              filter(any(startingPositionCode != "I" & n > 4)) %>% 
              ungroup()) %>% 
  left_join(player_info_all) %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>%
  filter(startingPositionCode %in% c("GK","GD","WD","C")) %>% 
  group_by(season,squadNickname) %>% 
  summarise(players = paste(unique(fullname), collapse = ", "))

player_positions_all <- 
  player_period_stats %>% 
  mutate(startingPositionCode = if_else(startingPositionCode == "-",currentPositionCode,startingPositionCode)) %>%
  group_by(season,squadId,startingPositionCode,playerId) %>% 
  summarise(n = n(),.groups = "keep") %>% 
  filter(any(startingPositionCode != "I" & n > 19)) %>% 
  ungroup() %>% 
  bind_rows(player_stats %>% 
              filter(season == 2014,startingPositionCode != "-") %>% 
              group_by(season,squadId,startingPositionCode,playerId) %>% 
              summarise(n = n(),.groups = "keep") %>% 
              filter(any(startingPositionCode != "I" & n > 3)) %>% 
              ungroup()) %>% 
  left_join(player_info_all) %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>% 
    mutate(startingPositionCode = factor(startingPositionCode,c("GK","GD","WD","C","WA","GA","GS"),ordered = T)) %>% 
    arrange(season,squadNickname,startingPositionCode) %>% 
  group_by(season,squadNickname) %>% 
  summarise(players = paste(unique(fullname), collapse = ", "))

shooting_stats %>% 
  left_join(team_info) %>% 
  left_join(player_info) %>% 
  filter(squadNickname == "Vixens", season == 2014) %>% 
  count(displayName, wt = scorepoints)

player_stats %>% 
  filter(season == 2014,startingPositionCode != "-",squadId == 804) %>% 
  group_by(season,squadId,startingPositionCode,playerId) %>% 
  summarise(n = n()) %>% 
  arrange(startingPositionCode,-n) %>% 
  ungroup()


player_stats %>% 
  filter(season == 2014,startingPositionCode != "-",squadId == 804) %>% 
  group_by(playerId) %>% 
  filter(any(startingPositionCode %in% c("GS","GA","WA","C"))) %>% 
  count(playerId) %>% 
  left_join(player_info)


# Off rating table --------------------------------------------------------

# Best teams
o_rtg_by_season %>% 
  ungroup() %>% 
  arrange(-oRtg) %>% 
  head(10) %>% 
  left_join(player_positions_off) %>% 
  inner_join(tribble(
                ~season, ~squadNickname,                                                                                                                              ~Outcome,
                  2021L,        "Fever", "Finished 3rd, with most wins$^{2}$. Most goals in a season. Lost prelim by 3.",
                  2013L,        "Steel",                                         "Losing record, finishing 6th. Most goals in this season.",
                  2016L,        "Steel",                    "Split conference. Perfect season. Only NZ team with goal %$^{3}$ > 100%. Most goals in this season. Lost semi by 3.",
                  2014L,        "Steel",                                                         "Finished 5th. Most goals in this season, goal % only just under 100%.",
                  2020L,       "Vixens",                                                                               "Finished 1st, highest goal % for the season. Won GF by 2.",
                  2020L,        "Fever",                                 "Finished 3rd, most goals in this season. Lost GF by 2. 2nd most goals in a season since 2009.",
                  2021L,       "Swifts",                                                                                  "Finished equal first on points. Won GF by 4 goals.",
                  2020L,       "Swifts",                                                                                 "Finished 4th, 2nd most total goals in season. Lost semi by 5.",
                  2012L,   "WBOP Magic",                                                                              "3rd on ladder, with highest goal %. Won GF by 3",
                  2013L,        "Pulse",                                                                        "Finished 5th. 4th most total goals. Goal % only just above 100%."
                )
) %>% 
  transmute(Season = season,
    Team = squadNickname,
    `Offensive rating` = round(oRtg,digits = 1),
    Country = str_replace(country,"New Zealand","New\U2009Zealand"),
    Outcome,
    `Attacking players$^{1}$` = players) %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic(lightable_options = "striped") %>% 
  kableExtra::column_spec(column = 3:4, width = "100px") %>% 
  kableExtra::add_footnote(c("Player must have played at least 1/3 of all quarters one of GS, GA, WA, C",
                             "Lost points due to due to salary cap violations",
                             "Goal% = goals for/goals against"),notation = "number")


o_rtg_by_season %>% 
  arrange(-oRtg) %>% 
  head(20) %>% 
  ggplot(aes(season)) +
  geom_bar(fill = "dodgerblue",col = "black") +
  theme_bw() +
  labs(title = "Top 20 Offensive Ratings",
    subtitle = "The seasons where a top 20 offensive rating appeared",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))

# Worst teams

o_rtg_by_season %>% 
  ungroup() %>% 
  arrange(oRtg) %>% 
  head(10) %>% 
  inner_join(tribble(~season, ~squadNickname, ~Outcome,
    2017, "Thunderbirds", "Finshed last with 1 win. Fewest goals for the season. Highest turnovers per game in all years.",
    2017, "Fever", "Finshed 2nd last with 1 win. 2nd fewest goals for the season.",
    2018, "Thunderbirds", "Finished last with zero wins. Fewest goals for the season, 130 goals less than the next most goals.",
    2019, "Thunderbirds", "Finished 2nd last. Fewest goals for the season, 80 goals less than the next most goals.",
    2009, "Pulse", "Finished last with 1 win. Fewest goals for the season, 70 goals less than the next most goals. Second hightest turnovers per game in all seasons.",
    2009, "Tactix", "Finished 6th. Second fewest goals for the season.",
    2009, "Fever", "Finished 7th.",
    2009, "Mystics", "Finished 8th.",
    2010, "Pulse", "Finished equal last, with second fewest goals for the season.",
    2010, "Tactix", "Finished equal last, with fewest goals for the season. Second fewest goals per game in all years."
  )) %>% 
  left_join(player_positions_off) %>% 
  transmute(Season = season,
            Team = squadNickname,
            `Offensive rating` = round(oRtg,digits = 1),
            Country = str_replace(country,"New Zealand","New\U2009Zealand"),
            Outcome,
            `Attacking players` = players) %>% 
knitr::kable() %>% 
  kableExtra::kable_classic(lightable_options = "striped") %>% 
  kableExtra::column_spec(column = 3:4, width = "100px")

# Off rating plot ---------------------------------------------------------

o_rtg_by_season %>% 
  ungroup() %>% 
  arrange(-oRtg) %>% 
  mutate(Rank = 1:n(),
         Team = paste(season,squadNickname,sep = " - ")) %>% 
  head(10) %>% 
  ggplot(aes(x = oRtg, y = Rank, col = country)) +
  geom_point() +
  geom_text(aes(label = Team),hjust = -0.1,show.legend = F) +
  scale_y_reverse(breaks = NULL) +
  scale_colour_manual(values = c("Australia" = "#008B00","New Zealand" = "black")) +
  scale_x_continuous(breaks = 71:85) +
  expand_limits(x = 81) +
  theme_bw() +
  labs(title = "Offensive rating",
    subtitle = "Top 20 offensive ratings in ANZ Championship and SSN, 2009 - 2021",
    x = "Offensive Rating",
    y = "",
    colour = "Country of origin",
    caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))

player_stats %>%
  filter(playerId == 80826) %>% 
  distinct(season,squadId) %>% 
  left_join(team_info)

team_stats %>% 
  mutate(goal1 = if_else(goal1 == 0 | is.na(goal1),goals,goal1),
         generalPlayTurnovers = if_else(generalPlayTurnovers == 0 | is.na(generalPlayTurnovers),turnovers,generalPlayTurnovers)) %>% 
  replace_na(list(goal1 = 0,goal2 = 0)) %>% 
  mutate(possessions = goalAttempts - offensiveRebounds + generalPlayTurnovers,
         goals = goal1 + goal2*2) %>%
  group_by(season) %>% 
  summarise(across(c(goals,goalAttempts,generalPlayTurnovers),sum)) %>% 
  pivot_longer(-season) %>% 
  ggplot(aes(season,value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name,scales = "free",ncol = 1)

shooting_stats %>% 
  count(season,scoreName) %>% 
  pivot_wider(names_from = scoreName,values_from = n,values_fill = list(n = 0)) %>% 
  mutate(attempts = goal + `2pt Goal` + miss + `2pt Miss`,
         `2pt Goal` = `2pt Goal` * 2L) %>% 
  select(-contains("miss")) %>% 
  pivot_longer(-season) %>% 
  mutate(type = if_else(name == "attempts","attempts","goals")) %>% 
  ggplot(aes(season,value, fill = name)) +
  geom_col() +
  facet_wrap(~type,scales = "free")

shooting_stats %>% 
  count(season,scoreName,squadId) %>% 
  pivot_wider(names_from = scoreName,values_from = n,values_fill = list(n = 0)) %>% 
  mutate(attempts = goal + `2pt Goal` + miss + `2pt Miss`,
         `2pt Goal` = `2pt Goal` * 2L,
         g = goal + `2pt Goal`,
         gps = (g) / attempts) %>% 
  arrange(-g) %>% 
  left_join(team_info)

o_rtg_by_season %>% 
  arrange(-oRtg) %>% 
  left_join(player_positions_off) %>%
  transmute(Rank = 1:n(),
            Season = season,
            Team = squadNickname,
            `Offensive rating` = round(oRtg,digits = 1),
            Country = str_replace(country,"New Zealand","New\U2009Zealand"),
            `Attacking players` = players) %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic(lightable_options = "striped") %>% 
  kableExtra::column_spec(column = 3:4, width = "100px") %>% 
  kableExtra::save_kable(file = "images/offensive_ratings.jpeg")


team_stats %>% 
  group_by(season,squadId) %>% 
  summarise(across(c(goals,goalAttempts, turnovers),sum),
            n = n(),
            gpg = goals/n,
            tpg = turnovers/n,
            accuracy = goals/goalAttempts,
            .groups = "drop") %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>% 
  mutate(rank_to = dense_rank(tpg),
         rank_g = dense_rank(gpg)) %>% 
  arrange(-rank_to)


# The good and bad years --------------------------------------------------

o_rtg_by_season  %>% 
  ggplot(aes(y = season, x = oRtg)) +
  geom_point(aes(col = "Team rating")) +
  scale_colour_manual(values = c("Season average" = "red", "Team rating" = "black")) +
  ggrepel::geom_text_repel(data = o_rtg_by_season %>%
                             arrange(-oRtg) %>%
                             head(10),
                           aes(label = squadNickname),
                           min.segment.length = unit(0, 'lines'),
                           hjust = 0.5) +
  ggrepel::geom_text_repel(data = o_rtg_by_season %>%
                             arrange(oRtg) %>%
                             head(10),
                           aes(label = squadNickname),
                           min.segment.length = unit(0, 'lines'),
                           hjust = 0.5) +
  geom_point(aes(x = moRtg,col = "Season average"),size = 3) +
  scale_y_continuous(breaks = 2009:2021) +
  scale_x_continuous(breaks = seq(50,80,2)) +
  labs(title = "Offensive rating trends",
       subtitle = "Changes in offensive ratings 2009 - 2021",
       x = "Offensive rating",
       y = "Season",
       caption = "Data: Champion Data",
       colour = "Offensive rating") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"))



# Net rating --------------------------------------------------------------

# Best teams
o_rtg_by_season %>% 
  ungroup() %>% 
  arrange(-Net) %>% 
  head(10) %>% 
  left_join(player_positions_all) %>% 
  inner_join(tribble(
    ~season, ~squadNickname,  ~Outcome,
        2009L,       "Vixens", "Finished 1st with 1 loss. 5th fewest goals against and the most turnovers against in all seasons. Won GF by 8 goals.",
        2009L,   "WBOP Magic", "Finished 2nd. Fewest goals against of all seasons. Lost SF by 15.",
        2010L,       "Swifts", "Finished 1st with 0 losses. Lost SF by 5. 3rd fewest goals and 3rd most turnovers against in all seasons.",
        2011L,    "Firebirds", "Finished 1st with 0 losses. Won the GF by 13.",
        2014L,       "Vixens", "Finished 1st with 4 losses. Won the GF by 11. Goal differential of +154.",
        2016L,    "Firebirds", "Finished 1st in Aus pool with 0 losses. Won GF by 2. Goal differential of +140.",
        2016L,        "Steel", "Split conference$^{1}$. Finished 1st in NZ pool with 0 losses. Only NZ team with goal % > 100%. Most goals in this season. Lost semi by 3.",
        2016L,       "Swifts", "Finished 2nd in Aus pool. Lost GF by 2. Goal differential of +158.",
        2017L,    "Lightning", "Finished 2nd with fewest goals against for the season. Won the GF by 17. Forced second most turnovers in all seasons.",
        2017L,       "Vixens", "Finished top of the ladder. Lost the prelim by 8."
  )) %>% 
  transmute(Season = season,
            Team = squadNickname,
            `Net rating` = round(Net,digits = 1),
            Country = str_replace(country,"New Zealand","New\U2009Zealand"),
            Outcome,
            Players = players) %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic(lightable_options = "striped") %>% 
  kableExtra::column_spec(column = 3:4, width = "100px") %>% 
  kableExtra::add_footnote(c("2016 had a split conference between Australian and New Zealand based teams."),notation = "number")

o_rtg_by_season  %>% 
  ggplot(aes(y = season, x = Net)) +
  geom_vline(xintercept = 0) +
  geom_point(data = o_rtg_by_season %>% 
               group_by(season,country) %>% 
               summarise(mNet = mean(Net)),
             aes(x = mNet,
                 fill = country,
                 shape = "Average"),
             col = "black",
             size = 5) +
  geom_point(aes(fill = country,
                 shape = "Team",
             col = country),
             size = 2,
             shape = 21) +
  ggrepel::geom_text_repel(data = o_rtg_by_season %>%
                             arrange(-Net) %>%
                             head(10),
                           aes(label = squadNickname),
                           min.segment.length = unit(0, 'lines'),
                           hjust = 0.5) +
  ggrepel::geom_text_repel(data = o_rtg_by_season %>%
                             arrange(Net) %>%
                             head(10),
                           aes(label = squadNickname),
                           min.segment.length = unit(0, 'lines'),
                           hjust = 0.5) +
  scale_color_manual(values = c("Australia" = "black","New Zealand" = "white"),guide = "none") +
  scale_fill_manual(values = c("Australia" = "#FFCD00","New Zealand" = "black")) +
  scale_shape_manual(values = c("Average" = 24,"Team" = 19)) +
  scale_y_continuous(breaks = 2009:2021) +
  scale_x_continuous(breaks = seq(-16,16,2),limits = c(-15,15)) +
  labs(title = "Net rating trends",
       subtitle = "Changes in net ratings 2009 - 2021",
       x = "Net rating",
       y = "Season",
       caption = "Data: Champion Data",
       fill = "Net rating",
       shape = "Metric") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black")) +
  guides(fill = guide_legend(override.aes = list(shape = c(21,21),fill = c("#FFCD00","black"),col = c("#FFCD00","black"))))

o_rtg_by_season %>% 
  ungroup() %>% 
  arrange(-Net) %>% View()
  head(10) %>% 
  transmute(Season = season,
            Team = squadNickname,
            `Defensive rating` = round(dRtg,digits = 1),
            Country = str_replace(country,"New Zealand","New\U2009Zealand"),
            Outcome,
            `Attacking players$^{1}$` = players) %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic(lightable_options = "striped") %>% 
  kableExtra::column_spec(column = 3:4, width = "100px") %>% 
  kableExtra::add_footnote(c("Player must have played at least 1/3 of all quarters one of GK, GD, WD, C"),notation = "number")

# What's the difference in wins a difference in net rating of 4 is about an additional 2 wins per season.
  
  team_stats %>%
    mutate(goal1 = if_else(goal1 == 0L | is.na(goal1),as.integer(goals),goal1)) %>% 
    replace_na(list(goal1 = 0L,goal2 = 0L)) %>% 
    mutate(goals = goal1 + goal2*2L) %>% 
    ungroup() %>% 
    select(season,round,match,squadId,goals) %>% 
    full_join(x = .,y = .,by = c("season","round","match")) %>% 
    filter(squadId.x != squadId.y) %>% 
    mutate(result = case_when(goals.x == goals.y ~ "Draw",
                              goals.x < goals.y ~ "Loss",
                              goals.x > goals.y ~ "Win")) %>% 
    count(squadId.x,season,result) %>% 
    pivot_wider(names_from = result,values_from = n,values_fill = list(n = 0)) %>% 
    mutate(win_pct = Win/(Win + Loss + Draw)) %>% 
    rename("squadId" = "squadId.x") %>% 
    left_join(team_info %>% select(squadId,squadNickname)) %>% 
    left_join(o_rtg_by_season %>% select(season,squadNickname,Net)) %>% 
    lm(data = .,win_pct ~ Net) %>% summary()
  
  o_rtg_by_season %>% 
    arrange(-Net) %>% 
    left_join(player_positions_all) %>%
    transmute(Rank = 1:n(),
              Season = season,
              Team = squadNickname,
              `Net rating` = round(Net,digits = 1),
              Country = str_replace(country,"New Zealand","New\U2009Zealand"),
              `Players` = players) %>% 
    knitr::kable() %>% 
    kableExtra::kable_classic(lightable_options = "striped") %>% 
    kableExtra::column_spec(column = 3:4, width = "100px") %>% 
    kableExtra::save_kable(file = "images/net_ratings.jpeg")

# All top 10's ------------------------------------------------------------

  
  o_rtg_by_season %>% 
    ungroup() %>% {bind_rows(
        arrange(.,-Net) %>% 
      head(10) %>%
      transmute(season,squadNickname,rating = Net,
        rank = 1:n(),
             type = "Net"),
      arrange(.,-oRtg) %>% 
      head(10) %>%
      transmute(season,squadNickname, rating = oRtg,
                rank = 1:n(),
             type = "Offensive"),
      arrange(.,dRtg) %>% 
      head(10) %>%
      transmute(season,squadNickname, rating = dRtg,
                rank = 1:n(),
             type = "Defensive"))} %>% 
    mutate(squad = paste(season,squadNickname, sep = " - ")) %>% 
    ggplot(aes(type, fct_reorder(squad,season),label = rank)) +
    geom_tile() +
    geom_text(col = "white") +
    theme_bw() +
    labs(title = "Top rated teams",
      subtitle = "Teams that placed in any of the top offensive, defensive or net ratings",
      x = "Rating",
      y = "",
      caption = "Data: Champion Data") +
    theme(plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 1),
      plot.background = element_rect(colour = "black"))
  
# Winning percentage ------------------------------------------------------

team_stats %>%
    mutate(goal1 = if_else(goal1 == 0L | is.na(goal1),as.integer(goals),goal1)) %>% 
  replace_na(list(goal1 = 0L,goal2 = 0L)) %>% 
  mutate(goals = goal1 + goal2*2L) %>% 
  ungroup() %>% 
  select(season,round,match,squadId,goals) %>% 
  full_join(x = .,y = .,by = c("season","round","match")) %>% 
  filter(squadId.x != squadId.y) %>% 
  mutate(result = case_when(goals.x == goals.y ~ "Draw",
    goals.x < goals.y ~ "Loss",
    goals.x > goals.y ~ "Win")) %>% 
  count(squadId.x,season,result) %>% 
  pivot_wider(names_from = result,values_from = n,values_fill = list(n = 0)) %>% 
  mutate(win_pct = Win/(Win + Loss + Draw)) %>% 
  rename("squadId" = "squadId.x") %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>% 
  left_join(o_rtg_by_season %>% select(season,squadNickname,Net)) %>% 
    mutate(resid = 0.04*Net - win_pct) %>% 
  {ggplot(data = .,aes(x = Net,y = win_pct,
             #group = paste(season,squadNickname)
             )) +
      geom_smooth(method = "lm",se = F, col = "black") +
  geom_point() + 
      ggrepel::geom_text_repel(data = arrange(.,resid) %>% head(3),aes(label = paste(season,squadNickname,sep = " - ")),
                min.segment.length = unit(0, 'lines'),
                hjust = 0.5) +
      ggrepel::geom_text_repel(data = arrange(.,-resid) %>% head(2),aes(label = paste(season,squadNickname,sep = " - ")),
                min.segment.length = unit(0, 'lines'),
                hjust = 0.5) +
      ggrepel::geom_text_repel(data = filter(.,Net > 10 | Net < -13),aes(label = paste(season,squadNickname,sep = " - ")),
                               min.segment.length = unit(0, 'lines'),
                               hjust = 0.5,
                               nudge_y = 0.05) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Win percentage vs Net rating",
      x = "Net rating",
      y = "Win percentage",
      caption = "Data: Champion Data") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 1),
      plot.background = element_rect(colour = "black"))}


# Defensive Rating ----------------------------------------------------------------
# Best defences -----------------------------------------------------------


o_rtg_by_season %>% 
  ungroup() %>% 
  arrange(dRtg) %>% 
  head(10) %>% 
  left_join(player_positions_def) %>% 
  inner_join(tribble(~season, ~squadNickname, ~Outcome,
                     2009, "Thunderbirds", "Finished 3rd. Forced the 2nd most turnovers against in all seasons. Lost GF by 8.",
                     2009, "Vixens", "Finished 1st with 1 loss. 5th fewest goals against and the most turnovers against in all seasons. Won GF by 8 goals.",
                     2009, "WBOP Magic", "Finished 2nd. Fewest goals against of all seasons. Lost SF by 15.",
                     2009, "Tactix", "Finished 6th with a losing record.",
                     2010, "Swifts", "Finished 1st with 0 losses. Lost SF by 5. 3rd fewest goals and 3rd most turnovers against in all seasons.",
                     2010, "WBOP Magic", "Finished 3rd. Lost GF by 5.",
                     2010, "Thunderbirds", "Finished 2nd. Won GF by 5. 6th fewest goals against of all seasons.",
                     2017, "Lightning", "Finished 2nd with fewest goals against for the season. Won the GF by 17. Forced second most turnovers in all seasons.",
                     2017, "Magpies", "Finished 4th with second lowest goals against for the season. Lost SF by 1.",
                     2017, "Vixens", "Finished top of the ladder. Lost the prelim by 8."
                     )) %>% 
  transmute(Season = season,
            Team = squadNickname,
            `Defensive rating` = round(dRtg,digits = 1),
            Country = str_replace(country,"New Zealand","New\U2009Zealand"),
            Outcome,
            `Attacking players$^{1}$` = players) %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic(lightable_options = "striped") %>% 
  kableExtra::column_spec(column = 3:4, width = "100px") %>% 
  kableExtra::add_footnote(c("Player must have played at least 1/3 of all quarters one of GK, GD, WD, C"),notation = "number")



# Worst defences ----------------------------------------------------------

o_rtg_by_season %>% 
  ungroup() %>% 
  arrange(-dRtg) %>% 
  head(10) %>% 
  left_join(player_positions_def) %>%
  inner_join(tibble::tribble(
               ~season, ~squadNickname, ~Outcome,
                 2013L,      "Mystics", "Finished last with the second most goals against for the season.",
                 2014L,       "Tactix", "Finished last with 1 win. Their opponents score 171 more goals than them during the season.",
                 2014L,        "Steel", "Finished 5th with second most goals against in the season.",
                 2016L,        "Pulse", "Finished last in NZ conference. Outscored by 128 goals over the season.",
                 2019L,        "Fever", "Finished 6th with second most goals against for the season. 80 goals more than the next most.",
                 2019L,    "Firebirds", "Finished last with 1 win. Most goals against for the season.",
                 2020L,    "Firebirds", "Finished 5th with second highest goals against for the season.",
                 2020L,       "Swifts", "Fished 4th. Lost SF by 5.",
                 2020L,       "GIANTS", "Finished 6th.",
                 2021L,    "Lightning", "Finished 4th. Lost SF by 8."
               )) %>% 
  transmute(Season = season,
            Team = squadNickname,
            `Defensive rating` = round(dRtg,digits = 1),
            Country = str_replace(country,"New Zealand","New\U2009Zealand"),
            Outcome,
            `Defensive players` = players) %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic(lightable_options = "striped") %>% 
  kableExtra::column_spec(column = 3:4, width = "100px")


# Whole table - defence -------------------------------------------------------------

o_rtg_by_season %>% 
  arrange(dRtg) %>% 
  left_join(player_positions_def) %>%
  transmute(Rank = 1:n(),
            Season = season,
            Team = squadNickname,
            `Defensive rating` = round(dRtg,digits = 1),
            Country = str_replace(country,"New Zealand","New\U2009Zealand"),
            `Defensive players` = players) %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic(lightable_options = "striped") %>% 
  kableExtra::column_spec(column = 3:4, width = "100px") %>% 
  kableExtra::save_kable(file = "images/defensive_ratings.jpeg")

# The good and bad years --------------------------------------------------

o_rtg_by_season  %>% 
  ggplot(aes(y = season, x = dRtg)) +
  geom_point(aes(x = mdRtg,col = "Season average"),size = 3) +
  geom_point(aes(col = "Team rating")) +
  ggrepel::geom_text_repel(data = o_rtg_by_season %>%
                             arrange(-dRtg) %>%
                             head(10),
                           aes(label = squadNickname),
                           min.segment.length = unit(0, 'lines'),
                           hjust = 0.5) +
  ggrepel::geom_text_repel(data = o_rtg_by_season %>%
                             arrange(dRtg) %>%
                             head(10),
                           aes(label = squadNickname),
                           min.segment.length = unit(0, 'lines'),
                           hjust = 0.5) +
  scale_colour_manual(values = c("Season average" = "red", "Team rating" = "black")) +
  scale_y_continuous(breaks = 2009:2021) +
  scale_x_continuous(breaks = seq(50,80,2)) +
  labs(title = "Defensive rating trends",
       subtitle = "Changes in defensive ratings 2009 - 2021",
       x = "Defensive rating",
       y = "Season",
       caption = "Data: Champion Data",
       colour = "Defensive rating") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"))



# Season defensive rating -------------------------------------------------



o_rtg  %>% 
  transmute(round,match,opponent = squadNickname,dPoss = possessions,
            dGoals = goals,
            dTo = generalPlayTurnovers,
            dAttpt = goalAttempts
            ) %>% 
  full_join(o_rtg) %>% 
  filter(opponent != squadNickname) %>% 
  group_by(season, squadNickname) %>% 
  summarise(dPoss = sum(dPoss),
            dTo = sum(dTo),
            dAtt = sum(dAttpt),
            dG = sum(dGoals),
            dRtg = sum(dGoals)/sum(dPoss)*100,
            shtPct = dG/dAtt,
            toPct = dTo/dPoss) %>% View()
  ggplot(aes(dPoss,dG,group = paste(season,squadNickname))) + 
  geom_point(aes(col= squadNickname)) +
    coord_equal() %>% 
  plotly::ggplotly()


o_rtg_by_season %>%
select(season,squadNickname,oRtg,dRtg) %>%
pivot_longer(cols = c(oRtg,dRtg)) %>%
ggplot(aes(x = value, y = factor(season),fill = factor(squadNickname)), group = factor(squadNickname)) +
  geom_dotplot(binaxis = "y", stackdir = "up", stackgroups = TRUE,  method = "histodot") +
  facet_wrap(~name)


o_rtg %>% 
  group_by(season,squadId) %>% 
  summarise(across(c(goals,goalAttempts,generalPlayTurnovers),sum)) %>% 
  mutate(acc = goals/goalAttempts) %>% 
  ungroup() %>% 
  full_join(.,.,by = c("season", "squadId"),suffix = c("For","Against"))
  pivot_longer(-season) %>% 
  ggplot(aes(season,value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name,scales = "free")
  
  
o_rtg  %>% 
    transmute(round,match,opponent = squadNickname,dAtt = goalAttempts,dGoals = goals,dTo = generalPlayTurnovers) %>% 
    full_join(o_rtg) %>% 
    filter(opponent != squadNickname) %>% 
    group_by(season, squadNickname) %>% 
  summarise(across(c(goals,goalAttempts,generalPlayTurnovers,dGoals,dAtt,dTo),sum)) %>% 
  filter(season %in% 2021) %>% 
  mutate(acc = dGoals/dAtt) 
