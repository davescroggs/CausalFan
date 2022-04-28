library(tidyverse)
library(superNetballR)
library(magrittr)
library(geomtextpath)
library(gt)

diamonds_cd_lookup <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/R/R Play/CausalFan/data/diamonds_cd_lookup.RDS")

# diamonds_series <- diamonds_cd_lookup %>%
#   mutate(game_files =   pmap(list(code, rounds, matches), function(c, r, m) {
#     list(roundNumber = 1:r, matchNumber = 1:m) %>%
#       cross_df() %$%
#       map2(roundNumber, matchNumber,  ~ downloadMatch(c, .x, .y))
#   }))

diamonds_series <- 
  readRDS(file = "data/diamonds_series.RDS")

extracted_metrics <- diamonds_series %>% 
  select(series,game_files) %>% 
  unnest(game_files) %>%
  mutate(scores = game_files %>%
           map(~ .x[["scoreFlow"]][["score"]]),
         team_stats = game_files %>%
           map(~ .x[["teamStats"]][["team"]]),
         player_stats = game_files %>%
           map(~ .x[["playerPeriodStats"]][["player"]]),
         teamPeriodStats = game_files %>%
           map(~ .x[["teamPeriodStats"]][["team"]]),
         team_info = game_files %>%
           map(~ .x[["teamInfo"]][["team"]]),
         player_info = game_files %>%
           map(~ .x[["playerInfo"]][["player"]]),
         round = game_files %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
         match = game_files %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
         year = game_files %>% map_chr(~.x[["matchInfo"]][["localStartTime"]]) %>% 
           as.Date() %>% lubridate::year()) %>%
  select(-game_files)

team_info <- extracted_metrics %>% 
  select(team_info) %>% 
  unnest_longer(team_info) %>%
  unnest_wider(team_info) %>% 
  group_by(squadId) %>% 
  slice(1) %>% 
  ungroup()

player_info <- extracted_metrics %>% 
  select(player_info) %>% 
  unnest_longer(player_info) %>%
  unnest_wider(player_info) %>% 
  group_by(playerId) %>% 
  slice(1) %>% 
  ungroup()

unnest_specific <- function(df,tbl){
  df %>% 
    select(series, year, round, match, {{tbl}}) %>% 
    unnest_longer({{tbl}}) %>%
    unnest_wider({{tbl}})
}

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

# Score line --------------------------------------------------------------

extracted_metrics %>% 
  filter(series == "2022 January Netball Quad Series",round == 4,match == 2) %>% 
  unnest_specific(teamPeriodStats) %>% 
  inner_join(team_info %>% select(squadId,squadNickname)) %>% 
  select(Team = squadNickname,period,goals) %>% 
  pivot_wider(names_from = period,values_from = goals) %>% 
  janitor::adorn_totals(where = "col") %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic()

extracted_metrics %>% 
  filter(series == "2022 January Netball Quad Series",round == 4,match == 2) %>% 
  unnest_specific(teamPeriodStats) %>% 
  inner_join(team_info %>% select(squadId,squadNickname)) %>% 
  select(Team = squadNickname,period,feeds) %>% 
  pivot_wider(names_from = period,values_from = feeds) %>% 
  janitor::adorn_totals(where = "col") %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic()


# Shooters Stats ----------------------------------------------------------

player_stats %>% 
  filter(series == "2022 January Netball Quad Series",round == 4,match == 2,goalAttempts > 0) %>% 
  inner_join(team_info %>% select(squadId,squadNickname), by = "squadId") %>% 
  inner_join(player_info, by = "playerId") %>% 
  transmute(Team = squadNickname,
            Player = surname,
            period = as.character(period),
            goals = paste(goals,goalAttempts,sep = "/")) %>% 
  bind_rows(player_stats %>% 
              filter(series == "2022 January Netball Quad Series",round == 4,match == 2,goalAttempts > 0) %>% 
              inner_join(player_info, by = "playerId") %>% 
              inner_join(team_info %>% select(squadId,squadNickname), by = "squadId") %>%
              group_by(squadNickname,surname) %>% 
              summarise(across(c(goals,goalAttempts),sum),.groups = "drop") %>% 
              transmute(Team = squadNickname,
                        Player = surname,
                        period = "Total",
                        goals = paste(goals,goalAttempts,sep = "/"))) %>% 
  pivot_wider(names_from = period,values_from = goals,values_fill = list(goals = "")) %>%
  arrange(Team) %>% 
  group_by(Team) %>% 
  gt() %>% 
  tab_spanner(
    label = "Quarter",
    columns = c(`1`,`2`,`3`,`4`)
  ) %>% 
  tab_header(title = md("**Individual shooting performances**")) 


# Offensive Efficiency ----------------------------------------------------

plot_labs <- player_stats %>% 
  distinct(series,round,match,squadId) %>% 
  full_join(.,.,by = c("series","round","match")) %>% 
  filter(squadId.x != squadId.y) %>% 
  rename("squadId" = "squadId.x", "opponent" = "squadId.y") %>% 
  left_join(team_info %>% select(squadId,squadCode)) %>% 
  left_join(team_info %>% transmute(opponent = squadId,opponentName = squadCode)) %>% 
  mutate(s = str_remove(series," NETBALL| Netball") %>% stringr::str_to_title(),
         game_label = paste(s," ",squadCode," v ",opponentName," R",round,sep = "")) %>% 
  slice(2, 15, 75) %>% 
  select(series,round,match,squadId,game_label)

player_stats %>% 
  group_by(playerId) %>% 
  filter(any(goalAttempts > 0)) %>% 
  ungroup() %>% 
  count(series,year,round,match,squadId, wt = rebounds,name = "o_boards") %>%
  left_join(team_stats) %>% 
  left_join(team_info) %>%
  mutate(EffectiveAttempts = goalAttempts - o_boards) %>%
  {ggplot(data = .,aes(EffectiveAttempts, goals, col = squadName)) +
  geom_textabline(slope = 1,label = "100% shot conversion",hjust = 0.2) +
  geom_point() +
      ggrepel::geom_text_repel(data = inner_join(.,plot_labs),aes(label = game_label),
                               nudge_x = -15,nudge_y = 3,family = "Courier", box.padding = 1,
                               show.legend = F) +
    scale_colour_manual(values = c("Silver Ferns" = "#000000","SPAR Proteas" = "#FCD270","Vitality Roses" = "#E41D36","Origin Diamonds" = "#3C8975")) +
  scale_x_continuous(minor_breaks = seq(0,100,1),breaks = seq(0,100,5),limits = c(30,80)) +
  scale_y_continuous(minor_breaks = seq(0,100,1),breaks = seq(0,100,5),limits = c(30,80)) +
  coord_equal() +
    theme_bw() +
  labs(x = "Effective attempts",
       y = "Goals",
       col = "Squad",
       title = "Shoot circle efficiency",
      subtitle = "Effective shooting efficiency - offensive rebounds offset misses",
      caption = "Data: Champion Data") +
    theme(plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 1),
      plot.background = element_rect(colour = "black"))}
  
# Quad Series Final 2022 --------------------------------------------------

QSF <- extracted_metrics %>% 
    filter(series == "2022 January Netball Quad Series",round == 4,match == 2) %>% 
    unnest_specific(teamPeriodStats) %>% 
    inner_join(team_info %>% select(squadId,squadNickname))

  QSF %>% 
    select(period,squadNickname,goals,goalAttempts,goalAssists,goalMisses,feeds,feedWithAttempt) %>%
    pivot_longer(cols = -c(period,squadNickname),names_to = "metric") %>% 
    #mutate(type = if_else(str_detect(metric,"goal"),"Goal","Feed")) %>% 
    ggplot(aes(x = value,y = squadNickname,col = squadNickname)) +
    geom_jitter(width = 0,aes(col = factor(period)),size = 3) +
    scale_color_brewer(palette = "Spectral") +
    scale_x_continuous(breaks = seq(0,100,1)) +
    facet_wrap(~metric,scales = "free",ncol = 2)
  
  extracted_metrics %>%
    unnest_specific(teamPeriodStats) %>% 
    inner_join(team_info %>% select(squadId,squadNickname)) %>% 
    filter(feedWithAttempt != 0) %>% 
    add_count(feeds,feedWithAttempt) %>% 
    {ggplot(data = .,aes(x = feeds,feedWithAttempt)) +
    geom_point(col = "grey",aes(size = n)) +
        geom_point(data = filter(.,series == "2022 January Netball Quad Series",round == 4,match == 2),
                   size = 3,aes(col = squadNickname)) +
        geom_text(data = filter(.,series == "2022 January Netball Quad Series",round == 4,match == 2),
                   size = 3,aes(label = period)) +
      geom_smooth(method = "lm",se = T) +
        scale_x_continuous(minor_breaks = seq(0,100,1)) +
        scale_y_continuous(minor_breaks = seq(0,100,1)) +
        theme_bw()}
  
  extracted_metrics %>%
    unnest_specific(teamPeriodStats) %>% 
    inner_join(team_info %>% select(squadId,squadNickname)) %>% 
    filter(feeds != 0) %>% 
    add_count(goals,feeds) %>% 
    {ggplot(data = .,aes(feeds, goals)) +
        geom_point(col = "grey",aes(size = n)) +
        geom_point(data = filter(.,series == "2022 January Netball Quad Series",round == 4,match == 2),
                   size = 3,aes(col = squadNickname)) +
        geom_text(data = filter(.,series == "2022 January Netball Quad Series",round == 4,match == 2),
                  size = 3,aes(label = period), col = "white") +
        #geom_smooth(method = "lm",se = T) +
        geom_smooth(method = "lm",se = F,formula = y ~ x + 0,col = "dodgerblue") +
        geom_smooth(method = "lm",se = F,col = "dodgerblue",aes(col = sqaudNickname)) +
        scale_x_continuous(minor_breaks = seq(0,100,1),breaks = seq(0,100,5)) +
        scale_y_continuous(minor_breaks = seq(0,100,1),breaks = seq(0,100,5)) +
        scale_colour_manual(values = c("Vitality Roses" = "#E41D36","Origin Diamonds" = "#3C8975")) +
        scale_size(limits = c(1,15),breaks = c(1,3,seq(5,15,5)),range = c(1,5)) +
        annotate("text",x = 10.5,y = 4,label = "Slope: 0.69 goals per feed",hjust = 0,vjust = 0.75) +
        geom_segment(
          aes(x = 10, y = 4, xend = 7.9, yend = 5.1),
          arrow = arrow(length = unit(0.01, "npc"))) +
        theme_bw() +
        coord_equal() +
        labs(title = "Feed conversion",
          subtitle = "Relationship between feeds and goals scored",
          caption = "Data: Champion Data",
          size = "Number of occurances",
          x = "Feeds",
          y = "Goals",
          col = "Squad") +
        theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 1),
          plot.background = element_rect(colour = "black"))}


  extracted_metrics %>%
    unnest_specific(teamPeriodStats) %>% 
    filter(feeds != 0) %>% 
    inner_join(team_info %>% select(squadId,squadNickname)) %>% 
    {lm(data = .,formula = goals ~ feeds:squadNickname + 0)} %>% 
    summary()

  QSF %>% 
    arrange(squadNickname,period) %>% 
    group_by(squadNickname) %>% 
    transmute(squadNickname, period, score = cumsum(goals)) %>% 
    pivot_wider(names_from = period,values_from = score)
  
  
# 3 in 4 times, the team that feeds more, win the game. More feeds = more opportunities = more goals  
  extracted_metrics %>%
    unnest_specific(teamPeriodStats) %>%
    #group_by(series,round,match,squadId,period) %>% 
    #summarise(across(c(goals,feeds,goalAttempts),sum)) %>% 
    filter(feeds != 0) %>% 
    group_by(series,round,match,period) %>% 
    transmute(goals,
              min_score = min(goals),
           goals_winner = if_else(min_score == mean(goals),"Draw",map2_chr(goals,min_score,~if_else(.x > .y,"Winner","Loser"))),
           feeds,
           min_feeds = min(feeds),
           feeds_winner = if_else(min_feeds == mean(feeds),"Draw",map2_chr(feeds,min_feeds,~if_else(.x > .y,"Winner","Loser")))) %>% 
    ungroup() %>% 
    count(goals_winner, feeds_winner) %>% 
    pivot_wider(names_from = feeds_winner,values_from = n,values_fill = list(n = 0)) %>% 
    janitor::adorn_totals(where = c("row","col")) %>% 
    rename("Goals winner" = "goals_winner") %>% 
    gt() %>% 
    tab_spanner(
      label = "Feeds winner",
      columns = c(Draw,Loser,Winner,Total)
    ) 
  
  
  team_stats %>% 
    arrange(-goals) %>% 
    head(1) %>% 
    transmute(goals,goalMisses,er = goalMisses*missedShotConversion/100)
  