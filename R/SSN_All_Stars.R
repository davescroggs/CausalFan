# SSN All Start Team

library(tidyverse)
library(superNetballR)
library(ggnewscale)

`2018_data` <- readRDS("data/2018_data.RDS")
`2019_data` <- readRDS("data/2019_data.RDS")
`2020_data` <- readRDS("data/2020_data.RDS")
`2021_data` <- readRDS("data/2021_data.RDS")

# Map squad colours
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

# Table of team IDs
team_info <- `2020_data` %>% 
  map_dfr(~.x[["teamInfo"]][["team"]]) %>% 
  distinct(squadNickname,squadId,squadName)

playerNames <- data %>% 
  map_dfr(~.x[["playerInfo"]][["player"]])
  select(playerId,displayName) %>% 
  distinct(playerId,.keep_all = T)

data <- map(2017:2021,~readRDS(paste("data/",.x,"_data.RDS",sep = ""))) %>% flatten()

all_scores <- data %>% 
  map_dfr(function(d){
    tibble(season = d[["matchInfo"]][["localStartTime"]] %>% 
             as.Date() %>% lubridate::year() %>% as.character(),
           round = d[["matchInfo"]][["roundNumber"]],
           match = d[["matchInfo"]][["matchNumber"]],
           scores = d[["playerPeriodStats"]])
  }) %>% unnest_longer(scores) %>% unnest_wider(scores) 


# Goalers -----------------------------------------------------------------

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

goalers <- all_scores %>%
  mutate(points = if_else(points == 0 | is.na(points),goals,points)) %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>% 
  group_by(season,squadNickname,playerId) %>% 
  summarise(goals = sum(points),
            minutesPlayed = sum(minutesPlayed),
            most_pos = Mode(startingPositionCode),
            feeds = sum(feeds),
            goalAssists = sum(goalAssists)) %>% 
  filter(goals > 0) %>% 
  arrange(season,squadNickname,-goals) %>% 
  inner_join(playerNames) %>% 
  group_by(season,squadNickname,displayName,most_pos) %>% 
  slice(1) %>% 
  filter(most_pos != "I") %>% 
  arrange(season,squadNickname,desc(most_pos)) %>% 
  group_by(season,squadNickname) %>% 
  summarise(goals = sum(goals),
            minutesPlayed = sum(minutesPlayed),
            players = paste(displayName,collapse = ", "),
            feeds = sum(feeds),
            goalAssists = sum(goalAssists)) %>% 
  ungroup() %>% 
  arrange(-goals)

goalers %>% 
  mutate(id = 1:n(),
         type = if_else(id < 6,paste(season,players, sep = " - "),"Other")) %>% 
  pivot_longer(cols = c(goals,goalAssists,feeds)) %>%
  ggplot(aes(x = value,fill = fct_reorder(type,id))) +
  geom_dotplot(binaxis = "x", stackgroups = TRUE, binwidth = 10,binpositions="all",method = "histodot",dotsize = 1.1) +
  facet_wrap(~name,scales = "free",ncol = 1) +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_x_continuous(breaks = seq(0,2000,25)) +
  scale_fill_manual(values = c("#377EB8", "#E41A1C", "#4DAF4A", "#984EA3", "#FF7F00","black")) +
  labs(x = "Stat count",
       title = "Shooting circle statistical leaders of SSN",
       subtitle = "Top 5 goal scoring circles and their important stats",
       fill = "Goal circle players and year") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1))


# Attacking mids ----------------------------------------------------------

# 2017 K Moloney & Liz Watson

mid_players <- all_scores %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>% 
  inner_join(playerNames) %>% 
  group_by(displayName) %>% 
  filter(any(startingPositionCode %in% c("WA","C"))) %>%
  group_by(season,squadNickname,displayName,startingPositionCode) %>% 
  summarise(minutesPlayed = sum(minutesPlayed,na.rm = T)) %>%
  ungroup() %>% 
  filter(startingPositionCode %in% c("WA","C")) %>% 
  arrange(season,squadNickname,-minutesPlayed) %>% 
  group_by(season,squadNickname,startingPositionCode) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(season,squadNickname,displayName)

mids <- all_scores %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>% 
  inner_join(playerNames) %>% 
  inner_join(mid_players) %>%
  mutate(deflections = if_else(is.na(deflections),deflectionWithNoGain + deflectionWithGain,deflections),
         turnovers = if_else(is.na(turnovers),missedGoalTurnover + generalPlayTurnovers,turnovers)) %>% 
  group_by(season,squadNickname,round,match) %>%
        summarise(players = list(unique(displayName)),
                  across(c(gain, feeds, turnovers),sum,na.rm = T)) %>% 
        ungroup() %>% 
  full_join(x = .,y = rename(.,opponent = squadNickname),by = c("season", "round", "match"),suffix = c("_For","_Against")) %>% 
    filter(squadNickname != opponent) %>% 
  group_by(season,squadNickname) %>%
  summarise(players = unique(unlist(players_For)) %>% paste(collapse = ", "),
            across(c(gain_For, feeds_For, turnovers_For,gain_Against, feeds_Against, turnovers_Against),sum,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(squad = paste(season,squadNickname),
         diff = feeds_For - feeds_Against,
         Difference = if_else(diff < 0,"Negative","Positive")) %>% 
  pivot_longer(cols = c(feeds_For, feeds_Against),
               names_to = c("Metric","Direction"),names_pattern = "(.*)_(.*)")
  
mids %>% 
ggplot(aes(x = value,y = fct_reorder(squad,diff))) +
  geom_point(aes(col = Direction)) +
  scale_color_manual("Feed differential", values = c("For" = "red","Against" = "black")) +
  new_scale_color() +
  geom_line(aes(col = Difference)) +
  scale_color_manual("Difference", values = c("Positive" = "green4","Negative" = "orange")) +
  labs(x = "Feeds",
       y = "",
       title = "Team Feed Differential",
       subtitle = "Difference between listed team feeds and their opponent") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1))

# Defence -----------------------------------------------------------------

all_results <- data %>% 
  map_dfr(function(d){
    tibble(season = d[["matchInfo"]][["localStartTime"]] %>% 
             as.Date() %>% lubridate::year() %>% as.character(),
           round = d[["matchInfo"]][["roundNumber"]],
           match = d[["matchInfo"]][["matchNumber"]],
           home_squad = d[["matchInfo"]][["homeSquadId"]],
           scores = d[["teamStats"]][["team"]])
  }) %>% unnest_wider(scores)

defence_data <- all_results %>% 
  mutate(points = if_else(points == 0 | is.na(points),goals,points)) %>% 
  inner_join(team_info) %>% 
  select(season,round,match,squadNickname,points) %>% 
  full_join(x = .,y = .,by = c("season","round","match")) %>% 
  filter(squadNickname.x != squadNickname.y) %>% 
  rename(pointsFor = points.x,pointsAgainst = points.y) %>% 
  group_by(squadNickname.x,season) %>%
  summarise(meanFor = mean(pointsFor),
            meanAgainst = mean(pointsAgainst)) %>% 
  group_by(season) %>% 
  mutate(seasonAve = mean(meanFor),
         goalDiffFor = meanFor - seasonAve,
         goalDiffAgainst = meanAgainst - seasonAve,
         squad = paste(season,squadNickname.x,sep = " - "),
         Difference = if_else(goalDiffAgainst < 0,"Negative","Positive")) %>% 
  ungroup() %>% 
  arrange(goalDiffAgainst)
  
defence_data %>% 
pivot_longer(cols = c(meanAgainst,seasonAve),names_to = "Goal differential") %>%
  ggplot(aes(x = value,y = fct_reorder(squad,desc(goalDiffAgainst)))) +
  geom_point(aes(col = `Goal differential`)) +
  scale_color_manual("Goal differential", values = c("meanAgainst" = "red","seasonAve" = "black")) +
  new_scale_color() +
  geom_line(aes(col = Difference)) +
  scale_color_manual("Difference", values = c("Positive" = "green4","Negative" = "orange")) +
  labs(x = "Goals",
       y = "Team and season",
       title = "Goal Differential",
       subtitle = "Difference between listed team goals and their relative opponents") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1))


d_players <- all_scores %>% 
  left_join(team_info %>% select(squadId,squadNickname)) %>% 
  inner_join(playerNames) %>% 
  group_by(displayName) %>% 
  filter(any(startingPositionCode %in% c("GK","GD","WD"))) %>%
  group_by(season,squadNickname,displayName,startingPositionCode) %>% 
  summarise(minutesPlayed = sum(minutesPlayed,na.rm = T)) %>%
  ungroup() %>% 
  filter(startingPositionCode %in% c("GK","GD","WD")) %>% 
  arrange(season,squadNickname,-minutesPlayed) %>% 
  group_by(season,squadNickname,startingPositionCode) %>% 
  slice(1) %>% 
  ungroup() %>% 
  inner_join(defence_data %>%
               head(5) %>% 
               transmute(season,squadNickname = squadNickname.x)) %>% 
  select(season,squadNickname,displayName)

all_results %>% 
  mutate(turnovers = if_else(is.na(turnovers),missedGoalTurnover + generalPlayTurnovers,turnovers)) %>%
  inner_join(team_info) %>% 
  full_join(all_results %>% 
              inner_join(team_info) %>% 
              transmute(season,round,match,opponent = squadNickname),by = c("season","round","match")) %>% 
  filter(squadNickname != opponent,season != 2021) %>% 
  group_by(season,opponent) %>% 
  summarise(across(c(feeds, turnovers, penalties,possessions,goalMisses,goalAttempts),sum,na.rm = T)) %>% 
  left_join(d_players %>% 
  distinct(season,squadNickname) %>% 
  transmute(opponent = squadNickname,season,Team = T)) %>% 
  mutate(Team = if_else(is.na(Team),"Other",paste(season, opponent))) %>% 
  ungroup() %>%
  pivot_longer(cols = c(feeds, turnovers, penalties,possessions,goalMisses,goalAttempts)) %>%
  ggplot(aes(x = value,fill = Team)) +
  geom_dotplot(binaxis = "x", stackgroups = TRUE,binpositions="all",method = "histodot",dotsize = 1.1) +
  facet_wrap(~name,scales = "free",ncol = 2) +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_x_continuous(n.breaks = 8) +
  scale_fill_manual(values = c("#377EB8", "#E41A1C", "#4DAF4A", "#984EA3", "#FF7F00","black")) +
  labs(x = "Stat count",
       title = "Opponent team attacking statistics",
       subtitle = "Opposition stat totals when playing the listed team",
       fill = "Defensive teaam") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1))


# Rookie ------------------------------------------------------------------

all_scores %>% 
  group_by(playerId) %>% 
  filter(!any(season %in% 2017:2019)) %>% 
  group_by(season,playerId) %>% 
  summarise(across(where(is.numeric),sum,na.rm = T)) %>% 
  inner_join(playerNames) %>% 
  mutate(nnppm = netPoints/minutesPlayed) %>% 
  select(season,displayName,minutesPlayed,nnppm, everything()) %>% View()

2021 J.Ward 1011747
2021 S.Dwyer 1019169
  


# 8th woman ---------------------------------------------------------------

`8th_woman` <- all_scores %>% 
  inner_join(mostCommonPos %>% filter(mostCommonPos == "I")) %>% 
  inner_join(playerNames) %>% 
  group_by(season,displayName) %>% 
  summarise(minutesPlayed = sum(minutesPlayed)) %>%
  filter(minutesPlayed > 100) %>% 
  select(-minutesPlayed) %>% 
  ungroup()

# 2020 Sophie Garbin

all_scores %>% 
  inner_join(playerNames) %>%
  inner_join(team_info) %>% 
  inner_join(mostCommonPos) %>% 
  mutate(points = if_else(points == 0 | is.na(points),goals,points)) %>% 
  filter(season == 2020,squadNickname == "Swifts",points > 0) %>% 
  group_by(season,displayName) %>% 
  summarise(across(where(is.numeric),sum,na.rm = T),.groups = "keep") %>% 
  transmute(points,minutesPlayed,attempts1,attempts2,
    gpm = points/minutesPlayed,
            gapm = goalAttempts/minutesPlayed,
            goalpct = (goal1 + 2*goal2)/(attempts1 + attempts2))

# 3 Stat leaders ----------------------------------------------------------


mostCommonPos <- all_scores %>% 
    group_by(season,playerId,startingPositionCode) %>% 
    summarise(mp = sum(minutesPlayed),.groups = "keep") %>% 
    arrange(season,playerId,-mp) %>%
    group_by(season,playerId) %>% 
    slice(1) %>% 
    transmute(season,playerId,mostCommonPos = startingPositionCode) %>% 
    ungroup()

  min_max <- function(x) x/max(x)
  
knn <- all_scores %>% 
  mutate(points = if_else(points == 0 | is.na(points),goals,points)) %>%
  inner_join(mostCommonPos) %>%
  filter(mostCommonPos == "GD") %>% 
  inner_join(playerNames) %>% 
  #filter(!mostCommonPos %in% c("-","I")) %>%
  group_by(season,displayName) %>% 
  summarise(across(c(gain, turnovers, deflections, intercepts, penalties,rebounds,minutesPlayed),sum,na.rm = T),.groups = "drop")
  mutate(across(c(gain, turnovers, deflections, intercepts, penalties,rebounds),~.x/minutesPlayed)) %>%
  filter(minutesPlayed > 200) %>% 
  ungroup()

model <- knn %>% 
  select(-c(minutesPlayed,season,displayName)) %>%
  as.matrix() %>%  
  dbscan::kNN(k = 3)

kNN_dist = model$dist  %>%
  as_tibble() %>% 
  mutate(rowMean = {rowMeans(.)}) %>% 
  rowid_to_column()


bind_cols(knn,kNN_dist %>% select(rowMean,rowid)) %>% View()
model[["id"]]
# 2019 Gretel Tippet

all_scores %>% 
  inner_join(playerNames) %>%
  mutate(points = if_else(points == 0 | is.na(points),goals,points),
         deflections = if_else(is.na(deflections),deflectionWithNoGain + deflectionWithGain,deflections),
         turnovers = if_else(is.na(turnovers),missedGoalTurnover + generalPlayTurnovers,turnovers)) %>%
  # inner_join(mostCommonPos) %>%
  # filter(mostCommonPos == "C") %>%
  group_by(season,displayName) %>% 
  summarise(across(c(feeds,gain,turnovers,minutesPlayed,deflections,intercepts),sum),.groups = "drop") %>% 
  mutate(across(c(feeds,gain,turnovers,minutesPlayed,deflections,intercepts),~dense_rank(desc(.)))) %>% View()
