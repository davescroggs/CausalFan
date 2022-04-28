library(tidyverse)
library(superNetballR)

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
  "Fever"	        , "#00953b",	"#000000")

squadPrimaryCols <- squadCols %>%   
  select(-Secondary) %>% 
  deframe()

# Table of team IDs
team_info <- `2020_data` %>% 
  map_dfr(~.x[["teamInfo"]][["team"]]) %>% 
  distinct(squadNickname,squadId,squadName)

playerNames <- data %>% 
  map_dfr(~.x[["playerInfo"]][["player"]]) %>% 
  select(playerId,displayName) %>% 
  distinct(playerId,.keep_all = T)

data <- map(2018:2021,~readRDS(paste("data/",.x,"_data.RDS",sep = ""))) %>% 
  flatten()

reg_all_results <- tibble(round = data %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
       match = data %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
       season = data %>% map_chr(~.x[["matchInfo"]][["localStartTime"]]) %>% 
                     as.Date() %>% lubridate::year() %>% as.character(),
       home_squad = data %>% map_int(~.x[["matchInfo"]][["homeSquadId"]]),
       subs = data %>% map(~ .x[["teamStats"]][["team"]])) %>%
  unnest_longer("subs") %>% unnest_wider("subs") %>%
  left_join(data %>%
              map_dfr( ~ .x[["teamInfo"]][["team"]]) %>% distinct(squadNickname, squadId),
            by = "squadId") %>% 
  ungroup() %>% 
  mutate(points = if_else(points == 0 | is.na(points),goals,points),
         deflections = if_else(is.na(deflections),deflectionWithNoGain + deflectionWithGain,deflections))

finals_data <- map(2018:2021,~readRDS(paste("data/",.x,"_finals_data.RDS",sep = ""))) %>% 
  flatten()

fin_all_results <- tibble(round = finals_data %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
                          match = finals_data %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
                          season = finals_data %>% map_chr(~.x[["matchInfo"]][["localStartTime"]]) %>% 
                            as.Date() %>% lubridate::year() %>% as.character(),
                          home_squad = finals_data %>% map_int(~.x[["matchInfo"]][["homeSquadId"]]),
                          subs = finals_data %>% map(~ .x[["teamStats"]][["team"]])) %>%
  unnest_longer("subs") %>% unnest_wider("subs") %>%
  left_join(finals_data %>%
              map_dfr( ~ .x[["teamInfo"]][["team"]]) %>% distinct(squadNickname, squadId),
            by = "squadId") %>% 
  ungroup() %>% 
  mutate(points = if_else(points == 0 | is.na(points),goals,points),
         deflections = if_else(is.na(deflections),deflectionWithNoGain + deflectionWithGain,deflections))


all_results %>% 
  select(season,round,match,squadNickname,points) %>% 
  full_join(x = .,y = .,by = c("round","match","season")) %>% 
  filter(squadNickname.x != squadNickname.y) %>%
  rename(goalsFor = points.x,goalsAgainst = points.y) %>% 
  ggplot(aes(x = goalsFor, y = goalsAgainst,col = squadNickname.y)) +
  geom_vline(aes(xintercept = mean(goalsFor))) +
  geom_hline(aes(yintercept = mean(goalsAgainst))) +
  geom_point(size = 3) +
  scale_colour_manual(values = squadPrimaryCols) +
  facet_wrap(~squadNickname.x)

all_results %>% 
  select(season,round,match,squadNickname,points) %>% 
  full_join(x = .,y = .,by = c("round","match","season")) %>% 
  filter(squadNickname.x != squadNickname.y) %>%
  rename(goalsFor = points.x,goalsAgainst = points.y) %>%
  group_by(squadNickname.x,season) %>% 
  mutate(meanFor = mean(goalsFor),
         meanAgainst = mean(goalsAgainst),
         goalsFor = goalsFor - meanFor,
         goalsAgainst = goalsAgainst - meanAgainst) %>% arrange(squadNickname.x) %>% 
  ungroup() %>%
  ggplot(aes(x = goalsFor, y = goalsAgainst,col = squadNickname.y)) +
  #geom_vline(aes(xintercept = meanFor)) +
  #geom_hline(aes(yintercept = meanAgainst)) +
  geom_point(size = 3) +
  scale_colour_manual(values = squadPrimaryCols) +
  facet_wrap(~squadNickname.x)
  


# 2018 Team ---------------------------------------------------------------

# What were their traits?
# - High scoring?
# Large difference between goals against when home vs away
# Beat
all_results %>%
  #filter(season == 2018) %>% 
  inner_join(team_info %>% rename(homeName = squadNickname),by = c("home_squad" = "squadId")) %>% 
  select(season,round,match,squadNickname,points,homeName) %>% 
  full_join(x = .,y = .,by = c("round","match","season","homeName")) %>% 
  filter(squadNickname.x != squadNickname.y) %>%
  rename(goalsFor = points.x,goalsAgainst = points.y) %>% 
  mutate(homeName = if_else(homeName == squadNickname.x,"Home","Away")) %>% 
  group_by(season,squadNickname.x,homeName) %>% 
  summarise(aveGoalsFor = mean(goalsFor),
            aveGoalsAgainst = mean(goalsAgainst),.groups = "drop") %>% 
  arrange(aveGoalsFor) %>%
  pivot_longer(cols = c(aveGoalsFor,aveGoalsAgainst)) %>% 
  ggplot(aes(y = fct_inorder(squadNickname.x))) +
  geom_point(aes(x = value,col = homeName)) +
  scale_color_manual(values = c("Away" = "red","Home" = "black")) +
  facet_grid(season~name)


  fin <- fin_all_results %>% 
  inner_join(team_info) %>% 
  filter(season == 2018) %>% 
  group_by(round,match) %>% 
  filter(any(squadNickname == "Fever")) %>% 
  ungroup() %>% 
  select(season,round,match,squadNickname,points) %>% 
  full_join(x = .,y = .,by = c("round","match","season")) %>% 
  filter(squadNickname.x != squadNickname.y,squadNickname.x == "Fever") %>%
  rename(goalsFor = points.x,goalsAgainst = points.y) %>% 
    mutate(game = if_else(round == 17,"GF","SF"))

  
  all_results %>% 
    select(season,round,match,squadNickname,points) %>% 
    full_join(x = .,y = .,by = c("round","match","season")) %>% 
    filter(squadNickname.x != squadNickname.y,squadNickname.x == "Fever",season == 2018) %>%
    rename(goalsFor = points.x,goalsAgainst = points.y) %>%
    mutate(`Finals Team` = if_else(squadNickname.y %in% c("GIANTS","Lightning"),"Yes","No")) %>% 
    ggplot(aes(x = goalsFor, y = goalsAgainst,col = squadNickname.y)) +
    geom_vline(aes(xintercept = mean(goalsFor))) +
    geom_hline(aes(yintercept = mean(goalsAgainst))) +
    geom_point(aes(size = `Finals Team`,shape = `Finals Team`)) +
    geom_abline(slope = 1, linetype = "dashed") +
    #geom_point(data = fin,col = "red",size = 5,shape = 4) +
    geom_text(data = fin,aes(label = game),show.legend = F) +
    scale_size_manual(values = c("Yes" = 5,"No" = 3)) +
    scale_shape_manual(values = c("Yes" = 17,"No" = 19)) +
    scale_colour_manual(values = squadPrimaryCols) +
    coord_equal()
  