library(tidyverse)
library(lubridate)
library(ggnewscale)
library(geosphere)

# Contents
# 1. Overall schedule
# 2. Home games, and away games that are actually home games
# 3. Time between games
# 4. Distance travelled


`2022_data` <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/R/R Play/CausalFan/data/2022_data.RDS")

venues <- tibble::tribble(
                                     ~venueName,        ~lat,        ~long, ~state,
                       "Netball SA Stadium",  -34.9325702, 138.5788922, "SA",
                       "Ken Rosewall Arena", -33.85377795, 151.0717874, "NSW",
                             "Nissan Arena",  -27.5596044,  153.065137, "QLD",
                                "RAC Arena",  -31.9482246, 115.8524104, "WA",
                              "USC Stadium", -26.71647928, 153.0685959, "QLD",
                          "John Cain Arena", -37.82221266, 144.9820402, "VIC",
                          "To Be Confirmed",           NA,          NA, NA_character_,
             "Derwent Entertainment Centre",  -42.8241342, 147.2832265, "TAS",
                               "Silverdome", -41.47509749, 147.1402037, "TAS",
                     "State Netball Centre",  -27.5594352, 153.0651261, "VIC",
            "Adelaide Entertainment Centre", -34.90780061, 138.5737468, "SA"
            )

team_info <- tibble::tribble(
  ~squadNickname, ~squadId, ~homeState,
  "Thunderbirds",     801L, "SA",
       "Magpies",    8119L, "VIC",
        "Swifts",     806L, "NSW",
        "GIANTS",    8118L, "NSW",
     "Firebirds",     807L, "QLD",
        "Vixens",     804L, "VIC",
         "Fever",     810L, "WA",
     "Lightning",    8117L, "QLD"
  )


schedule <- `2022_data` %>% 
  map_dfr(~.x[["matchInfo"]]) %>% 
  select(homeSquadId,venueName,roundNumber,localStartTime,matchNumber,awaySquadId) %>% 
  mutate(localStartTime = as_datetime(localStartTime) %>% as_date()) %>% 
  inner_join(venues) 

start_date = min(schedule$localStartTime %>% as.Date())
end_date = max(schedule$localStartTime %>% as.Date()) - days(2)

date_breaks = seq(start_date,end_date,by = "week")

home_away <- bind_rows(schedule %>% 
            inner_join(team_info %>% transmute(homeSquadId = squadId,squadNickname)) %>% 
            transmute(squadNickname,roundNumber,matchNumber,venueName,lat,long,localStartTime,`Home Team` = "Home"),
          schedule %>% 
            inner_join(team_info %>% transmute(awaySquadId = squadId,squadNickname)) %>% 
            transmute(squadNickname,roundNumber,matchNumber,venueName,lat,long,localStartTime,`Home Team` = "Away")) %>% 
  arrange(squadNickname,roundNumber) %>% 
  group_by(squadNickname) %>% 
  mutate(time_between = (lead(localStartTime) - localStartTime) %>% factor(levels = as.character(2:9),ordered = T),
         distance = pmap_dbl(list(long,lat,lag(long),lag(lat)),function(x,y,a,b) distm(c(x,y),c(a,b)))/1000) %>% 
  ungroup()
  

# Overall schedule --------------------------------------------------------

home_away %>% 
  rename(`Days between games` = time_between) %>% 
ggplot(aes(x = localStartTime,y = squadNickname,group = squadNickname)) +
  geom_line(aes(col = `Days between games`),size = 2) +
  scale_colour_brewer("Days between games",palette = "RdYlGn") +
  new_scale_color() +
  geom_point(aes(col = `Home Team`),size = 4.5) +
  scale_colour_manual("Home Team",values = c("Home" = "black","Away" = "red")) +
  theme_bw() +
  scale_x_date(breaks = date_breaks,
               date_labels = "%b-%d") +
  labs(x = "Game Date",
       y = "",
       title = "SSN 2022 Schedule",
       subtitle = "Breakdown of game times and home games for the upcoming season",
       caption  = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"))


# Distance travelled ------------------------------------------------------

home_away %>% 
  group_by(squadNickname) %>% 
  summarise(dist = sum(distance,na.rm = T))



# Home games, and away games that are actually home games -----------------

# Magpies have two home games in Tasmania

home_away %>% 
  inner_join(venues %>% select(venueName,state)) %>%
  inner_join(team_info) %>% 
  mutate(AwayIsHome = state == homeState) %>% 
  count(squadNickname,`Home Team`,AwayIsHome) %>% 
  pivot_wider(names_from = `Home Team`, values_from = n,values_fill = list(n = 0)) %>% View()

home_away %>% 
  inner_join(venues %>% select(venueName,state)) %>%
  inner_join(team_info) %>% 
  mutate(AwayIsHome = state == homeState) %>% 
  count(squadNickname,AwayIsHome) %>% 
  filter(AwayIsHome) %>%
  arrange(-n)

# Time between games ------------------------------------------------------

home_away %>% 
  filter(roundNumber != 14) %>% 
  group_by(squadNickname) %>% 
  mutate(time_between =  lag(time_between)) %>% 
  filter(!is.na(time_between)) %>% 
  count(squadNickname,time_between) %>% 
  ggplot(aes(x = n, y = time_between)) +
  geom_col() +
  facet_wrap(~squadNickname)

home_away %>% 
  filter(roundNumber != 14) %>% 
  mutate(short_gap = as.integer(time_between) < 4) %>% 
  count(squadNickname,short_gap) %>% 
  ggplot(aes(x = n, y = squadNickname,col = short_gap)) +
  geom_col()

# Match up ----------------------------------------------------------------

# Vixens have the Giants away, Fever in the West with a short turn-around, followed by the Swifts at home.
# Vixens have both their short round games against the Fever
# Firebirds have a tough run home
# Fever have 3 away games, with the 3rd being the first mid-week game.
# Magpies have 4 away games in a row, the last being in Perth.

get_ladders <- function(dat){
  
  season <- lubridate::year(dat[[1]][["matchInfo"]][["utcStartTime"]]) %>% as.integer()
  
  dat %>%
    map_dfr(tidyMatch) %>% 
    ladders() %>% 
    {if (season %in% 2018:2019) {
      arrange(., -points_new,-(goals_for/goals_against))
    }else {
      arrange(., -points,-(goals_for/goals_against))}} %>% 
    #{if (season %in% 2018:2019) . else .} %>% 
    left_join(team_info) %>% 
    transmute(pos = 1:n(), squadId,season = season)
  
}
`2021_data` <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/R/R Play/CausalFan/data/2021_data.RDS")

get_ladders(`2021_data`)

home_away  %>% 
  inner_join(home_away %>% transmute(away_team = squadNickname,roundNumber,matchNumber)) %>% 
  filter(squadNickname != away_team) %>% 
  inner_join(tibble(away_team = c("Fever","Swifts","Lightning","GIANTS","Firebirds","Magpies","Thunderbirds","Vixens"),
pos = 1:8)) %>% 
  ggplot(aes(x = roundNumber,y = pos,group = squadNickname)) +
  geom_line(col = "grey60") +
  geom_vline(xintercept = c(4,10)) +
  geom_text(aes(col = `Home Team`,label = away_team)) +
  scale_colour_manual("Home Team",values = c("Home" = "black","Away" = "red")) +
  scale_y_reverse() +
  facet_wrap(~squadNickname) +
  theme_bw()
