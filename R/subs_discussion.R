library(tidyverse)
library(superNetballR)


`2017_data` <- readRDS("data/2017_data.RDS")
`2018_data` <- readRDS("data/2018_data.RDS")
`2019_data` <- readRDS("data/2019_data.RDS")
`2020_data` <- readRDS("data/2020_data.RDS")
`2021_data` <- readRDS("data/2021_data.RDS")

data <- c(`2021_data`,`2020_data`,`2019_data`,`2018_data`,`2017_data`) %>% 
  keep(~lengths(.x[["playerSubs"]]) > 1) 
all_subs <- tibble(subs = data %>%
         map(~ .x[["playerSubs"]][["player"]]),
       round = data %>% map_int(~.x[["matchInfo"]][["roundNumber"]]),
       match = data %>% map_int(~.x[["matchInfo"]][["matchNumber"]]),
       season = data %>% map_chr(~.x[["matchInfo"]][["localStartTime"]]) %>% 
         as.Date() %>% lubridate::year() %>% as.character()) %>%
  unnest_longer("subs") %>% unnest_wider("subs") %>%
  left_join(data %>%
              map_dfr( ~ .x[["teamInfo"]][["team"]]) %>% distinct(squadNickname, squadId),
            by = "squadId")

all_subs %>%
  filter(period < 5,fromPos == "S") %>% 
  count(season) %>% 
  ggplot(aes(x = season,y = n/(56*2),group = 1)) +
  geom_point() +
  geom_line() +
  expand_limits(y = c(0,10)) +
  theme_minimal() +
  scale_y_continuous(breaks = 1:5*2) +
  labs(y = "Average subs per team, per game",
       caption = "Data: Champion Data") +
  theme(plot.background = element_rect(colour = "black"))

all_subs %>% 
  filter(fromPos != "S",period < 5,season %in% c("2019","2020","2021")) %>% 
    count(season,period,fromPos) %>%
  mutate(fromPos = factor(fromPos,levels = c("GS","GA","WA","C","WD","GD","GK"),ordered = T)) %>% 
  arrange(fromPos) %>% 
  ggplot(aes(x = period,y = n/112,col = fromPos)) +
  geom_point(size = 2) +
  geom_line(size = 1.3) + 
  scale_color_brewer(palette = "RdYlGn") +
  facet_wrap(~season,nrow = 1) +
  theme_bw() +
  labs(x = "Quarter",
       y = "Average position changes per team, per game",
       title = "Position changes",
       subtitle = "Average number of changes for a given position",
       caption = "Data: Champion Data",
       colour = "Position") +
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    plot.background = element_rect(colour = "black"))
  
# c("2019","2021","2020")

all_subs %>%
  filter(period < 5,season %in% c("2019","2020","2021"),fromPos == "S") %>% 
  mutate(minute = floor(periodSeconds/60),
         season = factor(season,levels = c("2020","2021","2019"),ordered = T)) %>%
  count(season,minute,period) %>% 
  ggplot(aes(x = minute,y = n/112)) +
  geom_col(aes(fill = season),position = "identity",col = "white",alpha = 0.85) +
  facet_wrap(~paste("Qtr -",period),nrow = 1) +
  theme_bw() +
  labs(x = "Game minute",
       y = "Average subs per team, per game",
       title = "Substitution timing",
       subtitle = "The minute in each quarter teams make changes",
       caption = "Data: Champion Data",
       fill = "Season") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black"))

all_subs %>% 
  filter(periodSeconds < 61) %>% 
  mutate(on_zero = periodSeconds ==0) %>% count(on_zero)
