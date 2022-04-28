library(ncaahoopR)
library(tidyverse)
library(readr)
library(knitr)



ncaa_name_dictionary <- read_csv("~/Downloads/ncaahoopR_data-master/ncaa_name_dictionary.csv") %>% 
  select(ESPN,conference) %>% 
  mutate(ESPN = str_replace_all(ESPN," ","_"))

# Standings ---------------------------------------------------------------

all_schedule <- tibble(file_path = list.files("~/Downloads/ncaahoopR_data-master/",recursive = F,pattern = "\\d{4}\\-\\d{2}",full.names = T) %>% 
                         str_replace(.,"\\/{2}","\\/") %>% 
                         paste(.,"/schedules/",sep = "") %>% 
                         list.files(.,recursive = T,full.names = T)) %>% 
  mutate(team = str_extract(file_path,"\\/{2}.*?csv") %>% str_remove_all("\\/{2}|\\_schedule.*"),
         season = str_extract(file_path,"\\d{4}") %>% as.integer()) %>% 
  filter(between(season, 2007,2019)) %>% 
  left_join(ncaa_name_dictionary,by = c("team" = "ESPN"))

acc_schedule <- all_schedule %>% 
  filter(conference == "ACC",between(season,2007,2019)) %>%  
  mutate(schedule = map(file_path,~data.table::fread(.x) %>% as_tibble() %>% filter(!is.na(team_score)) %>% slice_tail()),
         game_date = map_dbl(schedule,~.x$date %>% first()) %>% lubridate::as_date(),
         final_standing = map_chr(schedule,~.x$record) %>% map_chr(~str_match(.x,"\\((.*)\\-")[[2]]) %>% as.integer()) %>% 
  group_by(season) %>% 
  mutate(end_pos = dense_rank(desc(final_standing))) %>% 
  ungroup()

# Shared experience -------------------------------------------------------

make_crosses <- function(dat){
  dat %>%
    mutate(player = str_remove_all(player,"[[:punct:]]|\\s"),
           player_id = paste(player,"(",position,")",sep = "")) %$% 
    cross2(player_id,player_id,.filter = function(x, y) x >= y)  %>%  map(~paste(.x,collapse = "-")) %>% unlist()
}

all_csvs <- tibble(file_path = list.files("~/Downloads/ncaahoopR_data-master/",recursive = F,pattern = "\\d{4}\\-\\d{2}",full.names = T) %>% 
  str_replace(.,"\\/{2}","\\/") %>% 
  paste(.,"/box_scores/",sep = "") %>% 
  list.files(.,recursive = T,full.names = T)) %>% 
  mutate(team = str_extract(file_path,"\\/{2}.*?\\/") %>% str_remove_all("\\/"),
         season = str_extract(file_path,"\\d{4}") %>% as.integer()) %>% 
  filter(between(season, 2007,2019)) %>% 
  left_join(ncaa_name_dictionary,by = c("team" = "ESPN"))


acc_rosters <- all_csvs %>% 
  filter(conference == "ACC",between(season,2007,2019)) %>%  
  mutate(roster = map(file_path,~data.table::fread(.x) %>% as_tibble() %>% select(player,position,date) %>% filter(player != "TEAM")),
         crosses = map(roster,make_crosses),
         game_date = map_dbl(roster,~.x$date %>% first()) %>% lubridate::as_date()) %>% 
  arrange(team,season,game_date) %>% 
    group_by(team,season) %>% 
    slice(1:31) %>% 
    ungroup()

plot_cols <- ncaa_colors %>% 
  transmute(team = str_replace_all(espn_name," ","_"),primary_color) %>% 
  left_join(ncaa_name_dictionary, by = c("team" = "ESPN")) %>% 
  filter(conference == "ACC") %>% 
  select(team,primary_color) %>% 
  deframe()

acc_rosters %>% 
  mutate(game_date = map_dbl(roster,~.x$date %>% first()) %>% lubridate::as_date()) %>% 
  select(-roster) %>% 
  unnest("crosses") %>% 
  count(season,team,crosses) %>% 
  arrange(season,team) %>% 
  group_by(team,crosses) %>% 
  mutate(cum_cross = cumsum(n)) %>% 
  ungroup() %>% 
  group_by(season,team) %>% 
  summarise(tot_se = sum(cum_cross)) %>% 
    left_join(acc_schedule %>% select(team,season,end_pos),by = c("season", "team")) %>%
  ggplot(aes(x = season, y = tot_se, group = team,col = team,label = end_pos)) +
  geom_point(size = 4) + 
  geom_path(size = 2) +
    geom_text(aes(label = end_pos), col = "white") +
  scale_colour_manual(values = plot_cols) +
  scale_x_continuous(breaks = 2008:2019) +
  labs(x = "Season",
       y = "Cumulative shared experience",
       colour = 'ACC team',
       title = "Shared experience in NCAA Division 1 ACC conference") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

acc_rosters %>% 
  filter(season > 2010) %>% 
  mutate(plrs = map(roster,~.x$player)) %>% 
  select(season,team,plrs) %>% 
  unnest("plrs") %>% 
  distinct(season,team,plrs) %>% 
  count(season,team) %>% 
  ggplot(aes(y = fct_reorder(team,n,sum),x = n,fill = factor(season))) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = 15) +
  labs(x = "Average bench size",
       y = "Team",
       fill = "Season",
       title = "Average bench size per season") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

acc_rosters %>% 
  mutate(plrs = map(roster,~.x$player)) %>% 
  select(season,team,plrs) %>% 
  unnest("plrs") %>% 
  count(season,team,plrs) %>% 
  filter(team == "UNC") %>%
  group_by(plrs) %>% 
  mutate(tot_gms = cumsum(n)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  arrange(season,-tot_gms) %>% 
  filter(season > 2009) %>% 
  mutate(player_num = 1:n(),
         plrs = paste(plrs," (",tot_gms,")",sep = "")) %>% 
  select(-team,-n,-tot_gms) %>% 
  pivot_wider(names_from = season,values_from = plrs,values_fill = list(plrs = "")) %>% 
  kable(caption = "All players to hit the court for UNC in each season with the players total UNC cumulative games in brackets") %>% 
  kableExtra::kable_classic()

UNC_drafted <- tibble::tribble(
            ~plrs, ~DraftYr,
     "C. Anthony",     2020L,
  "C. Johnson",     2019L,
    "N. Little",     2019L,
       "C. White",     2019L,
     "T. Bradley",     2017L,
   "R. Bullock",     2013L,
  "H. Barnes",     2012L,
         "E. Davis",     2010L,
  "W. Ellington",     2009L,
      "D. Green",     2009L
  ) %>% mutate(DraftYr = DraftYr - 1)

acc_rosters %>%
  filter(team == "UNC") %>% 
  mutate(plrs = map(roster,  ~ paste(.x$player," (",.x$position,")",sep = ""))) %>% 
  select(season, team, plrs) %>%
  unnest("plrs") %>% 
  count(season, plrs) %>% 
  arrange(plrs,season) %>% 
  group_by(plrs) %>% 
  mutate(first_season = min(season),
         last_season = max(season),
         n = cumsum(n)) %>% 
  filter(last_season > 2010) %>% 
  ungroup() %>% 
  arrange(first_season,last_season,-n) %>% 
  mutate(plrs = str_remove(plrs," \\(.*")) %>% 
  left_join(UNC_drafted) %>%
  mutate(Drafted = if_else(!is.na(DraftYr),"Drafted","Un-drafted")) %>%
  ggplot(aes(y = fct_inorder(plrs),x = season,label = n,fill = Drafted)) +
  geom_tile() + 
  geom_text(col = "white") +
  scale_x_continuous(breaks = 2008:2019) +
  scale_fill_manual(values = c("Drafted" = "red","Un-drafted" = "black")) +
  labs(x = "Season",
       y = "Player",
       title = "Playing spans of UNC players") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  


acc_rosters %>%
  filter(team == "Miami") %>% 
  mutate(plrs = map(roster,  ~ paste(.x$player," (",.x$position,")",sep = ""))) %>% 
  select(season, team, plrs) %>%
  unnest("plrs") %>% 
  count(season, plrs) %>% 
  arrange(plrs,season) %>% 
  group_by(plrs) %>% 
  mutate(first_season = min(season),
         last_season = max(season),
         n = cumsum(n),
         year = 1:n()) %>% 
  filter(last_season > 2010) %>% 
  ungroup() %>% 
  arrange(first_season,last_season,-n) %>% 
  ggplot(aes(y = fct_inorder(plrs),x = season,label = n,fill = factor(year))) +
  geom_tile() + 
  geom_text(col = "black") +
  scale_x_continuous(breaks = 2008:2019) +
  scale_fill_brewer(palette = "Greens") +
  labs(x = "Season",
       y = "Player",
       fill = "Playing year",
       title = "Playing spans of Miami players") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
