library(tidyverse)

source("R/load_netball_data.R")
load_netball_data(2021)

pre_plot_data <- bind_rows(
  # starting positions
  player_stats %>% 
    select(round,match,period,squadNickname,playerId,startingPositionCode) %>% 
    mutate(periodSeconds = 0) %>% 
    rename(position = startingPositionCode),
  # Substitutions
  subs %>%
    mutate(periodSeconds = if_else(periodSeconds == 0, as.integer(1),periodSeconds)) %>% 
    select(season,playerId,round,match,squadNickname,position = toPos,period,periodSeconds)) %>% 
  arrange(season,round,match,squadNickname,playerId,period,periodSeconds) %>%
  group_by(season,squadNickname,playerId,match,round,period) %>% 
  mutate(position = if_else(position == "S","I",position),
         time_on = lead(periodSeconds),
         time_on = if_else(is.na(time_on),900,time_on)) %>%
  ungroup() %>% 
  left_join(player_info) %>% 
  filter(period < 5)

total_rounds <- player_stats %>% 
  distinct(displayName,round) %>% 
  count(displayName,name = "total_rounds")

df_custom <- function(x){
  pre_plot_data %>% 
    transmute("minute_{x}" := if_else((periodSeconds <= x*60 | periodSeconds == 1) & time_on > x*60 & time_on - x*60 > 30,1,0))
}


  map_dfr(0:14,~mutate(pre_plot_data,minute = .x,
                       is_on = if_else((periodSeconds <= .x*60 | periodSeconds == 1) & time_on > .x*60 & time_on - .x*60 > 30,1,0)))

plot_data <- map_dfc(0:14,df_custom) %>% 
  bind_cols(pre_plot_data,.) %>% 
  pivot_longer(cols = minute_0:minute_14) %>%
  group_by(season,squadNickname,displayName,period,position,name) %>%
  summarise(n = sum(value),
            .groups = "drop_last") %>%
  left_join(total_rounds) %>%
  mutate(name = str_extract(name,"\\d+"),
         name = as.integer(name),
         displayName = as.character(displayName),
         nn = sum(n),
         n = n/total_rounds,
         position = factor(position,levels = c("I","GK","GD","WD","C","WA","GA","GS"),ordered = T)) %>% 
  filter(n > 0)

plot_data %>%
  mutate(displayName = str_remove(displayName,".*\\.")) %>% 
  filter(squadNickname == "Lightning") %>%
  # group_by(displayName) %>%
  # filter(any(position %in% c("GA","GS"))) %>%
  # ungroup() %>%
  mutate(period = paste("Qtr - ",period,sep = "")) %>% 
  ggplot(aes(x = name,y = position,fill = n)) + 
  geom_tile() +
  scale_x_continuous(breaks = 0:14) +
  #scale_fill_distiller(palette = "Greens",direction = 1,labels = scales::percent,limits = c(0,1)) +
  scale_fill_gradient2(high = '#d7191c',mid = '#ffffbf', low = '#1a9641',labels = scales::percent,limits = c(0,1),midpoint = 0.5) +
  labs(title = "Lightning - Season 2021",
       subtitle = "% Time on court for each player in each position they played",
    x = "Minutes",
       y = "Position",
       fill = "% Time on court",
    caption = "Data: Champion data") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.x.bottom = element_line(),
        axis.ticks.y.left = element_line(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(color = "black"),
        strip.text.y = element_text(size = 7)) +
  facet_grid(displayName~period,scales = "free_y")


# Selectors ---------------------------------------------------------------

# Seasons
season_selector <- "Season 2020"
team_input <- c(NULL)
pos_input <- c(NULL)
player_input <- "C.Bassett"


season_selector <- c("Season 2017","Season 2018", "Season 2019","Season 2020","Season 2021")

# Positions
#pos_selector <- pre_plot_data %>% 
  

# Team
pull_distinct <- function(df,col){
  df %>% 
    distinct({{ col}}) %>% 
    pull({{ col }})
}

team_selector <- pull_distinct(pre_plot_data, squadNickname)

# Player
# WRONG
player_selector <- pull_distinct(pre_plot_data, displayName)






# NBA Subs - Positive Risidual --------------------------------------------

name_order <- pre_plot_data %>% 
  group_by(displayName,round) %>% 
  # summarise(n = n()) %>% 
  # arrange(-n) %>% 
  filter(period == 1) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(pos = case_when(
    position == "GS" ~ 1,
    position == "GA" ~ 2,
    position == "WA" ~ 3,
    position == "C" ~ 4,
    position == "WD" ~ 5,
    position == "GD" ~ 6,
    position == "GK" ~ 7,
    position == "I" ~ 8,
    TRUE ~ 9
  )) %>%
  select(displayName,round,pos)
  
plot_cols <- tibble(position = c("I","GS","GA","WA","C","WD","GD","GK"),
                    col = c("#a1a1a1","#f94144","#f3722c","#f8961e","#f9c74f","#90be6d","#43aa8b","#577590")) %>% 
  deframe()

pre_plot_data %>%
  left_join(name_order,by = c("displayName","round")) %>% 
  mutate(id = 1:n(),
         periodSeconds = periodSeconds/60,
         time_on = time_on/60) %>% 
  pivot_longer(cols = c(periodSeconds,time_on)) %>% 
  filter(round == 3,squadNickname == "Fever") %>% 
  ggplot(aes(y = fct_reorder(displayName,-pos),x = value, col = fct_relevel(position,c("I","GK","GD","WD","C","WA","GA","GS")))) +
  geom_line(aes(group = id),size = 8,alpha = 1) +
  #geom_point(size = 5,shape = "|",col = "black") +
  facet_grid(.~paste("Qtr",period)) +
  theme_minimal() +
  labs(col = "",
       y = "",
       x = "Game clock") +
  theme(panel.grid.major = element_blank()) +
  scale_color_manual(values = plot_cols)
  


plot_data %>%
  filter(str_detect(displayName,"Allen|Stant|Garbin|Housby")) %>% 
  group_by(displayName) %>%
  filter(any(position %in% c("GA"))) %>%
  ungroup() %>%
  mutate(period = paste("Qtr - ",period,sep = "")) %>% 
  ggplot(aes(x = name,y = position,fill = n)) + 
  geom_tile() +
  scale_x_continuous(breaks = 0:14) +
  #scale_fill_distiller(palette = "Greens",direction = 1,labels = scales::percent,limits = c(0,1)) +
  scale_fill_gradient2(high = '#d7191c',mid = '#ffffbf', low = '#1a9641',labels = scales::percent,limits = c(0,1),midpoint = 0.5) +
  labs(title = "Pinch hitters in season 2020",
       subtitle = "% Time on court in goal scoring positions",
       x = "Minutes",
       y = "Position",
       fill = "% Time on court",
       caption = "Data: Champion data") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.x.bottom = element_line(),
        axis.ticks.y.left = element_line(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(colour = "black")) +
  facet_grid(displayName~period,scales = "free_y")
