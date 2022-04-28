library(tidyverse)
library(geomtextpath)

game_log <- tibble::tribble(
  ~CP, ~POS,  ~OC, ~Minutes, ~Seconds,
  "F",  "F",  "G",      14L,     "40",
  "S",  "S",  "G",      14L,     "19",
  "F",  "F",  "G",      13L,     "45",
  "S",  "S",  "G",      12L,     "47",
  "F",  "F",  "G",      12L,     "30",
  "S",  "S",  "G",      12L,     "00",
  "F",  "F",  "G",      11L,     "45",
  "S",  "S",  "G",      11L,     "16",
  "F",  "F",  "T",      11L,     "07",
  "F",  "S",  "M",      10L,     "47",
  "F",  "F", "MR",      10L,     "15",
  "F",  "F",  "G",      10L,     "10",
  "S",  "S",  "T",      10L,     "00",
  "S",  "F",  "G",       9L,     "55",
  "F",  "F",  "T",       9L,     "42",
  "F",  "S",  "T",       9L,     "17",
  "F",  "F",  "G",       8L,     "49",
  "S",  "S",  "G",       8L,     "15",
  "F",  "F",  "G",       8L,     "02",
  "S",  "S",  "G",       7L,     "42",
  "F",  "F",  "G",       7L,     "19",
  "S",  "S",  "M",       7L,     "00",
  "S",  "F",  "G",       6L,     "49",
  "F",  "F",  "T",       6L,     "29",
  "F",  "S",  "M",       6L,     "05",
  "F",  "F", "MR",       5L,     "53",
  "F",  "F",  "G",       5L,     "50",
  "S",  "S", "MR",       5L,     "25",
  "S",  "S",  "G",       5L,     "24",
  "F",  "F",  "G",       4L,     "59",
  "S",  "S", "MR",       4L,     "40",
  "S",  "S",  "G",       4L,     "30",
  "F",  "F",  "G",       3L,     "50",
  "S",  "S", "MR",       3L,     "30",
  "S",  "S",  "G",       3L,     "20",
  "F",  "F",  "G",       2L,     "44",
  "S",  "S", "T",       2L,     "29",
  "S",  "F", "MR",       2L,     "10",
  "S",  "F",  "G",       2L,     "05",
  "F",  "F",  "G",       1L,     "45",
  "S",  "S", "T",       1L,     "32",
  "S",  "F", "MR",       1L,     "10",
  "S",  "F",  "G",       1L,     "06",
  "F",  "F",  "T",       0L,     "50",
  "F",  "S",  "M",       0L,     "43",
  "F",  "F",  "G",       0L,     "23",
  "S",  "S",  "T",       0L,     "11",
  "S",  "F",  "G",       0L,     "05"
  ) %>% 
  mutate(periodSeconds = 900 - (as.numeric(Minutes)*60 + as.numeric(Seconds)),
        #OC_extra = if_else(OC == "G" & CP != POS,"EG",OC),
        CP = if_else(CP == "F","Fever","Swifts"),
        POS = if_else(POS == "F","Fever","Swifts"),
        OC = case_when(
          OC == "G" ~ "Goal",
          OC == "T" ~ "Turnover",
          OC == "M" ~ "Miss",
          OC == "MR" ~ "Miss + o-reb",
        ))

SquadName_Colours <- SquadName_Colours[names(SquadName_Colours) %in% c("Fever","Swifts")]

game_log %>% 
  filter(CP != POS,OC == "G")


plot_dat <- game_log %>% 
  mutate(periodSeconds = 900 - (as.numeric(Minutes)*60 + as.numeric(Seconds)),
         OC = if_else(OC == "G" & CP != POS,"EG",OC),
         sp = if_else(POS == "S" & OC != "MR",1,0) %>% cumsum(),
         fp = if_else(POS == "F" & OC != "MR",1,0) %>% cumsum())

plot_dat %>% 
  ggplot(aes(periodSeconds)) +
  geom_step(aes(y = sp),col = "red") +
  geom_step(aes(y = fp),col = "green")
  geom_point()

  
  game_log %>% 
    mutate(posCount = 1:n()) %>% 
    ggplot(aes(x = POS,y = posCount)) +
    geom_tile(aes(fill = CP),col = "black") +
    geom_text(aes(label = OC)) +
    scale_y_reverse() +
    annotate("textsegment",
             x = 2.7, xend = 2.7,
             y = 10, yend = 35,
             label = "Time",
             arrow = arrow(angle = 30, length = unit(0.125, "inches"),
                           ends = "last", type = "closed")) +
    scale_fill_manual(values = SquadName_Colours) +
    scale_x_discrete(position = "top") +
    labs(title = "Fever v Swifts possession outcomes",
         subtitle = "Each possession from the 4th quarter of round 5",
         y = "",
         x = "",
         fill = "Centre pass",
         caption = "Data: Champion Data") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 1),
          plot.background = element_rect(colour = "black")) +
    expand_limits(x = 2.75) +
    facet_wrap(~"Team Possession")

  o_rtg %>% 
    filter(season == 2022) %>% 
    group_by(season,round,match) %>% 
    summarise(diff= max(possessions) - min(possessions)) %>% 
    arrange(-diff)
  