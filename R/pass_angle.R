library(tidyverse)
library(superNetballR)
library(ggridges)
library(ggforce)

pos_lvls <- c("GS","GA","WA","C","WD","GD","GK","S")

plot_cols <- tibble(position = pos_lvls,
                    col = c("#f94144","#f3722c","#f8961e","#f9c74f","#90be6d","#43aa8b","#577590","#a1a1a1")) %>% 
  deframe()

# Add netball ggplot elements
add_forward_third <- function() geom_rect(xmin = 0,xmax = 100,ymin = 133,ymax = 200,fill = "white",col = "black",inherit.aes = F)
add_centre_third <- function() geom_rect(xmin = 0,xmax = 100,ymin = 67,ymax = 133,fill = "white",col = "black",inherit.aes = F)
add_forward_goal_circle <- function() geom_arc_bar(aes(x0 = 50,y0 = 200,r0 = 0, r = 4.9/0.1525,start = pi/2,end = 3/2*pi), inherit.aes = F)
add_centre_circle <- function() geom_circle(aes(x0 = 50, y0 = 100,r = 0.45/0.1525),col = "black",fill = "white",inherit.aes = F)
add_forward_ring <- function() geom_circle(aes(x0 = 50, y0 = 197,r = 0.19/0.1525),col = "black",fill = "white",inherit.aes = F)


# Formulas ----------------------------------------------------------------

calc_dist <- function(x0,x1,y0,y1){
  
  # Convert to meters
  # 200 units / 30.5 meters length of netball court
  x0 = x0 * 0.1525
  x1 = x1 * 0.1525
  y0 = y0 * 0.1525
  y1 = y1 * 0.1525
  
  # Pythagorous theorem to calculate straightline distance
  pass_dist = sqrt((x1-x0)^2 + (y1 - y0)^2)
  return(sqrt((x1-x0)^2 + (y1 - y0)^2))
}

pass_angle <- function(M1,M2,O1 = 50,O2 = 100){
  M = c(M1,M2)
  O = c(O1,O2)
  M = M - O
  N = c(50,200) - O
  theta = atan2(N[2],N[1]) - atan2(M[2],M[1]) 
  return(theta * 180/pi)
}

askAndYouShallReceive <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/R/R Play/CausalFan/data/first_two_passes.RDS")

# Create dataset ----------------------------------------------------------

`2018_matchups` <- `2018_data` %>% 
  map(tidyMatch) %>% 
  map_dfr(superNetballR::matchResults) %>% 
  ungroup() %>% 
  transmute(round,squadNickname,game) %>% 
  {full_join(.,.,by = c("round","game"))} %>% 
  filter(squadNickname.x != squadNickname.y) %>% 
  rename("teamName" = "squadNickname.x","opponent" = "squadNickname.y","roundNo" = "round","matchNo" = "game") %>% 
  mutate(teamName = str_to_sentence(teamName),
         opponent = str_to_sentence(opponent))

first_two_passes <- askAndYouShallReceive %>% 
  mutate(
    # Calculate pass length
    first_pass_len = calc_dist(50, centrePassX, 100, centrePassY),
    second_pass_len = calc_dist(centrePassX, secondPhaseX, centrePassY, secondPhaseY),
    # Calculate pass angle of first pass
    pass1angle = map2_dbl(centrePassX, centrePassY,  ~ pass_angle(.x, .y)),
    pass1angle = if_else(pass1angle > 180, pass1angle - 360, pass1angle),
    pass2angle = pmap_dbl(list(M1 = secondPhaseX,M2 = secondPhaseY,O1 = centrePassX,O2 = centrePassY),.f = pass_angle),
    pass2angle = if_else(pass2angle > 180, pass2angle - 360, pass2angle),
    # Determine if the second pass goes in the goal circle
    in_circle = (secondPhaseX - 50)^2 + (secondPhaseY - 200)^2 < (4.9/0.1525)^2,
    centrePassRec = factor(centrePassRec,level = pos_lvls,ordered = T),
    secondPhaseRec = factor(secondPhaseRec,level = pos_lvls,ordered = T)) %>% 
  left_join(`2018_matchups`,by = c("roundNo", "teamName"))

# Pass angle

# Example -----------------------------------------------------------------

tibble::tribble(
       ~id,  ~x,   ~y,
      "cp", 50L, 100L,
  "pass 1", 25L, 130L,
  "pass 2", 10L, 190L,
    "goal", 50L, 197L
  ) %>% 
{ggplot(data = .,aes(x = x,y = y)) +
  add_forward_third() +
  add_centre_third() +
  add_forward_goal_circle() +
  add_forward_ring() +
  add_centre_circle() +
  geom_path(data = filter(.,id %in% c("cp","pass 1","pass 2"))) +
  geom_point(data = filter(.,id %in% c("cp","pass 1","pass 2")), size = 3) +
  annotate("text", x = 46, y = 110, label = "A",parse = T) +
  annotate("text", x = 25, y = 140, label = "B",parse = T) +
  geom_path(data = filter(.,id %in% c("pass 1", "goal")),linetype = 2) +
  geom_path(data = filter(.,id %in% c("cp", "goal")),linetype = 3) +
  coord_fixed(xlim = c(0,100),ylim = c(67,200)) +
  theme_void()}


# All teams passing angles ------------------------------------------------

bind_rows(first_two_passes %>% 
            filter(!is.na(secondPhaseX),!secondPhaseRec %in% c("GK"),!is.na(secondPhaseRec)) %>%
            transmute(angle = pass2angle,reciever = secondPhaseRec,type = "Second"),
          first_two_passes %>% 
            filter(!is.na(centrePassX),!centrePassRec %in% c("GK"),!is.na(centrePassRec)) %>%
            transmute(angle = pass1angle,reciever = centrePassRec,type = "First")) %>% 
  mutate(is_neg = if_else(angle < 0,"Neg","Pos"),
         #angle = abs(angle)
         #is_neg = paste(reciever,is_neg)
  ) %>% 
  ggplot(aes(angle,fill = reciever)) +
  #geom_density(alpha = 0.5) +
  geom_histogram(binwidth = 4) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(breaks = seq(-180,180,20)) +
  facet_grid(type~.) +
  scale_fill_brewer(palette = "Spectral") +
  theme_bw() +
  labs(x = "Pass Angle (°)",
       y = "Density",
       fill = "Reciever position",
       title = "Passing angle of all teams in 2018 SSN Season",
       subtitle = "First and second passing phases",
       caption = "Data courtesy of Aaron Fox (@aaron_s_fox)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,size = 8))


# First Pass --------------------------------------------------------------

## Pass angle totals
first_two_passes %>% 
  filter(!is.na(centrePassX),!centrePassRec %in% c("UNKNOWN","GS"),!is.na(centrePassRec)) %>% 
  ggplot(aes(x = pass1angle, fill = centrePassRec)) + 
  geom_histogram(binwidth = 2) +
  #geom_freqpoly(breaks = seq(-180,180,5),size = 1.1) +
  geom_vline(xintercept = c(40,65,-40,-65)) +
  scale_x_continuous(breaks = seq(-180,180,20)) +
  scale_fill_brewer(palette = "Spectral") +
  facet_wrap(~teamName,ncol = 2) +
  labs(x = "Centre Pass Angle (°)",
       y = "# Passes",
       title = "Pass angle of centre pass",
       subtitle = "Combined contribution of all positions (bars are stacked)",
       caption = "Data courtesy of Aaron Fox (@aaron_s_fox)",
       fill = "Receiver position") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,size = 8),
        axis.text.x = element_text(angle = 90,hjust = 1))

## Pass angle individual contributions

first_two_passes %>% 
  filter(!is.na(centrePassX),!centrePassRec %in% c("UNKNOWN","GS")) %>% 
  ggplot(aes(x = pass1angle, col = centrePassRec)) + 
  #geom_histogram(position = "identity",alpha = 0.5) +
  geom_freqpoly(breaks = seq(-180,180,5),size = 1.1) +
  geom_vline(xintercept = c(40,65,-40,-65)) +
  scale_x_continuous(breaks = seq(-180,180,20)) +
  scale_colour_brewer(palette = "Spectral") +
  facet_wrap(~teamName,ncol = 2) +
  labs(x = "Centre Pass Angle (°)",
       y = "# Passes",
       title = "Pass angle of centre pass",
       subtitle = "Individual contributions by positions (lines overlap)",
       caption = "Data courtesy of Aaron Fox (@aaron_s_fox)",
       colour = "Receiver position") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,size = 8),
        axis.text.x = element_text(angle = 90,hjust = 1))


# Example angles ----------------------------------------------------------

first_two_passes %>%
filter(!is.na(centrePassX),!centrePassRec %in% c("GK"),!is.na(centrePassRec)) %>%
mutate(angle = abs(pass1angle),
butter_zone = case_when(
between(angle,40,65) ~ "Butter",
angle > 65 & angle <= 90 ~ "Mid",
angle > 0 & angle <= 40 ~ "Forward",
angle > 90 ~ "Backwards",
TRUE ~ "NoButter")) %>%
count(butter_zone) %>%
mutate(pct = scales::percent(n/sum(n))) 
  
tribble(
  ~x, ~y,
  50, 100,
  22,200/3*2,
  0,200/3*2,
  0,200/3*2-9.75,
  50, 100,
  78,200/3*2,
  100,200/3*2,
  100,200/3*2 - 9.75,
) %>% mutate(sec = "40-60°") %>% 
{ggplot(.) +
  add_forward_third() +
  add_centre_third() +
  add_forward_goal_circle() +
  add_forward_ring() +
  add_centre_circle() +
  geom_polygon(data = filter(.,sec == "40-60°"), aes(x,y,fill = sec),col = "black") +
  geom_segment(aes(x = 50 - (0.875/0.1525),xend = 50 + (0.875/0.1525), y = 100 + (0.91/0.1525),yend = 100 + (0.91/0.1525)),col = "red") +
  geom_text(aes(label ="1.75 m",x = 50,y = 110),hjust = 0.5) +
  theme_void() +
    labs(fill = "Section") +
  coord_fixed(ylim = c(67,200))} 

# Second pass -------------------------------------------------------------

first_two_passes %>% 
  filter(!is.na(secondPhaseX),!secondPhaseRec %in% c("UNKNOWN"),!is.na(secondPhaseRec)) %>% 
  ggplot(aes(x = pass2angle, fill = secondPhaseRec)) + 
  geom_histogram(binwidth = 2) +
  #geom_freqpoly(breaks = seq(-180,180,5),size = 1.1) +
  #geom_vline(xintercept = c(40,60,-40,-60)) +
  scale_x_continuous(breaks = seq(-180,180,20)) +
  scale_fill_brewer(palette = "Spectral") +
  facet_wrap(~teamName,ncol = 2) +
  labs(x = "Second pass Angle (°)",
       y = "# Passes",
       title = "Pass angle of second pass",
       subtitle = "Combined contribution of all positions (bars are stacked)",
       caption = "Data courtesy of Aaron Fox (@aaron_s_fox)",
       fill = "Receiver position") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,size = 8),
        axis.text.x = element_text(angle = 90,hjust = 1))

first_two_passes %>% 
  filter(!is.na(secondPhaseX),!secondPhaseRec %in% c("UNKNOWN"),!is.na(secondPhaseRec)) %>% 
  ggplot(aes(x = pass2angle, col = secondPhaseRec)) + 
  geom_freqpoly(breaks = seq(-180,180,5),size = 1.1) +
  geom_vline(xintercept = c(15,40,-15,-40)) +
  scale_x_continuous(breaks = seq(-180,180,20)) +
  scale_colour_brewer(palette = "Spectral") +
  facet_wrap(~teamName,ncol = 2,scales = "free_y") +
  labs(x = "Second pass angle (°)",
       y = "# Passes",
       title = "Pass angle of second pass",
       subtitle = "Individual contributions by positions (lines overlap)",
       caption = "Data courtesy of Aaron Fox (@aaron_s_fox)",
       colour = "Receiver position") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,size = 8),
        axis.text.x = element_text(angle = 90,hjust = 1))

first_two_passes %>%
  filter(!is.na(secondPhaseX),!is.na(secondPhaseRec),secondPhaseRec %in% c("GA","WA","GS","C")) %>%
  mutate(side = if_else(centrePassX < 50,"Left","Right")) %>%
  ggplot(aes(pass2angle,fill = secondPhaseRec)) +
  #geom_density(alpha = 0.5) +
  geom_histogram(binwidth = 3) +
  #geom_freqpoly(binwidth = 3, size = 1,alpha = 0.8) +
  geom_vline(xintercept = 0) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_continuous(breaks = seq(-180,180,30)) +
  scale_y_continuous(breaks = seq(0,40,10)) +
  facet_grid(teamName~side) +
  theme_bw() +
  labs(x = "Second pass angle (°)",
       y = "# Passes",
       title = "Pass angle of second pass",
       subtitle = "Combined contribution of all positions (bars are stacked)",
       caption = "Data courtesy of Aaron Fox (@aaron_s_fox)",
       fill = "Receiver position") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,size = 8),
        axis.text.x = element_text(angle = 90,hjust = 1))


# By opponent -------------------------------------------------------------

first_two_passes %>% 
  filter(!is.na(centrePassX),!centrePassRec %in% c("UNKNOWN","GS"),!is.na(centrePassRec)) %>% 
  pivot_longer(cols = c(teamName,opponent),names_to = "teamPosition") %>%
  mutate(teamPosition = if_else(teamPosition == "opponent","defence","offence")) %>% 
  ggplot(aes(x = pass1angle, col = teamPosition)) + 
  #geom_histogram(binwidth = 2) +
  geom_freqpoly(breaks = seq(-180,180,5),size = 1.1) +
  geom_vline(xintercept = c(40,65,-40,-65)) +
  scale_x_continuous(breaks = seq(-180,180,20)) +
  scale_fill_brewer(palette = "Spectral") +
  facet_wrap(~value,ncol = 2) +
  labs(x = "Centre Pass Angle (°)",
       y = "# Passes",
       title = "Pass angle of centre pass",
       subtitle = "Comparison between team attacking vs defending",
       caption = "Data courtesy of Aaron Fox (@aaron_s_fox)",
       col = "Team Position") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,size = 8),
        axis.text.x = element_text(angle = 90,hjust = 1))

first_two_passes %>% 
  filter(!is.na(secondPhaseX),!secondPhaseRec %in% c("UNKNOWN"),!is.na(secondPhaseRec)) %>% 
  pivot_longer(cols = c(teamName,opponent),names_to = "teamPosition") %>%
  mutate(teamPosition = if_else(teamPosition == "opponent","defence","offence")) %>% 
  ggplot(aes(x = pass2angle, col = teamPosition)) + 
  #geom_histogram(binwidth = 2) +
  geom_freqpoly(breaks = seq(-180,180,5),size = 1.1) +
  #geom_vline(xintercept = c(40,60,-40,-60)) +
  scale_x_continuous(breaks = seq(-180,180,20)) +
  scale_fill_brewer(palette = "Spectral") +
  facet_wrap(~value,ncol = 2) +
  labs(x = "Second pass Angle (°)",
       y = "# Passes",
       title = "Pass angle of second pass",
       subtitle = "Combined contribution of all positions (bars are stacked)",
       caption = "Data courtesy of Aaron Fox (@aaron_s_fox)",
       col = "Team Position") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,size = 8),
        axis.text.x = element_text(angle = 90,hjust = 1))


# Pass Length -------------------------------------------------------------

first_two_passes
