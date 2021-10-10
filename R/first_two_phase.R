library(tidyverse)
library(superNetballR)
library(ggridges)
library(ggforce)

plot_cols <- tibble(position = c("UNKNOWN","GS","GA","WA","C","WD","GD","GK"),
                    col = c("#a1a1a1","#f94144","#f3722c","#f8961e","#f9c74f","#90be6d","#43aa8b","#577590")) %>% 
  deframe()


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

pass_angle <- function(M,N = c(50,100)){
  M = M - N
  N = c(0,201)
  theta = atan2(N[2],N[1]) - atan2(M[2],M[1]) 
  return(theta * 180/pi)
}


# Create dataset ----------------------------------------------------------

first_two_passes <- readRDS("data/first_two_passes.RDS") %>% 
  mutate(
    # Calculate pass length
    first_pass_len = calc_dist(50, centrePassX, 100, centrePassY),
    second_pass_len = calc_dist(centrePassX, secondPhaseX, centrePassY, secondPhaseY),
    # Calculate pass angle of first pass
    pass1angle = map2_dbl(centrePassX, centrePassY,  ~ pass_angle(c(.x, .y))),
    pass1angle = if_else(pass1angle > 180, -90 - (pass1angle - 180), pass1angle),
    # Determine if the second pass goes in the goal circle
    in_circle = (secondPhaseX - 50)^2 + (secondPhaseY - 200)^2 < (4.9/0.1525)^2,
    centrePassRec = factor(centrePassRec,level = c("GS","GA","WA","C","WD","GD","GK","S","UNKNOWN"),ordered = T),
    secondPhaseRec = factor(secondPhaseRec,level = c("GS","GA","WA","C","WD","GD","GK","S","UNKNOWN"),ordered = T))

# Plots -------------------------------------------------------------------

# 1 - Pass length distribution by teams
# 2 - Each team's 10 longest passes
# 3 - Passes to goal circle on second pass
# 4.1 -  Pass angle from centre pass - totals
# 4.2 -  Pass angle from centre pass - individuals


# Pass length distribution by teams

first_two_passes %>% 
  pivot_longer(cols = c(first_pass_len,second_pass_len)) %>% 
  ggplot(aes(x = value,y = teamName)) + 
  stat_density_ridges(quantile_lines = TRUE,quantiles = 2,alpha = 0.7) +
  theme_bw() + 
  facet_wrap(~name)

# Each team's 10 longest passes

first_two_passes %>% 
  group_by(teamName) %>% 
  arrange(teamName,-second_pass_len) %>% 
  slice(1:20) %>%
  mutate(id = as.character(1:n())) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(centrePassX,centrePassY,secondPhaseX,secondPhaseY),names_to = c("phase","dimension"),names_pattern = "(.*)(.$)") %>% 
  pivot_wider(names_from = dimension,values_from = value) %>% 
  mutate(position = if_else(phase == "centrePass",centrePassRec,secondPhaseRec)) %>% 
  ggplot(aes(x = X,y = Y,group = id)) +
  geom_rect(xmin = 0,xmax = 100,ymin = 133,ymax = 200,fill = "white",col = "black") +
  geom_rect(xmin = 0,xmax = 100,ymin = 67,ymax = 133,fill = "white",col = "black") +
  geom_arc_bar(aes(x0 = 50,y0 = 200,r0 = 0, r = 4.9/0.1525,start = pi/2,end = 3/2*pi), inherit.aes = F) +
  geom_circle(aes(x0 = 50, y0 = 100,r = 0.45/0.1525),col = "black",fill = "white",inherit.aes = F) +
  geom_circle(aes(x0 = 50, y0 = 197,r = 0.19/0.1525),col = "black",fill = "white",inherit.aes = F) +
  geom_point(aes(col = position),size = 3) +
  scale_color_brewer(palette = "Spectral") +
  geom_line() +
  coord_fixed(ylim = c(67,200)) +
  theme_void() +
  facet_wrap(~teamName,nrow = 2) +
  labs(title = "Long bombs",
       subtitle = "Each teams 20 longest second phase passes",
       caption = "Data courtesy of Aaron Fox (@aaron_s_fox)",
       colour = "Player position") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5,size = 12))

# Passes to goal circle on second pass
## Create labels for plot
goals_in_second <- first_two_passes %>%
  filter(in_circle) %>% 
  count(teamName,secondPhaseRec) %>% 
  filter(secondPhaseRec %in% c("GA","GS")) %>% 
  arrange(teamName,secondPhaseRec) %>% 
  group_by(teamName) %>% 
  summarise(lab = paste(secondPhaseRec,n,sep = ": ",collapse = "\n"))

first_two_passes %>% 
  filter(in_circle) %>% 
  mutate(id = as.character(1:n())) %>% 
  pivot_longer(cols = c(centrePassX,centrePassY,secondPhaseX,secondPhaseY),names_to = c("phase","dimension"),names_pattern = "(.*)(.$)") %>% 
  pivot_wider(names_from = dimension,values_from = value) %>% 
  mutate(position = if_else(phase == "centrePass",centrePassRec,secondPhaseRec)) %>% 
  ggplot(aes(x = X,y = Y,group = id)) +
  geom_rect(xmin = 0,xmax = 100,ymin = 133,ymax = 200,fill = "white",col = "black") +
  geom_rect(xmin = 0,xmax = 100,ymin = 67,ymax = 133,fill = "white",col = "black") +
  geom_arc_bar(aes(x0 = 50,y0 = 200,r0 = 0, r = 4.9/0.1525,start = pi/2,end = 3/2*pi), inherit.aes = F) +
  geom_circle(aes(x0 = 50, y0 = 100,r = 0.45/0.1525),col = "black",fill = "white",inherit.aes = F) +
  geom_circle(aes(x0 = 50, y0 = 197,r = 0.19/0.1525),col = "black",fill = "white",inherit.aes = F) +
  geom_point(aes(col = position),size = 3) +
  scale_color_brewer(palette = "Spectral") +
  geom_line() +
  geom_text(data = goals_in_second, aes(x = 15,y = 80,label = lab),inherit.aes = F) +
  coord_fixed(ylim = c(67,200)) +
  theme_void() +
  facet_wrap(~teamName,nrow = 2) +
  labs(title = "Go for goal",
       subtitle = "All second phase passes that enter the goal circle",
       caption = "Inset numbers are goaler pass recieved counts. Data courtesy of Aaron Fox (@aaron_s_fox)",
       colour = "Player position") +
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        plot.subtitle = element_text(hjust = 0.5,size = 12))

first_two_passes %>% 
  filter(centrePassRec != "UNKNOWN") %>% 
  mutate(quadrant = case_when(
    centrePassX <= 50 & centrePassY <= 100 ~ "BL",
    centrePassX > 50 & centrePassY <= 100 ~ "BR",
    centrePassX <= 50 & centrePassY > 100 ~ "TL",
    centrePassX > 50 & centrePassY > 100 ~ "TR",
    TRUE ~ "Error"
  ),
  id = as.character(1:n())) %>% 
  filter(teamName == "Swifts",quadrant == "TR",centrePassRec == "GA") %>% 
  pivot_longer(cols = c(centrePassX,centrePassY,secondPhaseX,secondPhaseY),
               names_to = c("phase","dimension"),
               names_pattern = "(.*)(.$)") %>% 
  pivot_wider(names_from = dimension,values_from = value) %>%
  mutate(position = if_else(phase == "centrePass",centrePassRec,secondPhaseRec)) %>% 
  ggplot(aes(x = X,y = Y,group = id)) +
  geom_rect(xmin = 0,xmax = 100,ymin = 67,ymax = 133,fill = "white",col = "black") +
  geom_rect(xmin = 0,xmax = 100,ymin = 133,ymax = 200,fill = "white",col = "black") +
  geom_arc_bar(aes(x0 = 50,y0 = 200,r0 = 0, r = 4.9/0.1525,start = pi/2,end = 3/2*pi), inherit.aes = F) +
  geom_circle(aes(x0 = 50, y0 = 100,r = 0.45/0.1525),col = "black",fill = "white",inherit.aes = F) +
  geom_circle(aes(x0 = 50, y0 = 197,r = 0.19/0.1525),col = "black",fill = "white",inherit.aes = F) +
  geom_line(alpha = 0.3) +
  geom_point(aes(col = position),size = 3, alpha = 0.8) +
  scale_color_brewer(palette = "Spectral") +
  coord_fixed(ylim = c(67,200),clip = "off") +
  theme_void() +
  labs(title = "Hit that circle edge",
       subtitle = "Swifts goal attack second phase passes from top right quadrant",
       caption = "Data courtesy of Aaron Fox (@aaron_s_fox)",
       colour = "Player position") +
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        plot.subtitle = element_text(hjust = 0.5,size = 12),
        plot.caption = element_text(hjust = 1))
  
  
  
  ggplot(aes(x = locx,y = locy,fill = pct,label = lab)) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient(low="green", high="red") +
  facet_wrap(~teamName)

# Pass angle

## Pass angle totals
first_two_passes %>% 
  filter(!is.na(centrePassX),!centrePassRec %in% c("UNKNOWN","GS")) %>% 
  mutate(pass1angle = map2_dbl(centrePassX,centrePassY,~pass_angle(c(.x,.y))),
         pass1angle = if_else(pass1angle > 180,-90 - (pass1angle - 180),pass1angle)) %>%
  ggplot(aes(x = pass1angle, fill = centrePassRec)) + 
  geom_histogram(binwidth = 2) +
  #geom_freqpoly(breaks = seq(-180,180,5),size = 1.1) +
  geom_vline(xintercept = c(50,70,-40,-60)) +
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
  geom_vline(xintercept = c(40,60,-40,-60)) +
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

