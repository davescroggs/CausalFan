library(wnblr)
data("shots")
library(tidyverse)
library(ggforce)

# Court is 28 x 15m

shots %>% 
  select(x,y) %>% 
  mutate(x = if_else(x > 50,100 - x, x),
         x = x / 100 * 28,
         y = y / 100 * 15) %>% 
  pivot_longer(cols = c(x,y)) %>% 
  ggplot(aes(value,name)) + 
  geom_violin()


add_key <- function() geom_rect(xmin = 0,xmax = 5.8,ymin = 9.95,ymax = 5.05,col = "black",fill = NA)
add_3pt_arc <- function() geom_arc(aes(x0 = 1.575, y0 = 7.5,r = 6.75,start = pi - 0.1862454,end = 0.1862454),col = "black",inherit.aes = F)
add_halfcourt <- function() geom_rect(xmin = 0,xmax = 14,ymin = 0,ymax = 15,col = "black",fill = NA)
add_top_key <- function() geom_arc(aes(x0 = 5.8, y0 = 7.5,r = 1.8,start = pi,end = 0),col = "black",inherit.aes = F)
add_backboard <- function() geom_segment(aes(x = 1.2,xend = 1.2,y = 6.6, yend = 8.4),col = "black")
add_basket <- function() geom_circle(aes(x0 = 1.575, y0 = 7.5,r = 0.45/2),col = "black",fill = NA,inherit.aes = F)
add_centre_circle <- function() geom_arc(aes(x0 = 14, y0 = 7.5,r = 1.8,start = pi, end = 2*pi),col = "black",inherit.aes = F)
add_3ball_segment1 <- function() geom_segment(aes(x = 0,xend = 2.825,y = 0.9,yend = 0.9),inherit.aes = F)
add_3ball_segment2 <- function() geom_segment(aes(x = 0,xend = 2.825,y = 14.1,yend = 14.1),inherit.aes = F)
add_all_half <- function() list(add_backboard(),add_3ball_segment1(),add_centre_circle(),add_basket(),add_3ball_segment2(),add_top_key(),add_halfcourt(),add_3pt_arc(),add_key())

# start_time = Sys.time()
# shots %>%
#   mutate(x = if_else(x > 50,100 - x, x),
#          x = x / 100 * 28,
#          y = y / 100 * 15) %>%
#   ggplot(aes(x,y)) +
#   geom_density_2d_filled(contour_var = "density") +
#   add_3pt_arc() +
#   add_key() +
#   add_top_key() +
#   add_halfcourt() +
#   add_backboard() +
#   add_basket() +
#   add_3ball_segment1() +
#   add_3ball_segment2() +
#   add_centre_circle() +
#   coord_fixed(xlim = c(0, 14),ylim = c(0,15)) +
#   theme_void() +
#   labs(title = "WNBL shot location by season",
#        subtitle = "Counts of different shooting locations for all teams in a given season",
#        caption = "Data: @jacquietran via wnlbr package",
#        fill = "Shot counts") +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         plot.caption = element_text(hjust = 1),
#         legend.position = "None") +
#   facet_wrap(~season)
# 
# Sys.time() - start_time

  

# Effective shooting pct --------------------------------------------------

library(gganimate)
library(transformr)
  
p <- shots %>%
  rowid_to_column() %>% 
  mutate(x = if_else(x > 50, 100 - x, x),
         x = x / 100 * 28,
         y = y / 100 * 15,
         shot_dist = sqrt((x - 1.6)^2 + (y - 7.5)^2 )) %>%
  ggplot(aes(shot_dist,group = factor(season))) +
  geom_density(size = 1.3) +
  geom_vline(xintercept = 6.7,linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(breaks = 1:14) +
  labs(x = "Shot distance (m) from cente of ring",
       y = "Density") +
  annotate("segment", x = 7, xend = 8.5, y = 0.26, yend = 0.26,
           arrow = arrow(ends = "first", angle = 45, length = unit(.2,"cm"))) +
  annotate("text",x = 9.25, y = 0.26,label = "3pt line")
p

anim <- p +
  gganimate::transition_states(season) +
  ggtitle("Shot distribution by distance",subtitle = 'Season {closest_state}') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
anim


get_midpoint <- function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
}


plot_dat <- shots %>%
  mutate(x = if_else(x > 50, 100 - x, x),
         x = x / 100 * 28,
         y = y / 100 * 15,
         shot_dist = sqrt((x - 1.6)^2 + (y - 7.5)^2 ),
         dist_grps = cut_width(shot_dist,0.5),
         dist_mp = map_dbl(dist_grps,get_midpoint),
         shot_result = if_else(shot_result == 1,"make","miss")) %>%
  count(season,dist_mp, shot_result) %>% 
  pivot_wider(names_from = shot_result,values_from = n,values_fill = list(n = 0)) %>% 
  mutate(n = make + miss,
         pct = make/n)

plot_dat %>% 
  filter(n > 10) %>% 
  ggplot(aes(dist_mp,pct)) +
  geom_point(aes(size = n)) +
  geom_path() +
  geom_vline(xintercept = 6.7,linetype = "dashed") +
  scale_y_continuous(breaks = seq(0,0.8,0.1),labels = scales::percent) +
  scale_x_continuous(breaks = 0:14) +
  expand_limits(y = c(0,0.8)) +
  annotate("segment", x = 7, xend = 8.5, y = 0.6, yend = 0.6,
           arrow = arrow(ends = "first", angle = 45, length = unit(.2,"cm"))) +
  annotate("text",x = 9.25, y = 0.6,label = "3pt line") +
  labs(title = "Shooting percentage by distance interval",
       x = "Shot distance (m), in 0.5 m intervals",
       y = "Shooting percentage",
       size = "Total shots",
       caption = "Data: @jacquietran via wnlbr package") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1)) +
  facet_wrap(~season,nrow = 2)


plot_dat %>% 
  filter(n > 10) %>% 
  ggplot(aes(dist_mp,pct,col = factor(season),group = factor(season))) +
  #geom_point(aes(size = n)) +
  geom_path(size = 1.3) +
  geom_vline(xintercept = 6.7,linetype = "dashed") +
  scale_y_continuous(breaks = seq(0,0.8,0.1),labels = scales::percent) +
  scale_x_continuous(breaks = 0:14) +
  scale_color_viridis_d() + 
  expand_limits(y = c(0,0.8)) +
  labs(title = "Shooting percentage by distance interval",
    x = "Shot distance (m), in 0.5 m intervals",
       y = "Shooting percentage",
       colour = "Season",
       caption = "Data: @jacquietran via wnlbr package") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1))


efg_dat <- shots %>%
  mutate(x = if_else(x > 50, 100 - x, x),
         x = x / 100 * 28,
         y = y / 100 * 15,
         shot_dist = sqrt((x - 1.6)^2 + (y - 7.5)^2 ),
         dist_grps = cut_width(shot_dist,0.5),
         dist_mp = map_dbl(dist_grps,get_midpoint),
         shot_result = if_else(shot_result == 1,"make","miss")) %>%
  count(season,dist_mp, shot_result, points_scored) %>% 
  pivot_wider(names_from = c(shot_result,points_scored),values_from = n,values_fill = list(n = 0)) %>% 
  mutate(n = make_2 + make_3 + miss_0,
         eFG = (make_2 * 2 + make_3 *3)/n)
  
efg_dat %>% 
  filter(n > 10) %>% 
  ggplot(aes(dist_mp,eFG, col = factor(season))) +
  geom_path(size = 1.3) +
  geom_vline(xintercept = 6.7,linetype = "dashed") +
  scale_y_continuous(breaks = seq(0,1.5,0.1)) +
  scale_x_continuous(breaks = 0:14) +
  scale_colour_viridis_d() +
  expand_limits(y = c(0,0.8)) +
  annotate("segment", x = 7, xend = 8.5, y = 1.4, yend = 1.4,
           arrow = arrow(ends = "first", angle = 45, length = unit(.2,"cm"))) +
  annotate("text",x = 9.25, y = 1.4,label = "3pt line") +
  labs(title = "Expected points by distance interval",
       subtitle = "ePts = Shooting percentage x points scored",
       x = "Shot distance (m), in 0.5 m intervals",
       y = "Expected points",
       colour = "Season",
       caption = "Data: @jacquietran via wnlbr package") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1))
