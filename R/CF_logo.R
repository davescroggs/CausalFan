library(tidyverse)
library(patchwork)

# Option 1

tibble(
  x = c(rep(2, 6),1,2,3),
  y = c(6:1,5,5,5),
  label = c("C", " ", "U", "S", "A", "L","F","A","N"),
  word = c(rep("A",6),rep("B",3))
) %>% 
ggplot(aes(x = x,y = y,label = label,col = word)) +
  geom_rect(aes(xmin = 0,xmax = 4,ymin = 0,ymax = 7),col = "black",fill = "#00CD66") +
  geom_text(size = 11,family = "mono") + 
  expand_limits(x = c(-3,8),y = c(0,7)) +
  scale_colour_manual(values = c("A" = "#000000", B = "white")) +
  theme_void() +
  theme(legend.position = "none")


# Option 2

tibble(
  x = c(1:6,2,2,2),
  y = c(rep(2,6),3,2,1),
  label = c("C", " ", "U", "S", "A", "L","F","A","N"),
  word = c(rep("A",6),rep("B",3))
) %>% 
  ggplot(aes(x = x,y = y,label = label,col = word)) +
  geom_rect(aes(xmin = 0,xmax = 7,ymin = 0,ymax = 4),col = "black",fill = "#00CD66") +
  geom_text(size = 11,family = "mono") + 
  expand_limits(x = c(-7,12),y = c(-2,9)) +
  scale_colour_manual(values = c("A" = "#000000", B = "white")) +
  theme_void() +
  theme(legend.position = "none")

# Option 3

dat <- tibble(
  x = c(1:6,2,2,2),
  y = c(rep(2,6),3,2,1),
  label = c("C", " ", "U", "S", "A", "L","F","A","N"),
  word = c(rep("A",6),rep("B",3))
)
dat %>% 
  ggplot(aes(x = x,y = y,label = label,col = word)) +
  geom_rect(aes(xmin = 0,xmax = 7,ymin = 0,ymax = 4),col = "black",fill = "#FFF2D1") +
  geom_text(size = 11,family = "mono") + 
  expand_limits(x = c(-3,8),y = c(0,7)) +
  scale_colour_manual(values = c("A" = "#000000", B = "grey40")) +
  theme_void() +
  theme(legend.position = "none")

# Option 4

dat <- tibble(y = c(10L, 10L, 10L, 10L, 10L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 12L, 12L, 12L, 12L, 12L, 13L, 13L, 13L, 13L, 13L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L),
       x = c(10L, 12L, 2L, 6L, 8L, 10L, 12L, 2L, 3L, 4L, 6L, 7L, 8L, 10L, 12L, 2L, 6L, 8L, 10L, 12L, 2L, 6L, 8L, 10L, 12L, 14L, 15L, 16L, 18L, 19L, 2L, 20L, 22L, 3L, 4L, 6L, 7L, 8L, 10L, 12L, 14L, 18L, 2L, 20L, 22L, 6L, 8L, 10L, 12L, 14L, 15L, 16L, 18L, 19L, 2L, 20L, 22L, 6L, 7L, 8L, 10L, 12L, 16L, 18L, 2L, 20L, 22L, 6L, 8L, 10L, 11L, 12L, 14L, 15L, 16L, 18L, 2L, 20L, 22L, 23L, 24L, 3L, 4L, 6L, 8L, 10L, 11L, 12L, 2L, 3L, 4L, 6L, 7L, 8L)) %>% 
  mutate(y = max(y) - y,
         fill = runif(94),
         fill = findInterval(fill,vec = quantile(fill,probs = seq(0,1,0.15))),
         fill = as.character(fill),
         word = if_else(y > 5,"CAS","FAN"))
  

# 7, 13 is good
dat %>% 
  ggplot(aes(x,y,fill = fill)) +
  geom_tile(color = "black",size = 1.3) +
  scale_fill_brewer(palette = 18) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#0A850A"))

# Option 5

dat %>% 
  ggplot(aes(x,y)) +
  geom_tile(color = "black",fill = "#F49F1C",size = 1.3) +
  scale_fill_brewer(palette = 18) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "midnightblue"))

# Option 6

dat %>% 
  ggplot(aes(x,y)) +
  geom_tile(color = "black",fill = "#FCC729",size = 1.3) +
  scale_fill_brewer(palette = 18) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#337DEF"))

# Option 6

dat %>% 
  ggplot(aes(x,y)) +
  geom_tile(color = "black",fill = "#337DEF",size = 1.3) +
  scale_fill_brewer(palette = 18) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#FCC729"))

plot_func <- function(k){
  dat %>% 
    ggplot(aes(x,y,fill = fill)) +
    geom_rect(xmin = 0,xmax = 25,
              ymin = -2,ymax = 11,col = "black",fill = "white") +
    geom_tile(color = "black",size = 1.3) +
    annotate("text", x = 15,y = 4,label = k) +
    scale_fill_brewer(palette = k) +
    theme_void() +
    theme(legend.position = "none") 
}

plots <- map(1:18,plot_func)

plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]]+plots[[5]] + plots[[6]] + plots[[7]] + plots[[8]] + plots[[9]] + plots[[10]]+plots[[11]] + plots[[12]] + plots[[13]] + plots[[14]] + plots[[15]] + plots[[16]]+plots[[17]] + plots[[18]]
 
dat %>% 
  ggplot(aes(x,y, fill = word)) +
  geom_tile(color = "black",size = 1.3) +
  #scale_fill_brewer(palette = 18) +
  theme_void() +
  theme(legend.position = "none",plot.background = element_rect(fill = "#020A7A")) +
  scale_fill_manual(values = c("CAS" = "#6DD47E","FAN" = "#FFD55A"))
                    
dat %>% 
  mutate(fill = as.character(rbinom(94,1,0.5))) %>% 
  ggplot(aes(x,y, fill = fill)) +
  geom_tile(color = "black",size = 1.3) +
  theme_void() +
  theme(legend.position = "none",plot.background = element_rect(fill = "#020A7A")) +
  scale_fill_manual(values = c("1" = "#6DD47E","0" = "#FFD55A"))

dat %>% 
  mutate(word = as.character(between(y,2,8))) %>%  
  ggplot(aes(x,y, fill = word)) +
  geom_tile(color = "black",size = 1.3) +
  theme_void() +
  theme(legend.position = "none",plot.background = element_rect(fill = "#020A7A")) +
  scale_fill_manual(values = c("TRUE" = "#6DD47E","FALSE" = "#FFD55A"))

dat %>% 
  ggplot(aes(x,y,fill = fill)) +
  geom_tile(color = "black",size = 1.3) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_void() +
  theme(legend.position = "none",plot.background = element_rect(fill = "#020A7A")) 

dat %>% 
  ggplot(aes(x,y,fill = fill)) +
  geom_tile(color = "black",size = 1.3) +
  scale_fill_brewer(palette = 18) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#0A850A"))

# Current leader

p1 <- dat %>% 
  mutate(fill = runif(94),
         fill = findInterval(fill,vec = quantile(fill,probs = seq(0,1,0.15))),
         fill = as.character(fill),
         x = if_else(word == "FAN",as.integer(x + 6),as.integer(x))) %>% 
  ggplot(aes(x,y,fill = fill)) +
  geom_tile(color = "black",size = 1.3) +
  scale_fill_brewer(palette = 17) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black"))

p1 +
  coord_cartesian(clip = "off") +
  expand_limits(x = c(2,24),y = c(-7,9))


p2 <- ggplot() + 
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black"))

p1 + p2 + p2 + p2 + p2 + p2 + plot_layout(nrow = 2)

# -------------------------------------------------------------------------

cf <- tibble(y = c(2, 2, 2, 2, 3, 4, 5, 6, 7, 7, 7, 7, 5, 5, 5, 5, 6, 8, 9, 10, 11, 8, 8, 8), 
                      x = c(2, 3, 4, 5, 2, 2, 2, 2, 2, 3, 4, 5, 5, 6, 7, 8, 5, 5, 5, 5, 5, 6, 7, 8),
                      word = c(rep("C",12),rep("F",12))) %>% 
  mutate(y = max(y) - y)

cf %>% 
  ggplot(aes(x,y,fill = word)) +
  geom_tile(color = "black",size = 1.6) +
  scale_fill_brewer(palette = 17) +
  theme_void() +
  #expand_limits(x = c(2,10),y = c(2,10)) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey25"))

cf %>% 
  mutate(fill = runif(n())) %>% 
  ggplot(aes(x,y,fill = word)) +
  geom_tile(color = "black",size = 1.6) +
  #scale_fill_brewer(palette = 17) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black")) +
  scale_fill_manual(values = c("C" = "#FEC44F","F" = "#CC4C02"))

cf %>% 
  mutate(fill = runif(n()),
         fill = findInterval(fill,vec = quantile(fill,probs = seq(0,1,0.15))),
         fill = as.character(fill)) %>%  
  ggplot(aes(x,y, fill = word)) +
  geom_tile(color = "black",size = 1.3) +
  theme_void() +
  expand_limits(x = c(1,10),y = c(0,9)) +
  theme(legend.position = "none",plot.background = element_rect(fill = "#020A7A")) +
  scale_fill_manual(values = c("C" = "#6DD47E","F" = "#FFD55A"))

cf %>% summarise(max(x),max(y),min(x),min(y))

aut_cols <- RColorBrewer::brewer.pal(n = 8, name = "YlOrBr")
RColorBrewer::display.brewer.pal(n = 8, name = "YlOrBr")

  
cf %>% 
  mutate(fill = as.character(ceiling(runif(n(),0,8)))) %>% 
  ggplot(aes(x,y,fill = fill)) +
  geom_tile(color = "black",size = 1.6) +
 scale_fill_brewer(palette = 17) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black"))
  scale_fill_manual(values = c("#FEE391", "#FEC44F", "#FE9929", "#D95F0E", "#993404"))


# Final Choices -----------------------------------------------------------

  p1 <- dat %>% 
    mutate(word = as.character(between(y,2,8))) %>% 
    ggplot(aes(x,y, fill = word)) +
    geom_tile(color = "black",size = 1.3) +
    theme_void() +
    theme(legend.position = "none",plot.background = element_rect(fill = "#020A7A")) +
    scale_fill_manual(values = c("TRUE" = "#6DD47E","FALSE" = "#FFD55A")) +
    #coord_cartesian(clip = "off") +
    expand_limits(x = c(2,24),y = c(-6,9))
 p1 
  
 p2 <-  data.frame(
    x = c(1, 2),
    y = c(2, 1),
    text = c(
      "","A bit of fun with sports stats,\nnetball mostly.")
  ) %>% 
  ggplot(aes(x, y)) +
    geom_text(aes(label = text), vjust = "inward", hjust = "inward",col = "White",size = 8,family = "mono",fontface = "bold") +
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#020A7A"))
  
  p1 + p2 + plot_layout(widths = c(1, 2))
  
  cf %>% 
    ggplot(aes(x,y,fill = word)) +
    geom_tile(color = "black",size = 1.6) +
    scale_fill_manual(values = c("C" = "#6DD47E","F" = "#FFD55A")) +
    theme_void() +
    expand_limits(x = c(1,9),y = c(2,10)) +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#020A7A"))
  