## Load Season 2020 Data
library(superNetballR)
library(tidyverse)
library(magrittr)

Season_id <- "11665"

game_register <- 
  list(round = 1:6,match = 1:4) %>% 
  cross_df() %>% 
  arrange(round) %$% 
map2(round,match, function(roundNumber,matchNumber) downloadMatch(Season_id,roundNumber,matchNumber))

saveRDS(game_register,file = "data/2022_data.RDS")
