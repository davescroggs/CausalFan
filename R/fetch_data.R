## Load Season 2020 Data
library(superNetballR)
library(tidyverse)
library(magrittr)

# Season ids
# 2023 = 12045
# 2022 = 11665
# 2021 = 11391
# 2020 = 11108
# 2019 = 10724
# 2018 = 10393
# 2017 = 10083
# 2016 = 9818
# 2015 = 9563
# 2014 = 9084
# 2013 = 8035
# 2012 = 8028
# 2011 = 8018
# 2010 = 8012
# 2009 = 8005

## Load Season 2020 Data
library(superNetballR)
library(tidyverse)
library(magrittr)

season_id = c(12045,11665,11391,11108,10724,10393,10083,9818,9563,9084,8035,8028,8018,8012,8005)

game_register <- 
  expand_grid(comp_id = Season_ids,round_id = 1:14,game_id = 1:4) %>% 
  arrange(game_id) %>% 
  pmap(downloadMatch)

saveRDS(game_register,file = "data/all_seasons_raw.RDS")

