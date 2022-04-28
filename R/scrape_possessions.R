library(tidyverse)
library(rvest)
library(magrittr)

files <- dir(path = "data/html_files/",pattern = ".*html") %>% 
  paste0("data/html_files/",.)


make_table <- function(html_file_path) {

col_names <- 
  read_html(html_file_path) %>%
  html_element(css = '#cd6364_SHELL_grids > div > table') %>%
  html_table(header = F) %>% 
  slice(2) %>% 
  select(1:28) %>% 
  pivot_longer(everything()) %>% 
  select(value,name) %>% 
  deframe()

tbl <- 
  read_html(html_file_path) %>%
  html_element(css = '#cd6364_SHELL_grids > div > table') %>%
  html_table(header = F) %>% 
  slice(3:n()) %>% 
  select(1:28) %>% 
  rename(col_names) %>% 
  mutate(across(c(GPT,R),as.integer))

squad <-  last(tbl$Name)

gpt <- tbl %>% tail(1) %>% pull(GPT)

game_clock <-
  read_html(html_file_path) %>%
  html_element(css = '#cd6364_SHELL_match_status > table > tbody > tr > td.cd6364_status') %>%
  html_text() %>% 
  str_split(pattern = " \\- ",simplify = T)

qtr <- str_extract(game_clock[1],pattern = "\\d") %>% as.integer()

periodSeconds <- str_split(game_clock[2],pattern = ":",simplify = T) %>% 
  as.integer() %>% 
  {first(.) * 60 + last(.)}

o_rebs <- tbl %>% 
  filter(POS %in% c("GS","GA")) %$% 
  sum(R)

tibble(squadName = squad,
       period = qtr,
       periodSeconds = periodSeconds,
       oRebs = o_rebs,
       generalPlayTurnovers = gpt)
}

tbls <- map_dfr(files,make_table) %>% 
  mutate(squadNickname = map_chr(squadName,~str_split(.x," ",simplify = T) %>% last()),
         generalPlayTurnovers = if_else(squadNickname == "Firebirds" & period == 3,4L,generalPlayTurnovers), 
         generalPlayTurnovers = if_else(squadNickname == "Vixens" & period == 3,1L,generalPlayTurnovers)) %>% 
  select(-squadName)

tbls %>% 
  arrange(squadName,period,periodSeconds) %>% 
  group_by(squadName,period) %>%
  mutate(across(c(oRebs,generalPlayTurnovers),~if_else(is.na(lag(.x)), .x, .x - lag(.x))),
         squadNickname = str_split(squadName," ",simplify = T) %>% last(),
         generalPlayTurnovers = if_else(squadNickname == "Firebirds" & period == 3,4,generalPlayTurnovers),
         generalPlayTurnovers = if_else(squadNickname == "Firebirds" & period == 3,4,generalPlayTurnovers))
  

source("R/load_netball_data.R")
load_netball_data(2022)

FIRvVIX <- player_stats %>% 
  filter(round == 1, match == 3) %>% 
  group_by(squadNickname,period) %>% 
    summarise(across(c(generalPlayTurnovers),sum)) %>% 
inner_join(
player_stats %>% 
  filter(round == 1, match == 3,attempts1 > 0) %>%
  group_by(squadNickname,period) %>% 
              summarise(oRebs = sum(rebounds))) %>% 
  mutate(periodSeconds = 900) %>% 
  bind_rows(tbls) %>% 
  arrange(squadNickname,period,periodSeconds,generalPlayTurnovers,oRebs) %>% 
  group_by(squadNickname,period) %>%
  mutate(across(c(oRebs,generalPlayTurnovers),~if_else(is.na(lag(.x)), .x, .x - lag(.x)))) %>% 
  ungroup() %>% 
  left_join(team_info)
