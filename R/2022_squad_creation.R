`2022_squads` <- tibble::tribble(
                ~Fever,            ~Firebirds,                ~GIANTS,           ~Lightning,             ~Magpies,               ~Swifts,      ~Thunderbirds,          ~Vixens,
  "Alice Teague-Neeld",       "Eboni Usoro-Brown",        "Amy Parmenter",       "Annie Miller",        "Ash Brazill",         "Allie Smith",    "Elle McDonald",   "Emily Mannix",
      "Courtney Bruce",        "Gabi Simpson",           "Amy Sligar",        "Cara Koenen", "Gabrielle Sinclair",        "Helen Housby",   "Georgie Horjus",   "Hannah Mundy",
           "Emma Cosh",        "Gretel Bueta",       "April Brandley",  "Kadie-Ann Dehaney",        "Geva Mentor",     "Kelly Singleton",     "Hannah Petty",      "Jo Weston",
        "Jess Anstiss",         "Jemma Mi Mi",      "Jamie-Lee Price",    "Karla Pretorius",      "Jacqui Newton",         "Maddy Proud",   "Latanya Wilson",      "Kate Eddy",
     "Jhaniele Fowler",          "Kim Jenner",            "Jo Harten",       "Kate Shimmin",      "Jodi-Ann Ward",        "Maddy Turner", "Lenize Potgieter",   "Kate Moloney",
          "Rudi Ellis",      "Kim Ravaillion",         "Lauren Moore",     "Laura Scherian",      "Kelsey Browne",        "Paige Hadley", "Maisie Nankivell",   "Kiera Austin",
       "Sasha Glasgow",        "Lara Dunkley",           "Maddie Hay",    "Mahalia Cassidy",        "Maggie Lind",         "Sam Wallace",  "Matilda Garrett",     "Liz Watson",
      "Stacey Francis",          "Mia Stower",     "Matilda McDonell",       "Reilly Batcheldor",        "Molly Jovic",          "Sarah Klau", "Shamera Sterling",  "Mwai Kumwenda",
       "Sunday Aryang",       "Romelda Aiken", "Matisse Letherbarrow",         "Steph Wood",     "Shimona Nelson",        "Tayla Fraser",   "Tayla Williams",   "Olivia Lewis",
      "Verity Charles", "Ruby Bakewell-Doran",         "Sophie Dwyer",    "Tara Hinchliffe",      "Sophie Garbin",  "Teigan O'Shannassy",      "Tippah Dwan",  "Rahni Samason",
     "Shannon Eagland",        "Macy Gardner",       "Jemma Donoghue",       "Sienna Allen",         "Nyah Allen",   "Leilani Rohweder,",      "Lucy Austin", "Ruby Barkmeyer",
      "Donnell Wallam",         "Hulita Veve",          "Clare Iongi",   "Annabelle Lawrie",          "Emma Ryde",      "Olivia Coleman", "Chelsea Blackman",   "Gabby Coffey",
      "Courtney Kruta",                    NA,         "Latika Tombs", "Maddie Hinchcliffe",                   NA,       "Emily Burgess",        "Tyler Orr",               NA,
        "Sloan Burton",                    NA,                     NA,                   NA,                   NA,                    NA,     "Molly Watson",               NA
  )

`2022_squads` %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = -id) %>% 
  filter(!is.na(value)) %>% 
  extract(value,into = c("A","B"),regex = "(.*?) (.*)") %>% 
  mutate(f = str_sub(A,1,1),
         playerName = paste(f,B,sep = ".")) %>% 
  left_join(temp,by = c("playerName" = "displayName")) %>%
  #filter(!is.na(playerId)) %>% 
  select(-c(A,B,f,id,max_ssn)) %>% write_rds(file = "data/signed_players_2022.RDS")

ff <- function(dat){
  
  season <- lubridate::year(dat[[1]][["matchInfo"]][["utcStartTime"]])

dat %>% 
  map_dfr(~.x[["playerInfo"]][["player"]]) %>% 
  distinct(playerId,displayName) %>%
  mutate(season = season) %>% 
  inner_join(dat %>% 
               map_dfr(~.x[["playerStats"]][["player"]]) %>% 
               distinct(squadId,playerId))
}

all_years_player_list <- map_dfr(list(`2017_data`,`2018_data`,`2019_data`,`2020_data`,`2021_data`),ff) %>% 
  group_by(playerId) %>% 
  mutate(last_season = max(season)) %>% 
  ungroup()

ff <- function(dat){
  
  season <- lubridate::year(dat[[1]][["matchInfo"]][["utcStartTime"]])
  
  dat %>% 
    map_dfr(~.x[["playerInfo"]][["player"]]) %>% 
    distinct(playerId,displayName) %>%
    mutate(season = season)
}

`2021_data`%>% 
  map_dfr(~.x[["playerStats"]][["player"]]) %>% 
  distinct(squadId,playerId)

write_rds(all_years_player_list,file = "data/all_years_player_list.RDS")

`2022_squads` %>% 
  mutate(across(.fns = ~replace_na(.x,""))) %>% 
  mutate(`#` = 1:n()) %>% 
  select(`#`,everything()) %>% 
  kableExtra::kbl() %>% kableExtra::kable_styling(full_width = F)
