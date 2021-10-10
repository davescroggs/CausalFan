## 2020 - 2021 signing Period

# Load libraries
library(tidyverse)
library(superNetballR)
library(tidygraph)
library(ggraph)

# Load reference and season data ----

`2021_data` <- readRDS("data/2021_data.RDS")

# Table of team IDs
team_info <- `2021_data` %>% 
  map_dfr(~.x[["teamInfo"]][["team"]]) %>% 
  distinct(squadNickname,squadId)

players_2022 <- readRDS("data/signed_players_2022.RDS") %>% 
  inner_join(team_info)

# Map squad colours
squadCols <- tribble(
  ~squadNickname,	~Primary,	~Secondary,
  "GIANTS"	      , "#f47920",	"#0082ca",
  "Thunderbirds"	, "#ef5091",	"#ffffff",
  "Vixens"	      , "#00a88f",	"#e50e63",
  "Magpies"	  , "#000000",	"#ffffff",
  "Swifts"      , "#ee3124",	"#0093d0",
  "Firebirds"	    , "#381460",	"#fdb515",
  "Lightning"	    , "#782b8f",	"#fdb71d",
  "Fever"	        , "#00953b",	"#000000",
  "No Team",       "#999999", "#595959")

squadPrimaryCols <- squadCols %>%   
  select(-Secondary) %>% 
  deframe()

squadSecondaryCols <- squadCols %>%   
  select(-Primary) %>% 
  deframe()

# Table of team abbreviations
name_abrv <- tibble::tribble(
  ~squadNickname, ~abrv,
  "Fever", "FEV",
  "Firebirds", "FIR",
  "GIANTS", "GIA",
  "Lightning", "LIG",
  "Magpies", "MAG",
  "No Team", "NO",
  "Swifts", "SWI",
  "Thunderbirds", "THU",
  "Vixens", "VIX"
)

# Table of player IDS
player_info <- c(`2021_data`) %>% 
  map_dfr(~.x[["playerInfo"]][["player"]]) %>% 
  select(playerId,displayName) %>%
  bind_rows(players_2022 %>% select(playerId,displayName)) %>% 
  distinct(playerId,.keep_all = T)

players_2021 <- `2021_data` %>% 
  map_dfr(~.x[["playerStats"]][["player"]]) %>% 
  group_by(playerId,squadId) %>% 
  summarise(minutesPlayed = sum(minutesPlayed),
            totalNNP = sum(netPoints)) %>% 
  left_join(team_info, by = "squadId") %>% 
  left_join(player_info, by = "playerId") %>% 
  mutate(season = "2021")


## signing network summary data
signing_network <- full_join(players_2021,players_2022,
                           by = "playerId",
                           suffix = c("_2021","_2022")) %>% 
  mutate(across(c(squadNickname_2021,squadNickname_2022),~if_else(is.na(.x),"No Team",.x))) %>% 
  mutate(displayName_2022 = if_else(is.na(displayName_2022),displayName_2021,displayName_2022)) %>% 
  group_by(squadNickname_2021,squadNickname_2022) %>% 
  summarise(players = list(displayName_2021),
            contracts = length(displayName_2021),
            minutes = sum(minutesPlayed,na.rm = T),
            nnp = sum(totalNNP,na.rm = T),
            nnp = round(nnp,0)) %>% 
  ungroup()

nodes <- signing_network %>%
  distinct(squadNickname_2021) %>% 
  rename("squadNickname" = "squadNickname_2021") %>% 
  mutate(ID = 1:n()) %>% 
  left_join(name_abrv)

edges <- signing_network %>% 
  left_join(nodes,by = c("squadNickname_2021"="squadNickname")) %>% 
  rename("from" = "ID") %>% 
  left_join(nodes,by = c("squadNickname_2022"="squadNickname")) %>% 
  rename("to" = "ID") %>% 
  select(from,to,minutes,contracts,nnp) %>% 
  # Manually set the direction of the edge loops
  left_join(tribble(
    ~abrv, ~direction,
    "FIR",       195L,
    "FEV",         150L,
    "VIX",        45L,
    "LIG",        90L,
    "SWI",       350L,
    "GIA",       35L,
    "MAG",       270L,
    "NO",       270L,
    "THU",       230L
  ) %>% 
    left_join(nodes),by = c("to" = "ID")) %>% 
  mutate(contract_loops = paste("underline(",contracts,")",sep = ""))


# Transfer of minutes played

network <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

ggraph(network, layout = 'stress', circular = T) + 
  # Arrows
  geom_edge_parallel(arrow = arrow(length = unit(2, 'mm')), 
                     start_cap = circle(5, 'mm'),
                     end_cap = circle(5, 'mm')) +
  # Labels
  geom_edge_parallel(aes(label = minutes),
                     edge_alpha = 0,
                     angle_calc = 'along',
                     label_dodge = -unit(3, 'mm'),
                     label_push = unit(8, 'mm')) +
  # Loops
  geom_edge_loop(aes(label = minutes,direction = direction),
                 arrow = arrow(length = unit(2, 'mm')), 
                 start_cap = circle(2, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = 'along',
                 label_dodge = -unit(2.5, 'mm'),
                 force_flip = T) +
  geom_node_point(size = 10,aes(col = squadNickname)) + 
  geom_node_text(aes(label = abrv), repel = F,col = "white") +
  coord_fixed() +
  scale_color_manual(values = squadPrimaryCols) +
  labs(colour = "",
       title = "Player movements - Playing minutes",
       subtitle = "Transfer of 2021 playing minutes as a result of movements",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Number of transfers 

ggraph(network, layout = 'stress', circular = TRUE) + 
  geom_edge_parallel(arrow = arrow(length = unit(2, 'mm')), 
                     start_cap = circle(5, 'mm'),
                     end_cap = circle(5, 'mm'),
                     angle_calc = 'along',
                     label_dodge = -unit(2.5, 'mm'),
                     label_push = unit(10, 'mm')) +
  geom_edge_parallel(aes(label = contracts),
                     edge_alpha = 0,
                     angle_calc = 'along',
                     label_dodge = -unit(3, 'mm'),
                     label_push = unit(10, 'mm')) +
  geom_edge_loop(aes(label = contract_loops,direction = direction),
                 arrow = arrow(length = unit(2, 'mm')), 
                 start_cap = circle(2, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = 'along',
                 label_dodge = -unit(2.5, 'mm'),
                 force_flip = T,
                 label_parse = T) +
  geom_node_point(size = 10,aes(col = squadNickname)) + 
  geom_node_text(aes(label = abrv), repel = F,col = "white") +
  coord_fixed() +
  scale_color_manual(values = squadPrimaryCols) +
  labs(colour = "",
       title = "Player movements - Contracts",
       subtitle = "Number and direction of player movements",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# NNP Traded 

ggraph(network, layout = 'stress', circular = TRUE) + 
  geom_edge_parallel(arrow = arrow(length = unit(2, 'mm')), 
                     start_cap = circle(5, 'mm'),
                     end_cap = circle(5, 'mm'),
                     angle_calc = 'along',
                     label_dodge = -unit(2.5, 'mm'),
                     label_push = unit(10, 'mm')) +
  geom_edge_parallel(aes(label = nnp),
                     edge_alpha = 0,
                     angle_calc = 'along',
                     label_dodge = -unit(3, 'mm'),
                     label_push = unit(10, 'mm')) +
  geom_edge_loop(aes(label = nnp,direction = direction),
                 arrow = arrow(length = unit(2, 'mm')), 
                 start_cap = circle(2, 'mm'),
                 end_cap = circle(5, 'mm'),
                 angle_calc = 'along',
                 label_dodge = -unit(2.5, 'mm'),
                 force_flip = T) +
  geom_node_point(size = 10,aes(col = squadNickname)) + 
  geom_node_text(aes(label = abrv), repel = F,col = "white") +
  coord_fixed() +
  scale_color_manual(values = squadPrimaryCols) +
  labs(colour = "",
       title = "Player movements - Nissan Net Points",
       subtitle = "Movement of NNP",
       caption = "Data: Champion Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


# Player signings table -------------------------------------------------------------

library(gt)

signing_network %>% 
  filter(squadNickname_2021 != squadNickname_2022) %>%
  transmute(`2021 squad` = squadNickname_2021,
            `2022 squad` = squadNickname_2022,
            `Total contracts` = contracts,
            `Total playing minutes` = minutes,
            Players = map_chr(players, ~paste(.x,collapse = ", ")),
            Players = str_replace_all(Players,"\\.","\\. ")) %>%
  filter(!Players == "NA") %>% 
  arrange(desc(`Total contracts`)) %>% 
  # kableExtra::kbl() %>% 
  # kableExtra::kable_styling(full_width = F)
  
  gt::gt(groupname_col = "2021 squad",
         rowname_col = "2022 squad") %>% 
  data_color(
    columns = vars(`Total playing minutes`),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = "YlOrRd",
      domain = NULL
    )
  ) %>% 
  summary_rows(
    groups = T,
    columns = vars(`Total playing minutes`,`Total contracts`),
    fns = list(TOTAL = "sum"),
    formatter = fmt_number,
    decimals = 0,
    use_seps = TRUE
  )  %>% 
  tab_options(column_labels.background.color = "#999999",
    row_group.background.color = "#FFEFDB80",
              table.background.color = "#F5F5F5") %>% 
gt::gtsave(filename = "images/signings_table.png")
# Player Movements Table --------------------------------------------------


players_21_22 <- tibble::tribble(
          ~player,          ~squadNickname, ~season,
           "Kiera Austin",       "Vixens",   2022L,
          "Mwai Kumwenda",       "Vixens",   2022L,
          "Rahni Samason",       "Vixens",   2022L,
           "Kate Moloney",       "Vixens",   2022L,
           "Hannah Mundy",       "Vixens",   2022L,
             "Liz Watson",       "Vixens",   2022L,
              "Kate Eddy",       "Vixens",   2022L,
           "Olivia Lewis",       "Vixens",   2022L,
           "Emily Mannix",       "Vixens",   2022L,
              "Jo Weston",       "Vixens",   2022L,
           "Helen Housby",       "Swifts",   2022L,
        "Kelly Singleton",       "Swifts",   2022L,
            "Sam Wallace",       "Swifts",   2022L,
           "Tayla Fraser",       "Swifts",   2022L,
           "Paige Hadley",       "Swifts",   2022L,
            "Maddy Proud",       "Swifts",   2022L,
            "Allie Smith",       "Swifts",   2022L,
             "Sarah Klau",       "Swifts",   2022L,
     "Teigan O'Shannassy",       "Swifts",   2022L,
           "Maddy Turner",       "Swifts",   2022L,
          "Romelda Aiken",    "Firebirds",   2022L,
           "Gretel Bueta",    "Firebirds",   2022L,
             "Mia Stower",    "Firebirds",   2022L,
           "Lara Dunkley",    "Firebirds",   2022L,
         "Kim Ravaillion",    "Firebirds",   2022L,
           "Gabi Simpson",    "Firebirds",   2022L,
             "Kim Jenner",    "Firebirds",   2022L,
    "Ruby Bakewell-Doran",    "Firebirds",   2022L,
    "Jemma Mi Mi",            "Firebirds",	 2022L,
    "Eboni Usoro-Brown",      "Firebirds",	 2022L,
            "Cara Koenen",    "Lightning",   2022L,
             "Steph Wood",    "Lightning",   2022L,
     "Reilley Batcheldor",    "Lightning",   2022L,
        "Mahalia Cassidy",    "Lightning",   2022L,
           "Annie Miller",    "Lightning",   2022L,
         "Laura Scherian",    "Lightning",   2022L,
      "Kadie-Ann Dehaney",    "Lightning",   2022L,
        "Tara Hinchliffe",    "Lightning",   2022L,
        "Karla Pretorius",    "Lightning",   2022L,
           "Kate Shimmin",    "Lightning",   2022L,
        "Jhaniele Fowler",        "Fever",   2022L,
           "Jess Anstiss",        "Fever",   2022L,
         "Courtney Bruce",        "Fever",   2022L,
          "Sasha Glasgow",        "Fever",   2022L,
         "Verity Charles",        "Fever",   2022L,
             "Rudi Ellis",        "Fever",   2022L,
     "Alice Teague-Neeld",        "Fever",   2022L,
              "Emma Cosh",        "Fever",   2022L,
          "Sunday Aryang",        "Fever",   2022L,
         "Stacey Francis",        "Fever",   2022L,
            "Tippah Dwan", "Thunderbirds",   2022L,
         "Georgie Horjus", "Thunderbirds",   2022L,
       "Lenize Potgieter", "Thunderbirds",   2022L,
         "Tayla Williams", "Thunderbirds",   2022L,
          "Elle McDonald", "Thunderbirds",   2022L,
       "Maisie Nankivell", "Thunderbirds",   2022L,
       "Shamera Sterling", "Thunderbirds",   2022L,
           "Hannah Petty", "Thunderbirds",   2022L,
         "Latanya Wilson", "Thunderbirds",   2022L,
        "Matilda Garrett", "Thunderbirds",   2022L,
          "Sophie Garbin",      "Magpies",   2022L,
            "Ash Brazill",      "Magpies",   2022L,
            "Geva Mentor",      "Magpies",   2022L,
         "Shimona Nelson",      "Magpies",   2022L,
          "Kelsey Browne",      "Magpies",   2022L,
          "Jacqui Newton",      "Magpies",   2022L,
         "Gabby Sinclair",      "Magpies",   2022L,
            "Molly Jovic",      "Magpies",   2022L,
          "Jodi-Ann Ward",      "Magpies",   2022L,
            "Maggie Lind",      "Magpies",   2022L,
           "Sophie Dwyer",       "GIANTS",   2022L,
             "Maddie Hay",       "GIANTS",   2022L,
         "April Brandley",       "GIANTS",   2022L,
              "Jo Harten",       "GIANTS",   2022L,
          "Amy Parmenter",       "GIANTS",   2022L,
        "Matilda McDonell",       "GIANTS",   2022L,
   "Matisse Letherbarrow",       "GIANTS",   2022L,
             "Amy Sligar",       "GIANTS",   2022L,
        "Jamie-Lee Price",       "GIANTS",   2022L,
           "Lauren Moore",       "GIANTS",   2022L
) %>% 
  extract(player,c("A","B"),regex = "(.*?) (.*)",remove = F) %>%
mutate(firstInitial = str_sub(A,start = 1,end = 1),
displayName = paste(firstInitial,".",B,sep = "")) %>%
left_join(team_info) %>%
left_join(player_info,by = c("displayName")) %>%
mutate(playerId = if_else(displayName == "R.Batcheldor",12345678L,playerId),
      playerId = if_else(displayName == "E.Usoro-Brown",192837456L,playerId),
       playerId = if_else(displayName == "A.Unie",987654321L,playerId)) %>%
select(playerId,displayName,squadId,season)
