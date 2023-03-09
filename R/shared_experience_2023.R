library(tidyverse)
library(magrittr)
library(gt)
library(geomtextpath)

source("R/load_netball_data.R")
load_netball_data()

theme_set(theme_bw() +
            theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.background = element_rect(colour = "black")))

player_list <- tibble::tribble(~fullName, ~squadNickname,
                          "Lucy Austin",  "Thunderbirds",
                    "Eleanor Cardwell", "Thunderbirds",
                         "Tippah Dwan", "Thunderbirds",
                      "Georgie Horjus", "Thunderbirds",
                    "Maisie Nankivell", "Thunderbirds",
                        "Hannah Petty", "Thunderbirds",
                      "Tayla Williams", "Thunderbirds",
                     "Matilda Garrett", "Thunderbirds",
                    "Shamera Sterling", "Thunderbirds",
                      "Latanya Wilson", "Thunderbirds",
                          "Nyah Allen",      "Magpies",
                       "Sophie Garbin",      "Magpies",
                      "Shimona Nelson",      "Magpies",
                          "Ash Brazill",      "Magpies",
                       "Kelsey Browne",      "Magpies",
                         "Molly Jovic",      "Magpies",
                   "Maddie Hinchliffe",      "Magpies",
                         "Geva Mentor",      "Magpies",
                       "Jodi-Ann Ward",      "Magpies",
                       "Jacqui Newton",      "Magpies",
                        "Sophie Dwyer",       "GIANTS",
                           "Jo Harten",       "GIANTS",
                "Matisse Letherbarrow",       "GIANTS",
                          "Maddie Hay",       "GIANTS",
                       "Amy Parmenter",       "GIANTS",
                     "Jamie-Lee Price",       "GIANTS",
                          "Amy Sligar",       "GIANTS",
                      "April Brandley",       "GIANTS",
                   "Matilda McDonell",       "GIANTS",
                        "Lauren Moore",       "GIANTS",
                        "Kiera Austin",       "Vixens",
                       "Mwai Kumwenda",       "Vixens",
                       "Rahni Samason",       "Vixens",
                           "Kate Eddy",       "Vixens",
                        "Kate Moloney",       "Vixens",
                        "Hannah Mundy",       "Vixens",
                          "Liz Watson",       "Vixens",
                        "Emily Mannix",       "Vixens",
                        "Olivia Lewis",       "Vixens",
                           "Jo Weston",       "Vixens",
                        "Sophie Fawns",       "Swifts",
                        "Helen Housby",       "Swifts",
                         "Sam Wallace",       "Swifts",
                        "Tayla Fraser",       "Swifts",
                        "Paige Hadley",       "Swifts",
                         "Maddy Proud",       "Swifts",
                         "Allie Smith",       "Swifts",
                          "Sarah Klau",       "Swifts",
                  "Teigan O'Shannassy",       "Swifts",
                        "Maddy Turner",       "Swifts",
                          "Mia Stower",    "Firebirds",
                      "Donnell Wallam",    "Firebirds",
                        "Lara Dunkley",    "Firebirds",
                        "Macy Gardner",    "Firebirds",
                      "Kim Ravaillion",    "Firebirds",
                        "Gabi Simpson",    "Firebirds",
                 "Ruby Bakewell-Doran",    "Firebirds",
                           "Remi Kamo",    "Firebirds",
                            "Ashlee Unie",    "Firebirds",
                "Gretel Bueta*", "Firebirds",
                         "Cara Koenen",    "Lightning",
                          "Steph Wood",    "Lightning",
                        "Charlie Bell",    "Lightning",
                     "Mahalia Cassidy",    "Lightning",
                        "Annie Miller",    "Lightning",
                      "Laura Scherian",    "Lightning",
                   "Kadie-Ann Dehaney",    "Lightning",
                     "Tara Hinchliffe",    "Lightning",
                     "Karla Pretorius",    "Lightning",
                      "Ashleigh Ervin",    "Lightning",
                     "Jhaniele Fowler",        "Fever",
                       "Sasha Glasgow",        "Fever",
                   "Alice Teague-Neeld",        "Fever",
                        "Jess Anstiss",        "Fever",
                      "Verity Simmons",        "Fever",
                           "Emma Cosh",        "Fever",
                       "Sunday Aryang",        "Fever",
                      "Courtney Bruce",        "Fever",
                          "Rudi Ellis",        "Fever",
                          "Kim Jenner",        "Fever"
                ) 
  
lists_2023 <- 
  player_list %>% 
inner_join(player_info %>% 
               transmute(fullName = paste(firstname, surname), playerId),
             by = join_by(fullName),
             multiple = "all") %>% 
  distinct(playerId, .keep_all = T)


# Team lists --------------------------------------------------------------

table_format_custom <- function(data){
  data %>%
    fmt_markdown(columns = everything()) %>% 
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = 10
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left") 
}

player_list %>% 
  group_by(squadNickname) %>% 
  mutate(id = 1:n()) %>% 
  pivot_wider(names_from = squadNickname, values_from = fullName) %>% 
  select(-id) %>% 
  gt::gt() %>% 
  tab_source_note(
    source_note = md("SOURCE: Netball Scoop<br>*Bueta will miss the 2023 season due to pregnancy with her replacement yet to be named.")
  ) %>% 
  table_format_custom()

player_combinations <-
  player_stats %>%
  distinct(squadNickname, playerId, season, round) %>% 
  transmute(season,
            round, 
            squadNickname,
            playerId) %>%
  bind_rows(lists_2023 %>%
              transmute(season = 2023, round = 1, playerId, squadNickname)) %>% 
  arrange(season, round, squadNickname, playerId) %>%
  {full_join(.,.,by = c("season", "round", "squadNickname"), multiple = "all")} %>%
  filter(playerId.x < playerId.y) %>%
  mutate(player_cross = paste(playerId.x, playerId.y, sep = "_")) %>% 
  count(season, squadNickname, playerId.x, playerId.y, round) %>% 
  mutate(n = if_else(season == 2023, 0, n)) %>%
  arrange(squadNickname,season,round) %>%
  group_by(playerId.x, playerId.y) %>%
  mutate(shared_exp = cumsum(n)) %>%
  ungroup()


player_combinations %>%
  count(season, squadNickname,round, wt = shared_exp) %>% 
  group_by(season, squadNickname) %>% 
  slice_max(n,n = 1) %>% 
  ungroup() %>% 
  ggplot(aes(x = season, y = n, col = squadNickname)) +
  geom_line(data = ~filter(., season < 2023), linewidth = 1.3) +
  geom_point(data = ~filter(., season < 2023), size = 3) +
  geom_line(data = ~filter(., season > 2021), linewidth = 1.3, linetype = 2) +
  geom_point(data = ~filter(., season > 2021), size = 3) +
  scale_colour_manual(values = SquadName_Colours) +
  scale_x_continuous(breaks = 2009:2023) +
  geomtextpath::geom_labelvline(xintercept = 2017, linetype = 2, label = "First Suncorp season", hjust = 0.05) +
  labs(x = "Season",
       y = "Shared experience",
       col = "Team",
       title = "Projected shared experience for 2023 SSN season",
       subtitle = "Dashed line shows SE at start of 2023 season",
       caption = "Data: Champion data") +
  expand_limits(y = 0)


# Losses ------------------------------------------------------------------


player_progression <- player_stats %>% 
  distinct(season, playerId, squadNickname) %>% 
  bind_rows(lists_2023 %>% 
              transmute(season = 2023, playerId, squadNickname)) %>% 
  group_by(playerId) %>% 
  arrange(playerId, season) %>% 
  mutate(team_progression = dplyr::consecutive_id(squadNickname),
         last_ssn = if_else(team_progression == lag(team_progression) | is.na(lag(team_progression)), "Stayed", "Moved"),
         next_ssn = case_when(
           team_progression != lead(team_progression) ~ "Moving",
           team_progression == lead(team_progression) ~ "Staying",
           is.na(lead(team_progression)) ~ "No club",
           TRUE ~ "Err")) %>% 
  ungroup()

# 80301 and 1019167 are duplicates due to replacing injured/COVID affected players  

player_combinations %>% 
  left_join(player_progression %>% 
              transmute(season,
                        playerId,
                        next_ssn.x = next_ssn),
            by = c("season", "playerId.x" = "playerId"),
            multiple = "all") %>% 
  left_join(player_progression %>% 
              transmute(season,
                        playerId,
                        next_ssn.y = next_ssn),
            by = c("season", "playerId.y" = "playerId"),
            multiple = "all") %>% 
  mutate(lost_se = if_else(next_ssn.x != "Staying" | next_ssn.y != "Staying", "Lost", "Retained")) %>% 
  count(season, squadNickname,round, lost_se, wt = shared_exp) %>% 
  add_count(season, squadNickname, round, wt = n, name = "total") %>% 
  group_by(season, squadNickname) %>% 
  slice_max(total,n = 1) %>% 
  ungroup() %>% 
  filter(season == 2022) %>% View
  ggplot(aes(x = n, y = fct_reorder(squadNickname, total), fill = lost_se)) +
  geom_col(position = position_stack())


player_progression %>% 
  filter(season > 2021) %>% 
  arrange(-season) %>% 
  distinct(playerId, .keep_all = T) %>% 
  inner_join(player_info %>% 
               distinct(playerId, .keep_all = T) %>% 
               transmute(playerId, full_name = paste(firstname, surname))) %>% 
  arrange(last_ssn, next_ssn, -season) %>% 
  # inner_join(player_stats %>% 
  #              group_by(playerId, squadNickname) %>% 
  #              summarise(games = n_distinct(season,round))) %>% 
  inner_join(player_stats %>% 
  filter(season == 2022) %>% 
  count(playerId, squadNickname, wt = minutesPlayed, name = "total_minutes"),
  by = join_by(playerId, squadNickname)) %>% 
  add_count(season, squadNickname, wt = total_minutes, name = "total_games") %>% 
  mutate(total_minutes = if_else(season == 2023 & last_ssn == "Stayed", total_minutes, -total_minutes),
         full_name = tidytext::reorder_within(full_name, total_minutes, squadNickname))  %>% 
  mutate(minutes_lost = sum(total_minutes[total_minutes < 0]), .by = c(season, squadNickname)) %>%
  mutate(squadNickname = fct_reorder(squadNickname, minutes_lost, sum),
         contract_status = if_else(season == 2023 & last_ssn == "Stayed", "Full contract", "Un-contracted")) %>% 
  ggplot(aes(x = total_minutes, y = full_name, fill = contract_status)) +
  geom_col() +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#1874CD", "#FF3030")) +
  labs(x = "Total 2022 playing mintues",
       y = "",
       title = "Carry-over of 2022 playing minutes",
       fill = "Contract status",
       subtitle = "Playing minutes of players expected in each team's contracted 10",
       caption = "Data: Champion data\nNote: For the sake of the figure, Bueta has been classified as un-contracted") +
  facet_wrap(~squadNickname, scales = "free_y", ncol = 2)



# The Wallace effect ------------------------------------------------------

player_combinations %>% 
  filter(playerId.x == 1001944 | playerId.y == 1001944, season == 2023) %>% 
  summarise(se = sum(shared_exp))

player_stats %>% 
  filter(season == 2022, squadNickname == "Swifts") %>% 
  distinct(round, playerId, surname) %>% 
  add_count(playerId, name = "total_games") %>% 
  arrange(round,-total_games) %>% 
  pivot_wider(names_from = round, values_from = surname, values_fill = list(surname = "")) %>% 
  select(everything(), -total_games,-playerId) %>% 
  gt() %>% 
  tab_source_note(
    source_note = md("")
  ) %>% 
  table_format_custom

rnd_lvls <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "2023_R1")

player_combinations %>% 
  left_join(player_progression %>% 
              transmute(season,
                        playerId,
                        next_ssn.x = next_ssn),
            by = c("season", "playerId.x" = "playerId"),
            multiple = "all") %>% 
  left_join(player_progression %>% 
              transmute(season,
                        playerId,
                        next_ssn.y = next_ssn),
            by = c("season", "playerId.y" = "playerId"),
            multiple = "all") %>% 
  mutate(lost_se = if_else(next_ssn.x != "Staying" | next_ssn.y != "Staying", "Lost", "Retained")) %>% 
  count(season, squadNickname,round, lost_se, wt = shared_exp) %>% 
  add_count(season, squadNickname, round, wt = n, name = "total") %>% 
  group_by(season, squadNickname) %>% 
  filter(season > 2021, squadNickname == "Swifts") %>% 
  distinct(round, total) %>% 
    mutate(round = as.character(round),
           round = if_else(season == 2023,"2023_R1", round),
           round = factor(round, levels = rnd_lvls, ordered = TRUE)) %>% 
  ggplot(aes(x = round, y = total, group = 1)) +
  #geom_point(data = ~filter(., round == "13"), size = 5, col = "red", shape = 21, stroke = 2) +
  geom_point() +
  ## Indicate loss of Wallace and Hadley in Rnd 2
  annotate("textsegment", x = 1.3, xend = 2.1, y = 1209 - 30, yend = 582 + 100, label = "Wallace, Hadley out", 
           color = "red",
           arrow = arrow(ends = "last", type = "closed", length = unit(0.01, "npc")), size = 3) +
  ## Show max SE for season
  annotate("segment", x = 12.9, y = 1340, xend = 12, yend = 1115, 
           arrow = arrow(ends = "first", type = "closed", length = unit(0.01, "npc"))) +
  annotate("text", x = 12,y = 1090, label = "  Max SE for season", hjust = 0.5) +
  ## Indicate loss of O'Shannesey, Fraser
  annotate("textsegment", x = 13.1, xend = 14.3, y = 1374 + 100, yend = 1046 + 10, label = "Fraser,\n O'Shannassy out", 
           color = "red",
           arrow = arrow(ends = "last", type = "closed", length = unit(0.01, "npc")), size = 3) +
  # Wallace back in
  # annotate("segment", x = 14.9, y = 1570, xend = 12, yend = 1500, 
  #          arrow = arrow(ends = "first", type = "closed", length = unit(0.01, "npc"))) +
  # annotate("text", x = 12,y = 1500, label = "  Wallace back in", hjust = 1) +
  annotate("textsegment", x = 14.5, xend = 15.3, y = 1046 + 100, yend = 1588 - 30, label = "Wallace in", 
           color = "green4",
           arrow = arrow(ends = "last", type = "closed", length = unit(0.01, "npc")), size = 3) +
  # Lineup consistency 
  annotate("textsegment", x = 3.5, xend = 12.5, y = 900, yend = 1305, label = "Consistent line-up", 
                  color = "green4",
           arrow = arrow(ends = "last", type = "closed", length = unit(0.01, "npc"))) +
  #scale_x_continuous(breaks = 1:14) +
  geom_textvline(xintercept = 14.5, label = "2023 season", linetype = 2) +
  geom_path(linetype = 1) +
  scale_x_discrete(labels = as.character(c(1:14,1))) +
  labs(x = "Round",
       y = "Team shared experience",
       title = "Swifts 2022 Shared Experience",
       subtitle = "Shared experience over the 2022 season",
       caption = "Data: Champion Data") +
  expand_limits(y = 0)
