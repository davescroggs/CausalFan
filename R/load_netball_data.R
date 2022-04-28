# Load data

load_netball_data <- function(years = 2009:2022) {
  load(file = "data/all_seasons_data.RData")

  goals <<- goals %>% 
    filter(season %in% years)
  
  player_info <<- player_info %>% 
    filter(season %in% years)
  
  player_stats <<- player_stats %>% 
    filter(season %in% years)
  
  player_match_stats <<- player_match_stats %>% 
    filter(season %in% years)
  
  schedule <<- schedule %>% 
    filter(season %in% years)
  
  subs <<- subs %>% 
    filter(season %in% years)
  
  team_stats <<- team_stats %>% 
    filter(season %in% years)
  
  team_info <<- team_info
    
  SquadName_Colours <<- SquadName_Colours
}
