init_phase = tibble(cp = "A",poss = "A",o = NA)

sim_poss <- function(df,x){
      
      if(tail(df,1)$poss == "A"){
        outcome <- rbinom(1,2,c(0.6,0.1,0.3))
      } else {
        outcome <- rbinom(1,2,c(0.1,0.1,0.8))
      }
      
      
      outcome = case_when(
        outcome == 0 ~ "miss",
        outcome == 1 ~ "make",
        outcome == 2 ~ "to")
      
      if(nrow(df) > 1){
        df[nrow(df),3] = outcome
        curr_cp = df %>% tail(1) %>% pull(cp)
        curr_poss = df %>% tail(1) %>% pull(poss)
      } else {
        df$o = outcome
        curr_cp = df$cp
        curr_poss = df$poss
      }
      
      
      if(outcome == "make"){
        next_cp = switch (curr_cp,
                        A = "B",
                        B = "A")
        next_pos = next_cp
      } else{
        next_cp = curr_cp
        next_pos = switch (curr_poss,
                           A = "B",
                           B = "A")
      }
      df <- df %>% add_row(cp = next_cp,
                     poss = next_pos,
                     o = outcome)
      return(df)
    }


game_sim <- reduce(1:30,sim_poss,.init = init_phase)

game_sim %>% count(poss)

game_sim %>% count(poss,o)

game_sim %>% 
  group_by(poss) %>% 
  mutate(i = 1:n()) %>%
  ungroup() %>% 
  mutate(pos_a = if_else(poss == "A",1,0) %>% cumsum(),
         pos_b = if_else(poss == "B",1,0) %>% cumsum(),
         goal_a = if_else(poss == "A" & o == "make",1,0) %>% cumsum(),
         goal_b = if_else(poss == "B" & o == "make",1,0) %>% cumsum())
  ggplot(aes(n,i,col = poss)) +
  geom_step()

  