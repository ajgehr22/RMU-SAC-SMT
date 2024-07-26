#************************************* PULL, COMMIT, COMMIT, PUSH EVERY TIME YOU EDIT *******************************

## read in starter code

source("C:\\SMT\\2024_SMT_Data_Challenge\\2024_SMT_Data_Challenge\\SMT_Data_starter.R")

## bring in shortstop play IDs and clean up

ss_play_ids <- read.csv("C:\\SMT\\RMU-SAC-SMT\\Gehr\\Overall_SS_Involvement.csv")

## read in player position data for game and play id in ss df

ss_position <- player_pos %>% 
  filter(game_str %in% ss_play_ids$game_str,
         play_id %in% ss_play_ids$play_id,
         player_position == 6) %>% 
  collect()

ss_position$Keep <- 0

## reduce to get game/play pairs in ss df

for(i in 1:153){
  for(j in 1:length(ss_position$game_str)){
    if(ss_play_ids$game_str[i] == ss_position$game_str[j] & ss_play_ids$play_id[i] == ss_position$play_id[j]){
      
      ss_position$Keep[j] <- "Yes"
      
    }
  }
}



