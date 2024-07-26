#************************************* PULL, COMMIT, COMMIT, PUSH EVERY TIME YOU EDIT *******************************

## read in starter code

#Adam
source("C:\\SMT\\2024_SMT_Data_Challenge\\2024_SMT_Data_Challenge\\SMT_Data_starter.R")

#Josh
source("~/Desktop/Analytics Competitions:Research/SMT 2024/2024_SMT_Data_Challenge/SMT_Data_starter.R")

#Liam input your source that can load the starter code


## bring in shortstop play IDs and clean up

ss_play_ids <- read.csv("C:\\SMT\\RMU-SAC-SMT\\Gehr\\Overall_SS_Involvement.csv")

## read in player position data for game and play id in ss df

ss_position <- player_pos %>% 
  filter(game_str %in% ss_play_ids$game_str,
         play_id %in% ss_play_ids$play_id,
         player_position == 6) %>% 
  collect()

## reduce to get game/play pairs in ss df

ss_play_ids$game_str <- paste(ss_play_ids$game_str, ss_play_ids$play_id, sep = "_")
ss_position$game_str <- paste(ss_position$game_str, ss_position$play_id, sep = "_")

ss_position <- ss_position %>% 
  filter(game_str %in% ss_play_ids$game_str)

