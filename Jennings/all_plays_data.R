#************************************* PULL, COMMIT, COMMIT, PUSH EVERY TIME YOU EDIT *******************************

## read in starter code

#Adam
source("C:\\SMT\\2024_SMT_Data_Challenge\\2024_SMT_Data_Challenge\\SMT_Data_starter.R")

#Josh
source("~/Desktop/Analytics Competitions:Research/SMT 2024/2024_SMT_Data_Challenge/SMT_Data_starter.R")

#Liam
source("SMT_Data_starter.R")

# libraries
library(tidyverse)

## bring in shortstop play IDs and clean up
ss_play_ids <- read.csv("Overall_SS_Involvement.csv")

## read in player position data for game and play id in SS df
ss_position <- player_pos %>% 
  filter(game_str %in% ss_play_ids$game_str,
         play_id %in% ss_play_ids$play_id,
         player_position == 6) %>% 
  collect()

## reduce to get game/play pairs in ss df
ss_play_ids <- ss_play_ids |> 
  mutate(unique_id = paste0(game_str, "_", play_id))

ss_position <- ss_position |> 
  mutate(unique_id = paste0(game_str, "_", play_id))

ss_position <- ss_position %>% 
  filter(unique_id %in% ss_play_ids$unique_id)

## repeat process with ball position data to 
ball_pos_plays <- ball_pos %>% 
  filter(game_str %in% ss_play_ids$game_str,
         play_id %in% ss_play_ids$play_id) %>% 
  collect()

ball_pos_plays <- ball_pos_plays |> 
  mutate(unique_id = paste0(game_str, "_", play_id))

ball_pos_plays <- ball_pos_plays %>% 
  filter(unique_id %in% ss_play_ids$unique_id)

## vector with ball positions
ss_position <- ss_position |> 
  mutate(unique_id = paste0(unique_id, "_", timestamp))

ball_pos_plays <- ball_pos_plays |> 
  mutate(unique_id = paste0(unique_id, "_", timestamp))

all_plays <- ss_position %>% 
  inner_join(ball_pos_plays, by = "unique_id")

## glimpse at all plays dataset
glimpse(all_plays)

## clean dataset
all_plays <- all_plays |> 
  # remove duplicate variables
  select(-c(game_str.y:timestamp.y, Season.y:Day.y)) |> 
  # change variable names
  rename(
    game_str = game_str.x,
    play_id = play_id.x,
    timestamp = timestamp.x,
    season = Season.x,
    home_team = HomeTeam.x,
    away_team = AwayTeam.x,
    day = Day.x
  )

## write to csv
write.csv(all_plays, "all_SS_plays.csv")
