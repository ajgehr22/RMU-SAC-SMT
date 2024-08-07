## data
### starter code
source("SMT_Data_starter.R")

### SS IDs (only plays where SS touched the ball, including when they were cutoffs)
ss_play_ids <- read.csv("Overall_SS_Involvement.csv")

## libraries
library(sportyR)
library(gganimate)


## feature engineering
# sometimes the frames per second at different stadiums can vary (30 fps vs 50 fps)
# this finds an even rounding interval and calculates fps from the data explicitly
fps <- player_pos |> 
  filter(
    player_position == 6
  ) |> 
  collect() |> 
  mutate(
    fps = timestamp - lag(timestamp), 
    .by = "player_position"
  ) |> 
  count(fps) |> 
  slice_max(n) |> 
  pull(fps)

# Combine data into one data frame
tracking_data <- player_pos |> 
  # start with player position data
  filter(
      player_position == 6) |> 
  collect() |> 
  mutate(
    type = if_else(player_position %in% c(10:13), "batter", "fielder"),
    position_z = NA
  ) |> 
  rename(
    position_x = field_x, 
    position_y = field_y
  ) |> 
  # add ball position data
  bind_rows(
    (ball_pos |> 
       collect() |> 
       rename(
         position_x = ball_position_x,
         position_y = ball_position_y,
         position_z = ball_position_z
       ) |> 
       mutate(
         type = "ball", 
         player_position = NA)
    )
  )  |>  
  arrange(timestamp) |> 
  # align timestamps to account for mechanical measurement error
  mutate(timestamp_adj = plyr::round_any(timestamp, fps)) |> 
  # trim the animation to start when the pitch is thrown
  #filter(timestamp >= time_of_pitch) |> 
  mutate(frame_id = match(timestamp_adj, unique(timestamp_adj)))

## calculate distance
### separate ball tracking data
ball_track <- tracking_data |> 
  filter(
    type == "ball",
  ) |> 
  select(
    game_id = game_str,
    play_id,
    timestamp,
    timestamp_adj,
    frame_id,
    ball_position_x = position_x,
    ball_position_y = position_y,
    ball_position_z = position_z
  )
  

### add ball x and y
ss_track <- tracking_data |> 
  filter(
    player_position == 6
  ) |> 
  rename(
    game_id = game_str
  ) |> 
  left_join(ball_track, by = c(
    "game_id",
    "play_id",
    "timestamp",
    "timestamp_adj",
    "frame_id")) |> 
  mutate(
    # euclidean distance
    distance = sqrt((position_x - ball_position_x)^2 + (position_y - ball_position_y)^2),
    # event column for when the ball is hit/fielded/not fielded
    event = if_else(ball_position_y < 1, "batted", if_else(distance < 1, "fielded", NA)),
    event = if_else(ball_position_y > position_y + 1, "not_fielded", event),
    game_play_id = paste0(game_id, "_", play_id)
  ) |> 
  # NAs for before pitch is thrown and after play ends
  drop_na(c(ball_position_x))

# previous run through
test <- tracking_ball |> 
  filter(
    game_id == "1884_116_Vis4BA_Home4A", 
    play_id == 92
  )

# current data
test <- ss_track |> 
  filter(
    game_id == "1884_116_Vis4BA_Home4A", 
    play_id == 92
  ) |> 
  drop_na(c(ball_position_x))

# filter for plays to SS
ss_filters <- ss_track |> 
  mutate(
    game_play_id = paste0(game_id, "_", play_id)
    ) |> 
  group_by(
    game_play_id
    ) |> 
  summarise(
    max_ball_y = max(ball_position_y),
    max_ball_z = max(ball_position_z),
    avg_ball_x = mean(ball_position_x),
    max_pos_x = max(position_x),
    min_ball_y = min(ball_position_y)
  ) |> 
  filter(
    max_ball_y <= 165,
    max_ball_z <= 11,
    avg_ball_x <= 0,
    max_pos_x <= 4,
    min_ball_y >= -1
    )

# apply filters
ss_track_clean <- ss_track |> 
  filter(game_play_id %in% ss_filters$game_play_id)
