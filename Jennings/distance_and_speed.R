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

## tracking SS data
tracking_ss <- player_pos |> 
  filter(player_position == 6) |> 
  collect() |> 
  group_by(game_str, play_id) |> 
  # align timestamps to account for mechanical measurement error
  mutate(timestamp_adj = plyr::round_any(timestamp, fps)) |> 
  # trim the animation to start when the pitch is thrown
  mutate(
    frame_id = match(timestamp_adj, unique(timestamp_adj)),
    position_x = field_x,
    position_y = field_y) |> 
  # ungroup
  ungroup() |> 
  # select columns
  select(
    game_id = game_str,
    play_id,
    frame_id,
    position_x,
    position_y)

## ball tracking data
tracking_ball <- ball_pos |> 
  collect() |> 
  group_by(game_str, play_id) |> 
  # align timestamps to account for mechanical measurement error
  mutate(timestamp_adj = plyr::round_any(timestamp, fps)) |> 
  # trim the animation to start when the pitch is thrown
  mutate(
    frame_id = match(timestamp_adj, unique(timestamp_adj)),
    ) |>
  # ungroup
  ungroup() |> 
  # select columns
  select(
    game_id = game_str,
    play_id,
    frame_id,
    ball_position_x,
    ball_position_y,
    ball_position_z
  ) |> 
  # join the SS tracking data
  left_join(tracking_ss) |> 
  # group by
  group_by(game_id, play_id, frame_id) |> 
  # filter by balls that are line drives or ground balls
  filter(
    ball_position_z <= 11,
    ball_position_y <= 165
  ) |> 
  mutate(
    # distance bewteen SS and ball
    distance = sqrt((position_x - ball_position_x)^2 + (position_y - ball_position_y)^2),
    # distance between now and previous ball location
    # ball_distance = sqrt((ball_position_x - dplyr::lag(ball_position_x, n = 1))^2 + (ball_position_y - dplyr::lag(ball_position_y, n = 1))^2),
    # # estimated speed (velocity) of the ball
    # speed = (ball_distance - dplyr::lag(ball_distance, n = 1)) / 0.1
    event = if_else(ball_position_y < 1, "batted", if_else(distance < 1, "fielded", NA)),
    event = if_else(ball_position_y > position_y + 1, "not_fielded", event)
    ) |> 
  ungroup()
  
# groundball to SS
test <- tracking_ball |> 
  filter(
    game_id == "1883_003_Vis4AB_Home4A", 
    play_id == 4
  )

# pop-up to SS?
test <- tracking_ball |> 
  filter(
    game_id == "1883_002_Vis4AB_Home4A", 
    play_id == 17
  )

# random
test <- tracking_ball |> 
  filter(
    game_id == "1884_116_Vis4BA_Home4A", 
    play_id == 92
  )

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
    player_position == 6
    ) |> 
  collect() |> 
  select(
    game_id = game_str,
    play_id,
    timestamp,
    position_x = field_x, 
    position_y = field_y
  ) |> 
  # add ball position data
  bind_rows(
    (ball_pos |> 
       select(
         game_id = game_str,
         play_id,
         timestamp,
         position_x = ball_position_x,
         ball_position_y,
         ball_position_z
       ))
  )  |>  
  group_by(game_id, play_id, timestamp) |> 
  arrange(timestamp) |> 
  # align timestamps to account for mechanical measurement error
  mutate(
    timestamp_adj = plyr::round_any(timestamp, fps),
    event = if_else(ball_y < 1, "batted", if_else(distance < 1, "fielded", NA)),
    event = if_else(ball_y > position_y + 1, "not_fielded", event),
    time_of_pitch = first(timestamp),
    event = if_else(timestamp == time_of_pitch, "time_of_pitch", event)
    ) |> 
  # trim the animation to start when the pitch is thrown
  #filter(timestamp >= time_of_pitch) |> 
  mutate(frame_id = match(timestamp_adj, unique(timestamp_adj))) |> 
  ungroup()

## calculate distance
### separate ball tracking data
ball_track <- tracking_data |> 
  filter(
    type == "ball"
  )

### add ball x and y
ss_track <- tracking_data |> 
  filter(
    player_position == 6 &
      timestamp %in% ball_track$timestamp
  ) |> 
  mutate(
    ball_x = ball_track$position_x,
    ball_y = ball_track$position_y,
    # euclidean distance
    distance = sqrt((position_x - ball_x)^2 + (position_y - ball_y)^2),
    event = if_else(ball_y < 1, "batted", if_else(distance < 1, "fielded", NA)),
    event = if_else(ball_y > position_y + 1, "not_fielded", event)
  )

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
    position_y <= 165,
    position_z <= 11
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
    event = if_else(ball_position_y < 1, "batted", if_else(distance < 1, "fielded", NA)),
    event = if_else(ball_position_y > position_y + 1, "not_fielded", event)
  )

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
  )

# NAs for before pitch is thrown and after play ends, can drop those because they are not necessary