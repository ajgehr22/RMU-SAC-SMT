## data
### starter code
source("~/Desktop/Analytics Competitions:Research/SMT 2024/2024_SMT_Data_Challenge/SMT_Data_starter.R")

### SS IDs (only plays where SS touched the ball, including when they were cutoffs)
ss_play_ids <- read.csv("~/Desktop/Analytics Competitions:Research/SMT 2024/RMU_SAC_SMT/Gehr/Overall_SS_Involvement.csv")

## libraries
library(tidyverse)
library(sportyR)

## get ball position data where SS could have reasonably got it
# radius around the SS 
# see if ball gets within radius
# distance EDA
# radius going up for z coordinate
# cone is hard technically
# r shiny app
# scale it from 0 to 100 (distance)
ball_pos_plays <- ball_pos %>% 
  filter(
    # can justify that without code 
    ball_position_z <= 11 &
      ball_position_x <= 0 &
      ball_position_y <= 165
  ) %>% 
  collect()

sportyR::geom_baseball(display_range = "infield", league = "MiLB") +
  geom_point(aes(x = -47, y = 140), color = "firebrick", size = 4) +
  geom_point(aes(x = -30, y = 60), color = "black", size = 4) +
  geom_point(aes(x = -78, y = 150), color = "black", size = 4) +
  geom_abline(aes(intercept = 0, slope = -1.9)) +
  geom_abline(aes(intercept = 0, slope = -30))

all_SS_plays |> 
  filter(
    field_x <= 0 &
      ball_position_x <= 0
  ) |> 
  group_by(
    game_str,
    play_id
  ) |> 
  summarize(
    mean_position_z = mean(ball_position_z),
    mean_position_y = mean(ball_position_y),
    mean_position_x = mean(ball_position_x)
  ) |> 
  filter(
    mean_position_z <= 11 &
      mean_position_y <= 165 & 
      mean_position_x >= -78
  )|> 
  select(game_str, play_id) |> 
  slice_sample(n = 10)

# example play sequence
animate_play("1883_003_Vis4AB_Home4A", 4)


# Set the specs for the gif we want to create (lower res to make it run quicker)
options(gganimate.dev_args = list(width = 3, height = 3, units = 'in', res = 120))

# sometimes the frames per second at different stadiums can vary (30 fps vs 50 fps)
# this finds an even rounding interval and calculates fps from the data explicitly
fps <- player_pos |> 
  filter(
    game_str == "1883_003_Vis4AB_Home4A" &
      play_id == 4 &
      player_position < 14
  ) |> 
  collect() |> 
  mutate(
    fps = timestamp - lag(timestamp), 
    .by = "player_position"
  ) |> 
  count(fps) |> 
  slice_max(n) |> 
  pull(fps)

# first frame (time of pitch)
time_of_pitch <- game_events |> 
  filter(
    game_str == "1883_003_Vis4AB_Home4A" &
      play_id == 4
  ) |> 
  collect() |> 
  select(timestamp) |> 
  slice_head(n = 1) |> 
  pull(timestamp)

# Combine data into one data frame
tracking_data <- player_pos |> 
  # start with player position data
  filter(
    game_str == "1883_003_Vis4AB_Home4A" &
      play_id == 4 &
      player_position < 14) |> 
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
       filter(game_str == "1883_003_Vis4AB_Home4A", play_id == 4) |> 
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
  filter(timestamp >= time_of_pitch) |> 
  mutate(frame_id = match(timestamp_adj, unique(timestamp_adj)))

# make field design
p <-  geom_baseball(league = "MiLB", display_range = "infield") +
  geom_point(data = tracking_data |> 
               filter(type != "ball"),
             aes(x = position_x, y = position_y, fill = type),
             shape = 21, size = 3,
             show.legend = F) +
  geom_text(data = tracking_data |> 
              filter(type == "fielder"),
            aes(x = position_x, y = position_y, label = player_position),
            color = "black", size = 2,
            show.legend = F) +
  geom_point(data = tracking_data |> 
               filter(type == "ball"),
             aes(x = position_x, y = position_y,
                 size = position_z),
             fill = "white",
             shape = 21,
             show.legend = F) +
  transition_time(frame_id) +
  annotate("text", x = c(150, 0), y = c(10, 400), color = "white",
           label = c(paste("Play:", 4), paste("Game ID:", "1883_003_Vis4AB_Home4A"))) +
  shadow_wake(0.1, exclude_layer = c(1:16))

max_frame <- max(tracking_data$frame_id)

animate(p, fps = fps * 1.5, nframes = max_frame)

## calculate distance
tracking_data |> 
  filter(player_position %in% c(6, NA)) |> 
  mutate(distance = )

ball_track <- tracking_data |> 
  filter(
    type == "ball"
  )

ss_track <- tracking_data |> 
  filter(
    player_position == 6 &
      timestamp %in% ball_track$timestamp
  ) |> 
  mutate(
    ball_x = ball_track$position_x,
    ball_y = ball_track$position_y,
distance = dist(
  c(field_x, field_y),
  c(ball_x, ball_y))
)

ball_track <- tracking_data |> 
  filter(
    type == "ball"
  )

sqrt(sum((ss_track[,c("position_x", "position_y")] - ball_track[,c("position_x", "position_y")]) ^ 2))
