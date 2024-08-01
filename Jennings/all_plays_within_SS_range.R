## data
### starter code
source("SMT_Data_starter.R")

### SS IDs (only plays where SS touched the ball, including when they were cutoffs)
ss_play_ids <- read.csv("Overall_SS_Involvement.csv")

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
  geom_abline(aes(intercept = 0, slope = -1.9)) +
  geom_abline(aes(intercept = 0, slope = -30))
