## EDA

## libraries
library(tidyverse)

## data
all_ss_plays <- read.csv("all_SS_plays.csv")

## glimpse
glimpse(all_ss_plays)

## str
str(all_ss_plays)

summary(all_ss_plays)

## example play sequence
animate_play("1883_003_Vis1AB_Home1A", 4)

## figure out way to bin movements (e.g., 3-5 feet to left is medium left)