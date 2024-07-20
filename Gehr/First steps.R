#************************************* PULL, COMMIT, COMMIT, PUSH EVERY TIME YOU EDIT *******************************

## read in starter code

source("C:\\SMT\\2024_SMT_Data_Challenge\\2024_SMT_Data_Challenge\\SMT_Data_starter.R")

#************* Project Idea *************#
#
# Look into the differences in range/defensive efficiency of middle infielders across the various minor league levels
# Investigate differences between second basemen and shortstops
# Investigate improvements of players that get called up
#
#****************************************#

# collect unique shortstop IDs

game_info_SS <- game_info %>% 
  select(game_str, home_team, away_team, shortstop) %>% 
  collect()

SS_ids <- unique(game_info_SS$shortstop)
SS_ids <- na.omit(SS_ids)
SS_ids <- SS_ids[SS_ids < 1000]
length(SS_ids)
print(SS_ids)

game_info_SS <- na.omit(game_info_SS)

# collect unique second basemen IDS

game_info_2B <- game_info %>% 
  select(game_str, home_team, away_team, second_base) %>% 
  collect()

SecondB_ids <- unique(game_info_2B$second_base)
SecondB_ids <- na.omit(SecondB_ids)
SecondB_ids <- SecondB_ids[SecondB_ids < 1000]
length(SecondB_ids)
print(SecondB_ids)

game_info_2B <- na.omit(game_info_2B)

# filter for plays in 1A first day 1883 where the shortstop is involved ***** PRACTICE

#shortstop_involved_practice <- game_events %>% 
 # filter(Season == "Season_1883",
  #       HomeTeam == "Home1A",
   #      Day == "day_001",
    #     player_position == 6) %>% 
  #collect()

#shortstop_involved_practice$player_ID <- 0

# filter for all 1A plays 1883 where the shortstop is involved

shortstop_involved_1A <- game_events %>% 
  filter(Season == "Season_1883",
         HomeTeam == "Home1A",
         player_position == 6) %>% 
  collect()

shortstop_involved_1A$player_ID <- 0

# add shortstop ID to game info for 1A

for(i in 1:340){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_1A$game_str[i] & game_info_SS$shortstop[j] < 1000){
        
        shortstop_involved_1A$player_ID[i] <- game_info_SS$shortstop[j]
        
    }
  }
}

for(i in 341:length(shortstop_involved_1A$game_str)){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_1A$game_str[i] & game_info_SS$shortstop[j] < 1000){
      
      shortstop_involved_1A$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

# Repeat for 2A, 3A, 4A as well as 1884

############## NEXT STEPS ##############
# 
# Find all plays where SS or 2B were involved
# Split plays into routine, intermediate, stellar
# Look at fielding percentage across levels
# Investigate improvements from entire system from year to year
# Investigate improvements for individual players from year to year

