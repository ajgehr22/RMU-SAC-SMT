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
SS_ids <- SS_ids[-1]
SS_ids <- SS_ids[SS_ids < 1000]
length(SS_ids)
print(SS_ids)

# collect unique second basemen IDS

game_info_2B <- game_info %>% 
  select(game_str, home_team, away_team, second_base) %>% 
  collect()

SecondB_ids <- unique(game_info_2B$second_base)
SecondB_ids <- SecondB_ids[-1]
SecondB_ids <- SecondB_ids[SecondB_ids < 1000]
length(SecondB_ids)
print(SecondB_ids)

# filter for plays in 1A first day 1883 where the shortstop is involved ***** PRACTICE

shortstop_involved <- game_events %>% 
  filter(Season == "Season_1883",
         HomeTeam == "Home1A",
         Day == "day_001",
         player_position == 6) %>% 
  collect()

shortstop_involved$player_ID <- 0

# add shortstop ID to game info

#*************** DOES NOT WORK ***************#

for(i in 1:length(shortstop_involved$game_str)){
  for(j in 1:length(game_info_SS)){
    if(game_info_SS$game_str[j] == shortstop_involved$game_str[i] & game_info_SS$shortstop[j] < 1000){
      shortstop_involved$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

############## NEXT STEPS ##############
# 
# Find all plays where SS or 2B were involved
# Split plays into routine, intermediate, stellar
# Look at fielding percentage across levels
# Investigate improvements from entire system from year to year
# Investigate improvements for individual players from year to year

