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
  select(game_str, home_team, away_team, at_bat, shortstop) %>% 
  collect()

SS_ids <- unique(game_info_SS$shortstop)
SS_ids <- na.omit(SS_ids)
SS_ids <- SS_ids[SS_ids < 1000]
length(SS_ids)
print(SS_ids)

game_info_SS <- na.omit(game_info_SS)
game_info_SS <- game_info_SS %>% 
  filter(shortstop < 1000)

# collect unique second basemen IDS

game_info_2B <- game_info %>% 
  select(game_str, home_team, away_team, at_bat, second_base) %>% 
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

shortstop_involved_1A_83 <- game_events %>% 
  filter(Season == "Season_1883",
         HomeTeam == "Home1A",
         player_position == 6) %>% 
  collect()

shortstop_involved_1A_83$player_ID <- 0
shortstop_involved_1A_83 <- na.omit(shortstop_involved_1A_83)

# add shortstop ID to game info for 1A

for(i in 1:340){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_1A_83$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_1A_83$at_bat[i]){
        
        shortstop_involved_1A_83$player_ID[i] <- game_info_SS$shortstop[j]
        
    }
  }
}

for(i in 341:length(shortstop_involved_1A_83$game_str)){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_1A_83$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_1A_83$at_bat[i]){
      
      shortstop_involved_1A_83$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

shortstop_involved_1A_83 <- shortstop_involved_1A_83 %>% 
  filter(player_ID != 0)

# filter for all 2A plays 1883 where the shortstop is involved

shortstop_involved_2A_83 <- game_events %>% 
  filter(Season == "Season_1883",
         HomeTeam == "Home2A",
         player_position == 6) %>% 
  collect()

shortstop_involved_2A_83$player_ID <- 0
shortstop_involved_2A_83 <- na.omit(shortstop_involved_2A_83)

# add shortstop ID to game info for 2A

for(i in 1:136){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_2A_83$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_2A_83$at_bat[i]){
      
      shortstop_involved_2A_83$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 137:length(shortstop_involved_2A_83$game_str)){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_2A_83$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_2A_83$at_bat[i]){
      
      shortstop_involved_2A_83$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

shortstop_involved_2A_83 <- shortstop_involved_2A_83 %>% 
  filter(player_ID != 0)

# filter for all 3A plays 1883 where the shortstop is involved

shortstop_involved_3A_83 <- game_events %>% 
  filter(Season == "Season_1883",
         HomeTeam == "Home3A",
         player_position == 6) %>% 
  collect()

shortstop_involved_3A_83$player_ID <- 0
shortstop_involved_3A_83 <- na.omit(shortstop_involved_3A_83)

# add shortstop ID to game info for 3A

for(i in 1:217){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_3A_83$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_3A_83$at_bat[i]){
      
      shortstop_involved_3A_83$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 218:length(shortstop_involved_3A_83$game_str)){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_3A_83$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_3A_83$at_bat[i]){
      
      shortstop_involved_3A_83$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

shortstop_involved_3A_83 <- shortstop_involved_3A_83 %>% 
  filter(player_ID != 0)

# filter for all 4A plays 1883 where the shortstop is involved

shortstop_involved_4A_83 <- game_events %>% 
  filter(Season == "Season_1883",
         HomeTeam == "Home4A",
         player_position == 6) %>% 
  collect()

shortstop_involved_4A_83$player_ID <- 0
shortstop_involved_4A_83 <- na.omit(shortstop_involved_4A_83)

# add shortstop ID to game info for 4A

for(i in 1:340){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_4A_83$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_4A_83$at_bat[i]){
      
      shortstop_involved_4A_83$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 341:length(shortstop_involved_4A_83$game_str)){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_4A_83$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_4A_83$at_bat[i]){
      
      shortstop_involved_4A_83$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

shortstop_involved_4A_83 <- shortstop_involved_4A_83 %>% 
  filter(player_ID != 0)

# filter for all 1A plays 1884 where the shortstop is involved

shortstop_involved_1A_84 <- game_events %>% 
  filter(Season == "Season_1884",
         HomeTeam == "Home1A",
         player_position == 6) %>% 
  collect()

shortstop_involved_1A_84$player_ID <- 0
shortstop_involved_1A_84 <- na.omit(shortstop_involved_1A_84)

# add shortstop ID to game info for 1A

for(i in 1:358){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_1A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_1A_84$at_bat[i]){
      
      shortstop_involved_1A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 359:716){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_1A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_1A_84$at_bat[i]){
      
      shortstop_involved_1A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 717:1074){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_1A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_1A_84$at_bat[i]){
      
      shortstop_involved_1A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 1075:length(shortstop_involved_1A_84$game_str)){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_1A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_1A_84$at_bat[i]){
      
      shortstop_involved_1A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

shortstop_involved_1A_84 <- shortstop_involved_1A_84 %>% 
  filter(player_ID != 0)

# filter for all 2A plays 1884 where the shortstop is involved

shortstop_involved_2A_84 <- game_events %>% 
  filter(Season == "Season_1884",
         HomeTeam == "Home2A",
         player_position == 6) %>% 
  collect()

shortstop_involved_2A_84$player_ID <- 0
shortstop_involved_2A_84 <- na.omit(shortstop_involved_2A_84)

# add shortstop ID to game info for 2A

for(i in 1:354){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_2A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_2A_84$at_bat[i]){
      
      shortstop_involved_2A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 355:708){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_2A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_2A_84$at_bat[i]){
      
      shortstop_involved_2A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 709:1062){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_2A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_2A_84$at_bat[i]){
      
      shortstop_involved_2A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 1063:length(shortstop_involved_2A_84$game_str)){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_2A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_2A_84$at_bat[i]){
      
      shortstop_involved_2A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

shortstop_involved_2A_84 <- shortstop_involved_2A_84 %>% 
  filter(player_ID != 0)

# filter for all 3A plays 1884 where the shortstop is involved

shortstop_involved_3A_84 <- game_events %>% 
  filter(Season == "Season_1884",
         HomeTeam == "Home3A",
         player_position == 6) %>% 
  collect()

shortstop_involved_3A_84$player_ID <- 0
shortstop_involved_3A_84 <- na.omit(shortstop_involved_3A_84)

# add shortstop ID to game info for 3A

for(i in 1:324){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_3A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_3A_84$at_bat[i]){
      
      shortstop_involved_3A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 325:648){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_3A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_3A_84$at_bat[i]){
      
      shortstop_involved_3A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 649:972){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_3A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_3A_84$at_bat[i]){
      
      shortstop_involved_3A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 973:length(shortstop_involved_3A_84$game_str)){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_3A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_3A_84$at_bat[i]){
      
      shortstop_involved_3A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

shortstop_involved_3A_84 <- shortstop_involved_3A_84 %>% 
  filter(player_ID != 0)

# filter for all 4A plays 1884 where the shortstop is involved

shortstop_involved_4A_84 <- game_events %>% 
  filter(Season == "Season_1884",
         HomeTeam == "Home4A",
         player_position == 6) %>% 
  collect()

shortstop_involved_4A_84$player_ID <- 0
shortstop_involved_4A_84 <- na.omit(shortstop_involved_4A_84)

# add shortstop ID to game info for 4A

for(i in 1:366){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_4A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_4A_84$at_bat[i]){
      
      shortstop_involved_4A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 367:732){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_4A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_4A_84$at_bat[i]){
      
      shortstop_involved_4A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 733:1098){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_4A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_4A_84$at_bat[i]){
      
      shortstop_involved_4A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

for(i in 1099:length(shortstop_involved_4A_84$game_str)){
  for(j in 1:length(game_info_SS$game_str)){
    if(game_info_SS$game_str[j] == shortstop_involved_4A_84$game_str[i] & game_info_SS$shortstop[j] < 1000 & game_info_SS$at_bat[j] == shortstop_involved_4A_84$at_bat[i]){
      
      shortstop_involved_4A_84$player_ID[i] <- game_info_SS$shortstop[j]
      
    }
  }
}

shortstop_involved_4A_84 <- shortstop_involved_4A_84 %>% 
  filter(player_ID != 0)

#binding rows
shortstop_overall_involvement <- bind_rows(shortstop_involved_1A_83, shortstop_involved_2A_83,
                                           shortstop_involved_3A_83, shortstop_involved_4A_83,
                                           shortstop_involved_1A_84, shortstop_involved_2A_84,
                                           shortstop_involved_3A_84, shortstop_involved_4A_84)

#writing the overall involvement to a new csv
write.csv(shortstop_overall_involvement, "C:\\SMT\\RMU-SAC-SMT\\Gehr\\Overall_SS_Involvement.csv")

############## NEXT STEPS ##############
# 
# Find all plays where SS or 2B were involved
# Split plays into routine, intermediate, stellar
# Look at fielding percentage across levels
# Investigate improvements from entire system from year to year
# Investigate improvements for individual players from year to year

