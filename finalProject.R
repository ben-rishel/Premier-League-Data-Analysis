#devtools::install_github("JaseZiv/worldfootballR")
#installs most updated version of worldfootballR package
#This package allows easy scraping of FBref and Transfermarkt
#https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html
library(worldfootballR)
library(dplyr)


#Function to extract a season of team stats from a particular league (PL here)
#stat_types are league_table, league_table_home_away, standard
#keeper, keeper_adv, shooting, passing, passing_types, goal_shot_creation
#defense, possession, playing_time, misc
pl_team_stats_raw <- fb_season_team_stats(
  country = "ENG", 
  gender = "M", 
  season_end_year = 2025, 
  tier = "1st", 
  stat_type = "standard"
  )
glimpse(pl_team_stats_raw)

#Function to extract player stats
#stat_types are standard, shooting, passing, passing_types, gca, defense
#possession, playing_time, misc, keepers, keepers_adv
#can be used for the same purpose of fb_season_team_stats also
pl_player_stats_raw <- fb_league_stats(
  country = "ENG", 
  gender = "M",
  season_end_year = 2024,
  tier = "1st",
  stat_type = "standard",
  team_or_player = "player"
  )
glimpse(pl_player_stats_raw)

#Function to extract team stats from big 5 leagues (can provide range of years)
#stat_types are standard, shooting, passing, passing_types, gca, defense
#possession, playing_time, misc, keepers, keepers_adv
#Can use Team_or_Opponent = "team" to just get team's "for" stats
big5_team_stats_raw <- fb_big5_advanced_season_stats( 
  season_end_year = 2025, 
  stat_type = "standard", 
  team_or_player = "team"
  )
glimpse(big5_team_stats_raw)

#fb_big5_advanced_season_stats can also be used for player stats
#same stat_types
big5_player_stats_raw <- fb_big5_advanced_season_stats(
  season_end_year = 2025, 
  stat_type = "standard", 
  team_or_player = "player"
  )
glimpse(big5_player_stats_raw)
