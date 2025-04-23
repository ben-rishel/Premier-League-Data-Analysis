#### Research Question 1. Is home-field advantage real overall? For certain teams?

### Plan: Goal, Needs, Steps

## Goal: Research whether or not home-field advantage is real in world football

## Needs: Data from worldfootballR that contains home-away splits

## Steps
# Import Packages
# Load in Data
# Wrangle data to just contain home/away stats
# Visualize the difference in home/away stats for all teams
# Run a t-test to see if the difference is statistically significant
# Repeat for other leagues

### Import Packages

library(worldfootballR)
library(dplyr)
library(tidyr)

pl_team_stats_raw <- fb_season_team_stats(
  country = "ENG", 
  gender = "M", 
  season_end_year = 2025, 
  tier = "1st", 
  stat_type = "league_table_home_away"
)

#glimpse(pl_team_stats_raw)

pl_pts_per_match <- pl_team_stats_raw %>%
  select(
    Squad,     
    MP_Home,
    MP_Away,
    Pts_per_MP_Home,
    Pts_per_MP_Away,
    xGD_per_90_Home,
    xGD_per_90_Away
  ) %>%
  mutate(
    across(c(Pts_per_MP_Home, Pts_per_MP_Away, xGD_per_90_Home, xGD_per_90_Away), as.numeric)
  )

pl_more_pts_home <- pl_pts_per_match %>%
  summarize(
    Squad = Squad,
    Pts_gained_at_home_per_match = Pts_per_MP_Home - Pts_per_MP_Away,
    xPts_gained_at_home_per_match = xGD_per_90_Home - xGD_per_90_Away
  )

View(pl_more_pts_home)
