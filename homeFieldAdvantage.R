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
library(ggplot2)

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

#Creating Density Chart to Analyze How Much Home Field Advantage Matters
ggplot( 
  data = pl_more_pts_home,
  aes(
    x = Pts_gained_at_home_per_match
    )
  ) +
  geom_density(
    fill = "#69b3a2", 
    color = "#e9ecef"
  ) + 
  labs(
    x = "Points Gained Per Match at Home",
    y = "Density",
    title = "Premier League Points Gained at Home"
  ) +
  scale_x_continuous(
    limits = c(-.75, 1.1)
    ) + 
  theme_light()



#Top/Bottom Density Plot that Compares Points vs. Expected Points
ggplot(
  data = pl_more_pts_home,
  aes(x = Pts_gained_at_home_per_match)
) +
  # Top
  geom_density(
    aes(y = ..density..),
    fill = "#69b3a2"
  ) +
  geom_label(
    aes(x = .8, y = 1, label = "Points Gained at Home Per Match")
  ) +
  # Bottom (flipped)
  geom_density(
    aes(x = xPts_gained_at_home_per_match, y = -..density..),
    fill = "#404080"
  ) +
  geom_label(
    aes(x = .8, y = -1, label = "xPoints Gained at Home per Match")
  ) + 
  labs(
    x = "Points/xPoints Gained Per Match at Home",
    y = "Density",
    title = "Premier League Points/Expected Points Gained at Home"
  ) +
  theme_light()
