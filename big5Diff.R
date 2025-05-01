#### Research Question 2. 2. What are the differences across the big 5 leagues?

### Plan: Goal, Needs, Steps

## Goal: Use Transfermarket data to find differences of big 5 leagues

## Needs: 
# Nouns: Data, worldfootballR package, dplyr, tidyr, ggplot2, knitr
# Verbs: worldfootballR functions, data wrangling verbs, ggplot functions

## Steps
# Import Packages
# Load in Data
# Wrangle data
# Visualize difference in team/player valuations across leagues
# Run a t-test to see if the difference is statistically significant

### Import Packages

library(worldfootballR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)

### Load in Data


big_5_valuations <- tm_player_market_values(
  country_name = c("England", "Spain", "Italy", "Germany", "France"),
  start_year = 2024
  )

### Wrangle Data

big_5_valuations_wrangled <- big_5_valuations %>%
  select(
    comp_name,
    squad,
    player_market_value_euro
  )

### Visualize difference in team valuations across big 5 leagues

team_player_vals <- big_5_valuations_wrangled  %>%
  group_by(comp_name, squad) %>%
  summarise(
    avg_player_value = mean(player_market_value_euro, na.rm = TRUE),
    player_count = n(),
    .groups = "drop"
  )

## Create Sorted Table + Viz

### Visualize difference in league valuations across big 5 leagues

league_team_vals <- team_player_vals  %>%
  group_by(comp_name) %>%
  summarise(
    avg_team_value = mean(avg_player_value, na.rm = TRUE),
    team_count = n(),
    .groups = "drop"
  )

## Create Sorted Table + Viz

### Run a t-test to see if the difference is statistically significant