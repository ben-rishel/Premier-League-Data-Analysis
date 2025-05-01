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

### Import Packages

library(worldfootballR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(scales) #for dollar formatting

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
    avg_player_value = 1.13 * mean(player_market_value_euro, na.rm = TRUE),
    player_count = n(), #^adjusting for euro -> dollar conversion
    .groups = "drop" #drop grouping for later analysis
  ) 

## Create Sorted Table

# Table of ten teams with highest average player values

top_ten <- team_player_vals %>% #selecting top 10
  slice_max(
    order_by = avg_player_value, 
    n = 10) %>%
  mutate(
    avg_player_value = dollar(avg_player_value) #for formatting in table
  )

top_ten %>% #creating table
  kable(
    booktabs = TRUE,
    align = c(rep("l", 4)),
    col.names = c("Big 5 League", "Squad Name", "Average Player Value", "# of Players")
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 16
  )


# Table of ten lowest average player values

bottom_ten <- team_player_vals %>% #selecting bottom 10
  slice_min(
    order_by = avg_player_value, 
    n = 10) %>%
  mutate(
    avg_player_value = dollar(avg_player_value) #for formatting in table
  )

bottom_ten %>% #creating table
  kable(
    booktabs = TRUE,
    align = c(rep("l", 4)),
    col.names = c("Big 5 League", "Squad Name", "Average Player Value", "# of Players")
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 16
  )

### Visualize difference in league valuations across big 5 leagues

league_team_vals <- team_player_vals  %>%
  group_by(comp_name) %>%
  summarise(
    avg_team_value = 1.13 * mean(avg_player_value, na.rm = TRUE) / 1000000,
    team_count = n()
  )

## Create Viz (Bar chart of 5 leagues average)

ggplot(
  league_team_vals, 
  aes(
    x = comp_name,
    y = avg_team_value
    )
  ) + 
  geom_bar(
    stat = "identity"
  ) +
  labs(
    x = "Big 5 League",
    y = "Average Team Valuation (Millions of Dollars)"
  ) +
  theme_bw()

