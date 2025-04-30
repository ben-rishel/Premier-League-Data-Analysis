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
library(knitr)


### Load in Data
pl_team_stats_raw <- fb_season_team_stats(
  country = "ENG", 
  gender = "M", 
  season_end_year = 2025, 
  tier = "1st", 
  stat_type = "league_table_home_away"
)

### Data Wrangling

## Selecting attributes for analysis, and making them numeric values
pl_pts_per_match <- pl_team_stats_raw %>%
  select(
    Squad,     
    Pts_per_MP_Home,
    Pts_per_MP_Away,
    xGD_per_90_Home,
    xGD_per_90_Away
  ) %>%
  mutate( #Force stats to be numeric
    across(c(Pts_per_MP_Home, Pts_per_MP_Away, xGD_per_90_Home, xGD_per_90_Away), as.numeric)
  )

## Creating Summary Table with Points Gained Metrics
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
  geom_density( #density plot
    fill = "#69b3a2", 
    color = "#e9ecef"
  ) + 
  labs(
    x = "Points Gained Per Match at Home",
    y = "Density",
    title = "Premier League Points Gained at Home"
  ) +
  scale_x_continuous(
    limits = c(-.75, 1.1) #Making sure all data fits by setting x bounds
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
  geom_label( #adds label to top chart
    aes(x = .8, y = 1, label = "Points Gained at Home Per Match")
  ) +
  # Bottom (flipped)
  geom_density(
    aes(x = xPts_gained_at_home_per_match, y = -..density..),
    fill = "#404080"
  ) +
  geom_label( #adds label to bottom chart
    aes(x = .8, y = -1, label = "xPoints Gained at Home per Match")
  ) + 
  labs(
    x = "Points/xPoints Gained Per Match at Home",
    y = "Density",
    title = "Premier League Points/Expected Points Gained at Home"
  ) +
  theme_light()


#Testing League-Wide Home Field Advantage Statistical Significance

pl_2025_match_raw <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2025, tier = "1st")

## Selecting attributes for analysis, and making them numeric values
pl_2025_match <- pl_2025_match_raw %>%
  select(
    HomeGoals,     
    AwayGoals
  ) %>%
  mutate( #Force stats to be numeric
    across(c(HomeGoals, AwayGoals), as.numeric)
  )

## Creating Summary Table with Match as Case, attributes homePts, awayPts
pl_homeAwayPts <- pl_2025_match %>%
  mutate(
    homePts = case_when( ## When home scores more, win, if same, tie
      HomeGoals  > AwayGoals ~ 3,
      HomeGoals  == AwayGoals ~ 1,
      HomeGoals  < AwayGoals ~ 0
    ),
    awayPts = case_when(
      AwayGoals  > HomeGoals ~ 3,
      AwayGoals  == HomeGoals ~ 1,
      AwayGoals < HomeGoals ~ 0
    )
  )

## Creating table to be used for t-test
pl_homePtsGained <- pl_homeAwayPts %>%
  summarize(
    homePtsGained = homePts - awayPts
  ) 

test <- t.test( 
        pl_homePtsGained$homePtsGained,
        mu = 0,          # Null Hypothesis: no home advantage
        alternative = "greater" ) #One sided t-test

## Building Dataframe with info from t-test
test_info <- data.frame(
  `T-Statistic` = unname(test$statistic),
  `P-Value`     = test$p.value,
  `CI-lower`    = test$conf.int[1],
  `CI-upper`    = test$conf.int[2],
  check.names   = FALSE  # keep our nice column names
)

## Create Nice Table with Kable - "One-Sample T-Test: Home vs. Away Point Difference"
test_info %>% kable(
      digits  = 3,
      align = c(rep("c", 4))
      ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 16
  )



