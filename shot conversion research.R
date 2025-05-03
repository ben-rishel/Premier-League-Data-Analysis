# GOAL: Use scraped and clean data to find each Premier League teams'
# shot conversion rate in the 2024-25 season. This will help answer our 
# question of which team has the best shot conversion rate and which has the worst.

# NEEDS: Verbs --> filter, select, mutate, arrange
# NEEDS: Nouns --> worldFootballR package, all necessary libraries, team stats

# STEPS:
# 1. Install the packages and call them
# 2. Call needed libraries to scrape data
# 3. Call all functions from scraping data
# 4. Create new R file to answer research question
# 5. Call all needed libraries
# 6. Create a function to obtain the shooting stats for each premier league team
# 7. Create a function that cleans the shooting stats and uses the shot conversion rate
# 8. Clean the possession data, with necessary verbs.
# 9. Create visualization for the conjoined data.


# Calls all needed libraries
library(worldfootballR)
library(dplyr)
library(ggplot2)


# Focuses on shooting for all the teams in English Mens' First Tier
pl_shooting <- fb_season_team_stats(
  country = "ENG", 
  gender = "M", 
  season_end_year = 2025, 
  tier = "1st", 
  stat_type = "shooting"
)


# Cleans shooting data and uses the shot conversion formula
pl_conversion <- pl_shooting %>%
  filter(!str_starts(Squad, "vs ")) %>%
  select(Squad, Goals = Gls_Standard, Shots = Sh_Standard) %>%
  mutate(
    ConversionRate = (Goals / Shots) * 100
  ) %>%
  arrange(desc(ConversionRate))

# Creates a horizontal bar chart to visualize the shot conversion rate for each team
ggplot(
  data = pl_conversion,
  mapping = aes(
    x = reorder(Squad, ConversionRate),
    y = ConversionRate
  )
) +
  geom_col(fill = "#18a7db") +
  coord_flip() +
  labs(
    title = "Premier League Teams Shot Conversion Rate (2024-25)",
    x = "Team",
    y = "Shot Conversion Rate (%)"
  ) +
  theme_minimal()

