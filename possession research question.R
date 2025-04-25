# GOAL: Use scraped data and clean data to create a data visualization that 
# displays and answers our research questions, "Does having a higher average possession
# "percentage translate to wins?"

# NEEDS: Verbs --> filter, select, inner_join, mutate
# NEEDS: Nouns --> worldFootballR package, all necessary libraries

# STEPS:
# 1. Install the packages and call them
# 2. Call needed libraries to scrape data
# 3. Call all functions from scraping data
# 4. Create new R file to answer research question
# 5. Call all needed libraries
# 6. Create a function to obtain the possession stat for male first
# tier teams in England only, most recent year.
# 7. Create a function to obtain the table of the male first tier league in
# England, in the most recent year.
# 8. Clean the possession data, with necessary verbs.
# 9. Clean the league table data
# 10. Join the two cleans data and put into one
# 11. Create visualization for the conjoined data.


# Calls all needed libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Focuses on possession for all the teams in English Mens' First Tier
pl_possession <- fb_season_team_stats(
  country = "ENG", 
  gender = "M", 
  season_end_year = 2025, 
  tier = "1st", 
  stat_type = "possession"
)

# Focuses on the teams on the table in most recent year
pl_table <- fb_season_team_stats(
  country = "ENG", 
  gender = "M", 
  season_end_year = 2025, 
  tier = "1st", 
  stat_type = "league_table"
)

# Cleans the possession data
pl_possession_clean <- pl_possession %>%
  filter(!str_starts(Squad, "vs ")) %>% # Gets rid of average opposing possession
  select(Squad, Poss) %>% # Focuses on team and the possession percentage
  mutate(Poss = as.numeric(str_remove(Poss, "%"))) 


# Cleans the league table data
pl_table_clean <- pl_table %>%
  filter(!str_starts(Squad, "vs ")) %>% # Filters out average opposing once again
  select(Squad, W) # Focuses on the team and the wins


# Combines the two clean datas
pl_poss_wins <- pl_possession_clean %>%
  inner_join(pl_table_clean, by = "Squad")

View(pl_poss_wins)

# Creates scatter plot for the combined data
ggplot(
  data = pl_poss_wins,
  mapping = aes(
    x = Poss,
    y = W,
    color = Squad
  )
) +
  geom_point(size = 3) +
  labs(
    x = "Possession Percentage",
    y = "Wins",
    color = "Squad",
    title = "Wins by Possession Percentage"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )
  
  

