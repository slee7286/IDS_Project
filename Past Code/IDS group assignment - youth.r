#1. Setup - load packages and the datasets 

library(tidyverse)
library(ggplot2)
library(dplyr)

setwd("/Users/chiakalaemezie/Downloads/data sets")


GDP_per_capita <- read.csv("gdp-per-capita-worldbank.csv")
youth <- read.csv("youth-not-in-education-employment-training.csv")
continents <- read.csv("continents-according-to-our-world-in-data.csv")

# Check column names 
view(GDP_per_capita)
view(youth)
view(continents)


#2. Clean and prepare continent lookup - we just need a unique mapping from Code to Continent
continent_lookup <- continents %>%
  select(Code, Continent) %>%
  distinct()

view(continent_lookup)

#3. Clean youth NEET data and merge with continents - rename it to 'neet_share' for convenience
youth_clean <- youth %>%
  rename(
    neet_share = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.
  )

view(youth_clean)

# Merge continent info onto youth data using the 3-letter country code 
youth_continent <- youth_clean %>%
  left_join(continent_lookup, by = "Code") %>%
  # Drop Antartica and observations without a continent
  filter(!is.na(Continent), Continent != "Antartica")

view(youth_continent)

#4. Aggregate: average NEET by continent and year - an unweighted mean across countries in a continent 
continent_year_neet <- youth_continent %>%
  group_by(Continent, Year) %>%
  summarise(
    mean_neet = mean(neet_share, na.rm = TRUE),
    n_countries = sum(!is.na(neet_share)),
    .groups = "drop"
  )

#5. Visualise trends over time by continent 
ggplot(continent_year_neet, 
       aes(x = Year, y = mean_neet, colour = Continent)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Share of Youth Not in Employment, Education or Training (NEET)",
    subtitle = "Average across countries within each continent",
    x = "Year",
    y = "Mean NEET (% of youth population)",
    colour = "Continent"
  ) +
  theme_minimal()
  

#6. Summarise change up to 2020 by continent
# For each continent, we:
#  - find the earliest year with data, 
#  - find 2020 (or the latest year <= 2020 if 2020 is missing),
#  - compute absolute and percentage change 

earliest_neet <- continent_year_neet %>%
  group_by(Continent) %>%
  slice_min(Year, n = 1, with_ties = FALSE) %>%
  rename(
    earliest_year = Year, 
    neet_earliest = mean_neet
  ) %>%
  ungroup()

latest_neet <- continent_year_neet %>%
  filter(Year <= 2020) %>%
  group_by(Continent) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  rename(
    latest_year = Year,
    neet_latest = mean_neet
  ) %>%
  ungroup()

neet_change_summary <- earliest_neet %>%
  inner_join(latest_neet, by = "Continent") %>%
  mutate(
    absolute_change = neet_latest - neet_earliest,
    percent_change = 100 * (neet_latest - neet_earliest) / neet_earliest
  ) %>%
  arrange(absolute_change)

view(neet_change_summary)

  
