# 1 Load packages
library(tidyverse)

# 2 Set working directory  <-- change this path to your folder
setwd("/Users/rongxuanmah/Downloads/data sets")

# 3 Read the three datasets
continents <- read_csv("continents-according-to-our-world-in-data.csv")
gdp       <- read_csv("gdp-per-capita-worldbank.csv")
youth     <- read_csv("youth-not-in-education-employment-training.csv")
# youth is loaded now but not yet used for target 1

# 4 Clean and keep only needed columns

# Continents file
# Typical OWID columns are Entity, Code, Year, Continent
continents_clean <- continents |>
  rename(
    country   = Entity,
    iso_code  = Code,
    year      = Year,
    continent = Continent
  ) |>
  select(iso_code, continent) |>
  distinct()

# GDP per capita file
# Typical OWID columns are Entity, Code, Year,
# and a long GDP per capita column name
gdp_clean <- gdp |>
  rename(
    country  = Entity,
    iso_code = Code,
    year     = Year,
    gdp_pc   = `GDP per capita, PPP (constant 2017 international $)`
  ) |>
  select(country, iso_code, year, gdp_pc)

# 5 Merge GDP with continent info and drop Antarctica

gdp_continent <- gdp_clean |>
  left_join(continents_clean, by = "iso_code") |>
  filter(!is.na(continent),
         continent != "Antarctica")

# Optional filter for years from 1990 onwards only
gdp_continent <- gdp_continent |>
  filter(year >= 1990)

# 6 Compute annual GDP per capita growth for each country
# growth in percent compared with previous year

gdp_growth <- gdp_continent |>
  group_by(continent, country) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    gdp_growth_pct = (gdp_pc / lag(gdp_pc) - 1) * 100
  ) |>
  ungroup()

# 7 Summarise progress at continent level over time
# mean and median growth, and share of countries with growth >= 7 percent

continent_progress <- gdp_growth |>
  group_by(continent, year) |>
  summarise(
    mean_growth_pct   = mean(gdp_growth_pct, na.rm = TRUE),
    median_growth_pct = median(gdp_growth_pct, na.rm = TRUE),
    share_ge_7        = mean(gdp_growth_pct >= 7, na.rm = TRUE),
    .groups = "drop"
  )

# 8 Look at the summary table
print(continent_progress)

# 9 Plot average GDP per capita growth for each continent over time
# with a dashed horizontal line at 7 percent

ggplot(continent_progress,
       aes(x = year, y = mean_growth_pct, colour = continent)) +
  geom_line()+
  geom_hline(yintercept = 7, linetype = "dashed") +
  labs(
    title = "Average GDP per capita growth by continent",
    subtitle = "PPP, constant 2017 international dollars",
    x = "Year",
    y = "Mean GDP per capita growth (percent)",
    colour = "Continent"
  ) +
  theme_minimal()

# 10 If you want one panel per continent

ggplot(continent_progress,
       aes(x = year, y = mean_growth_pct)) +
  geom_hline(yintercept = 7, linetype = "dashed") +
  geom_line() +
  facet_wrap(~ continent, scales = "free_y") +
  labs(
    title = "Average GDP per capita growth by continent",
    subtitle = "Dashed line shows 7 percent SDG target",
    x = "Year",
    y = "Mean GDP per capita growth (percent)"
  ) +
  theme_minimal()
