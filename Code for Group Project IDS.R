#Dividing Europe into top 4 against Bottom 6 by GDP per capita 


# YOUTH NEET over time: Top 4 most developed vs bottom 5 least developed countries in Europe

library(tidyverse)
library(ggplot2)
library(dplyr)

setwd("C:/Users/ibnea/Downloads/data sets (1)/data sets")

youth_path      <- "C:/Users/ibnea/Downloads/data sets (1)/data sets/youth-not-in-education-employment-training.csv"
gdp_path        <- "C:/Users/ibnea/Downloads/data sets (1)/data sets/gdp-per-capita-worldbank.csv"
continents_path <- "C:/Users/ibnea/Downloads/data sets (1)/data sets/continents-according-to-our-world-in-data.csv"

youth      <- read.csv(youth_path, check.names = TRUE)
gdp        <- read.csv(gdp_path, check.names = TRUE)
continents <- read.csv(continents_path, check.names = TRUE)


# 2) Code -> Continent

continent_lookup <- continents %>%
  select(Code, Continent) %>%
  distinct()


# 3) Find NEET column and rename it to 'neet_share'

neet_col <- names(youth)[grepl("not.in.education.*employment.*training", tolower(names(youth)))]
neet_col <- neet_col[1]  
if (is.na(neet_col) || length(neet_col) == 0) stop("NEET column not found. Check names(youth).")

youth_clean <- youth %>%
  rename(neet_share = all_of(neet_col))


# 4) GDP per capita column -> 'gdp_pc'
gdp_col <- names(gdp)[grepl("gdp.*per.*capita", tolower(names(gdp)))]
gdp_col <- gdp_col[1]
if (is.na(gdp_col) || length(gdp_col) == 0) stop("GDP per capita column not found. Check names(gdp).")


# 5) Only Europe in both datasets
youth_europe <- youth_clean %>%
  left_join(continent_lookup, by = "Code") %>%
  filter(Continent == "Europe")

# GDP
gdp_europe_latest <- gdp %>%
  left_join(continent_lookup, by = "Code") %>%
  filter(Continent == "Europe", Year <= 2020) %>%
  group_by(Code, Entity) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(Code, Entity, gdp_pc =.data[[gdp_col]]) %>%
  filter(!is.na(gdp_pc))


# 6) Pick Bottom 5 and Top 4 countries by GDP per capita 
bottom5 <- gdp_europe_latest %>%
  arrange(gdp_pc) %>%
  slice_head(n = 5) %>%
  mutate(Group = "Bottom 5 GDP per capita")

top4 <- gdp_europe_latest %>%
  arrange(desc(gdp_pc)) %>%
  slice_head(n = 4) %>%
  mutate(Group = "Top 4 GDP per capita")

# Combine target countries
target_countries <- bind_rows(bottom5, top4) %>%
  select(Code, Entity, Group)


# 7) Keep NEET time series only for those countries
plot_df <- youth_europe %>%
  semi_join(target_countries, by = c("Code", "Entity")) %>%
  left_join(target_countries, by = c("Code", "Entity")) %>%
  filter(!is.na(neet_share))

# 8) Plot: colors = country, linetype = group (hidden in legend to keep it simple)
ggplot(plot_df, aes(x = Year, y = neet_share, color = Entity, linetype = Group, group = Entity)) +
  geom_line(size = 1.4, alpha = 0.95) +
  labs(
    title = "NEET over time (Europe): Top 4 vs Bottom 5 by GDP per capita",
    x = "Year",
    y = "NEET (% of youth 15–24)",
    color = "Country"
  ) +
  # Hide the group linetype legend to keep the legend simple (only countries are shown)
  guides(
    linetype = "none",
    color = guide_legend(override.aes = list(linetype = "solid", linewidth = 1.6))
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 8)
  )





