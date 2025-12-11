library(tidyverse)
library(ggplot2)
library(ggthemes)

setwd('C:/Users/slee7/OneDrive - Imperial College London/Introduction to Data Science/IDS Midterm/data sets')

csv1 <- read.csv(file = "continents-according-to-our-world-in-data.csv",
                 sep = ",")


csv2 <- read.csv(file = "gdp-per-capita-worldbank.csv",
                 sep = ",")

csv1 <- csv1 %>% mutate(Year = NULL,
                        Entity = NULL)

combined <- csv2 %>% left_join(csv1, join_by(Code))

combined <- combined %>% rename("GDP per capita" = "GDP.per.capita..PPP..constant.2017.international...")

combined <- combined %>%
  group_by(Code) %>%
  mutate(`GDP Growth Rate` = ((`GDP per capita` - lag(`GDP per capita`)) / lag(`GDP per capita`)) * 100) %>%
  ungroup()

growth_rate <- combined %>%
  group_by(Code) %>%
  summarize("Mean GDP Growth Rate" = mean(`GDP Growth Rate`, na.rm = TRUE)) %>%
  right_join(csv1) %>%
  na.omit()

growth_rate %>%
  ggplot(aes(
    x = Continent,
    y = `Mean GDP Growth Rate`)) +
  geom_boxplot() +
  labs(
    x = "Continents",
    y = "Mean GDP Growth Rate (%)",
    title = "Mean GDP Growth Rate by Continent"
    ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 8),
    axis.line = element_line(colour = "darkblue", size = 1),
    legend.title = element_text(face = "bold")
  )

combined_1 <- combined %>%
  filter(Continent == "Asia" | Continent == "Oceania" | Continent == "Africa")

combined_2 <- combined %>%
  filter(Continent == "Europe" | Continent == "North America" | Continent == "South America")


combined_1 %>%
  ggplot(aes(
    x = Year,
    y = `GDP Growth Rate`,
    color = Continent)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), aes(fill = Continent), alpha = 0.15) +
  labs(
    x = "Year",
    y = "GDP Growth Rate (%)",
    title = "GDP Growth Rate by Year (Part 1)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 8),
    axis.line = element_line(colour = "darkblue", size = 1),
    legend.title = element_text(face = "bold")
  )

combined_2 %>%
  ggplot(aes(
    x = Year,
    y = `GDP Growth Rate`,
    color = Continent)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), aes(fill = Continent), alpha = 0.15) +
  labs(
    x = "Year",
    y = "GDP Growth Rate (%)",
    title = "GDP Growth Rate by Year (Part 2)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 8),
    axis.line = element_line(colour = "darkblue", size = 1),
    legend.title = element_text(face = "bold")
  )
