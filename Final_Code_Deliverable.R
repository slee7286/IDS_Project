library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)

# 1) Data cleaning

### Set working directory to CSV file location
setwd('C:/Users/slee7/OneDrive - Imperial College London/Introduction to Data Science/IDS Midterm/data sets')

### Read and store 3 CSV files and 1 custom CSV files, separating data by comma delimiter
csv1 <- read.csv(file = "continents-according-to-our-world-in-data.csv",
                 sep = ",")

csv2 <- read.csv(file = "gdp-per-capita-worldbank.csv",
                 sep = ",")


csv3 <- read.csv(file = "youth-not-in-education-employment-training.csv",
                 sep = ",")

csv4 <- read.csv(file = "Group K12 Custom Data.csv",
                 sep = ",")

### Remove Year Data and Entity Data
csv1 <- csv1 %>% mutate(Year = NULL,
                        Entity = NULL)

### Primary key is the unique country code, given by column 'Code'
### Left join adds the column 'Continent' to csv2 and csv3
gdp <- csv2 %>% left_join(csv1, join_by(Code))
youth <- csv3 %>% left_join(csv1, join_by(Code))

### Rename long column names to be more simple
gdp <- gdp %>% rename("GDP Per Capita" = "GDP.per.capita..PPP..constant.2017.international...")
youth <- youth %>% rename("Youth NEET Share" = "Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.")

# 2) Data wrangling for GDP per capita/GDP growth rate

### Calculate GDP growth rate based on GDP per capita data
gdp <- gdp %>%
  mutate(`GDP Growth Rate` = ((`GDP Per Capita` - lag(`GDP Per Capita`)) / lag(`GDP Per Capita`)) * 100)

### Create new data frame containing mean GDP growth rate for every continent
### Left join join adds the column 'Continent' and we omit NA values
growth_rate_country <- gdp %>%
  group_by(Code) %>%
  summarize("Mean GDP Growth Rate" = mean(`GDP Growth Rate`, na.rm = TRUE)) %>%
  left_join(csv1) %>%
  na.omit()

# 3) Data wrangling for share of youth NEET

### Exclude San Marino due to lack of GDP data in 2021
### Exclude Ukraine due to lack of Youth NEET data
bot5top5_recent <- gdp %>%
  filter(Continent == "Europe") %>%
  filter(Code != "UKR") %>%
  filter(Year == 2021) %>%
  arrange(`GDP Per Capita`)

### Filter top 5 and bottom 5 European countries by GDP Per Capita
bot5top5 <- bind_rows(head(bot5top5_recent, 5) %>%
              mutate(Group = "Bottom 5"),
            tail(bot5top5_recent, 5) %>%
              mutate(Group = "Top 5"))

### Left join to include Youth NEET Share
bot5top5 <- youth %>%
  left_join(bot5top5 %>%
              mutate(Entity = NULL,
                     Year = NULL,
                     `GDP Per Capita` = NULL,
                     Continent= NULL,
                     `GDP Growth Rate` = NULL), join_by(Code)) %>%
  na.omit()

# 4) Graphs for GDP Per Capita across individual continents

### Exclude outliers from data frame
growth_rate_country_nout <- growth_rate_country %>%
  group_by(Continent) %>%
  mutate(a = case_when(
    `Mean GDP Growth Rate` %in% boxplot.stats(`Mean GDP Growth Rate`)$out == FALSE ~ FALSE
  )) %>%
  na.omit()

quartiles <- growth_rate_country %>%
  group_by(Continent) %>%
  reframe("Quartiles" = quantile(`Mean GDP Growth Rate`, probs = seq(0,1,0.25), type = 7, na.rm = T))


quantile(growth_rate_country$`Mean GDP Growth Rate`, probs = seq(0,1,0.25), type = 7, na.rm = T)

### Create box plot of mean GDP growth rate per continent
### Labels for 
growth_rate_country %>%
  ggplot(aes(
    x = Continent,
    y = `Mean GDP Growth Rate`)) +
  geom_boxplot(outliers = FALSE) +
  labs(
    x = "Continents",
    y = "Mean GDP Growth Rate (%)",
    title = "Mean GDP Growth Rate by Continent"
    ) +
  stat_summary(fun.data = function(x) data.frame(y=median(x), label=paste("Median:",round(median(x),1))),
               geom = "text", vjust = -0.5, size = 2.5) +
  stat_summary(fun.data = ~ data.frame(quarts = quantile(.x, probs = .25)),
                                       aes(y = stage(`Mean GDP Growth Rate`, after_stat = quarts),
                   label = paste("Q1:",round(after_stat(quarts), digits = 3))),
               geom = "label", vjust = 1.2, size = 2.5) +
  stat_summary(fun.data = ~ data.frame(quarts = quantile(.x, probs = .75)),
               aes(y = stage(`Mean GDP Growth Rate`, after_stat = quarts),
                   label = paste("Q3:",round(after_stat(quarts), digits = 3))),
               geom = "label", vjust = -0.2, size = 2.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 8),
    axis.line = element_line(colour = "darkblue", size = 1),
    legend.title = element_text(face = "bold")
  ) 

# 5) Graphs for share of youth NEET comparison within Europe

bot5top5 %>% ggplot(aes(x = Year, y = `Youth NEET Share`, color = Entity, linetype = Group, group = Entity)) +
  geom_line(size = 1.4, alpha = 0.95) +
  labs(
    title = "NEET over time (Europe): Top 5 vs Bottom 5 by GDP per capita",
    x = "Year",
    y = "NEET (% of youth 15–24)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 8)
  )
