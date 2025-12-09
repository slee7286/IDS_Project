# Introduction to Data Science - K12 Group Assignment
# Final Code: GDP Growth (Target 1) and Youth NEET (Target 2)

# 1a) Setup: load packages 

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)

# 1b) Setup: Load and clean core datasets 

### Set working directory to folder containing the CSV files 
setwd('C:/Users/slee7/OneDrive - Imperial College London/Introduction to Data Science/IDS Midterm/data sets')

### Read and store 3 CSV files and 1 custom CSV files, separating data by comma delimiter
# - csv1: continent classification 
# - csv2: GDP per capita (PPP 2017 USD $)
# - csv3: youth NEET share (% of youth 15-24)
csv1 <- read.csv(file = "continents-according-to-our-world-in-data.csv",
                 sep = ",")

csv2 <- read.csv(file = "gdp-per-capita-worldbank.csv",
                 sep = ",")


csv3 <- read.csv(file = "youth-not-in-education-employment-training.csv",
                 sep = ",")


### Remove Year Data and Entity Data - we drop Year and Entity because we only need each country's continent 
csv1 <- csv1 %>% mutate(Year = NULL,
                        Entity = NULL)

### Primary key is the unique country code, given by column 'Code'
### Left join attaches continent information to GDP and NEET data using the 3-letter country code as the key 
gdp <- csv2 %>% left_join(csv1, join_by(Code))
youth <- csv3 %>% left_join(csv1, join_by(Code))

### Rename long column name into shorter, clearer labels:
# - GDP Per Capita 
# - Youth NEET Share 
gdp <- gdp %>% rename("GDP Per Capita" = "GDP.per.capita..PPP..constant.2017.international...")
youth <- youth %>% rename("Youth NEET Share" = "Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.")


# 2) Data wrangling for GDP per capita and GDP growth rates 

### For each country (Entity), compute the year-on-year GDP growth rate based on GDP per capita. This approximates the SDG Target 8.1 (sustained growth)
gdp <- gdp %>%
  group_by(Entity) %>%
  mutate(`GDP Growth Rate` = ((`GDP Per Capita` - lag(`GDP Per Capita`)) / lag(`GDP Per Capita`)) * 100)

### For each country (Code), compute the mean GDP growth rate across all years into a new data frame.
### Then join continent info using left join so we can summarise and visualise by continent.
growth_rate_country <- gdp %>%
  group_by(Code) %>%
  summarize("Mean GDP Growth Rate" = mean(`GDP Growth Rate`, na.rm = TRUE)) %>%
  left_join(csv1) %>%
  na.omit() # omitting any N/A values 

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


# 5) Europe: classify countries by development level 
# - Filter European countries and restrict to 1990-2020, this period aligns with the availability of higher-quality data and the SDG focus.
europe_data <- gdp %>%
  filter(Continent == "Europe",
         Year >= 1990,
         Year <= 2020)


# For each European country, compute the average GDP per capita (1990-2020).
# - This is used as a proxy for development level. 
europe_avg_gdp <- europe_data %>%
  group_by(Entity, Code) %>%
  summarise(`Avg GDP Per Capita` = mean(`GDP Per Capita`, na.rm = TRUE),
            .groups = "drop")

# Use the median of average GDP per capita to split countries into:
# - "More developed" (above or equal to median)
# - "Less developed" (below median)
median_gdp <- median(europe_avg_gdp$`Avg GDP Per Capita`, na.rm = TRUE)

europe_classification <- europe_avg_gdp %>%
  mutate(`Development Level` = if_else(`Avg GDP Per Capita` >= median_gdp,
                                       "More Developed",
                                       "Less Developed"))

# Attach development level back to the full panel (country-year) dataset.
europe_data <- europe_data %>%
  left_join(europe_classification %>%
              select(Code, `Development Level`),
            by = "Code") %>%
  filter(!is.na(`Development Level`))



# 6) Graph: Europe GDP growth over time by development level
# - Smoothed groph paths for more vs less developed European countries.
# - The dashed red line at 7% marks the UN target for LDCs in SDG 8.1.
plot1 <- europe_data %>%
  ggplot(aes(x = Year, y = `GDP Growth Rate`, color = `Development Level`)) +
  geom_smooth(method = "loess", aes(fill = `Development Level`), alpha = 0.2, size = 1.2) +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", x = min(europe_data$Year, na.rm = TRUE) + 2, y = 7.5, label = "UN Target: 7% for LDCs", color = "red", size = 3) +
  labs(
    title = "GDP Growth Rate in Europe: More vs. Less Developed Countries",
    x = "Year",
    y = "GDP Growth Rate (%)",
    color = "Development Level",
    fill  = "Development Level"
  ) +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text    = element_text(size = 10),
    axis.title   = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot1)

# 7) Graph: Distribution of GDP growth in Europe by development level 
# - Boxplot removes extreme outliers to focus on the bulk of the distribution.
# - Compares typical growth volatility for more vs less developed countries. 

plot2 <- europe_data %>%
  filter(`GDP Growth Rate` >= -20,
         `GDP Growth Rate` <= 20) %>%  # remove extreme outliers
  ggplot(aes(x = `Development Level`,
             y = `GDP Growth Rate`,
             fill = `Development Level`)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 7,
             linetype = "dashed",
             color = "red",
             size = 0.8) +
  labs(
    title = "Distribution of GDP Growth Rates in Europe by Development Level",
    x = "Development Level",
    y = "GDP Growth Rate (%)",
    fill = "Development Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text  = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.position = "none"
  )

print(plot2)


# 8) Graphs for share of youth NEET across all the continents over time 

# - Average NEET by continent and year (unweighted across countries)
# - We filter to Year <= 2020 to match the SDG 8.6 target horizon. 
youth <- youth %>%
  group_by(Continent, Year) %>%
  filter(!is.na(Continent), Year <= 2020) %>%
  summarise(
    mean_neet = mean(`Youth NEET Share`, na.rm = TRUE),
    .groups = "drop"
  )

# Line plot: evolution of average NEET share across continents over time.
ggplot(youth, 
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
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic"),
    legend.title = element_text(face = "bold")
  )

# Summarise change up to 2020 by continent
# For each continent, we:
#  - find the earliest year with data, 
#  - find 2020 (or the latest year <= 2020 if 2020 is missing),
#  - compute absolute and percentage change 

earliest_neet <- youth %>%
  group_by(Continent) %>%
  slice_min(Year, n = 1, with_ties = FALSE) %>%
  rename(
    earliest_year = Year, 
    neet_earliest = mean_neet
  ) %>%
  ungroup()

latest_neet <- youth %>%
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
  arrange(absolute_change) %>%
  select(Continent, absolute_change, percent_change, neet_earliest)


# 9) NEET in Europe: Top 5 vs Bottom 5 by GDP per capita 

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
