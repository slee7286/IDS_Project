library(tidyverse)
library(ggplot2)
library(ggthemes)

### Set working directory to CSV file location
setwd('C:/Users/slee7/OneDrive - Imperial College London/Introduction to Data Science/IDS Midterm/data sets')

### Read and store 3 CSV files and 1 custom CSV file in four variables, separating data by comma delimiter
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
### Left join adds the column 'Continent' to csv2 and csv3, which we store in another variable
combined_gdp <- csv2 %>% left_join(csv1, join_by(Code))
combined_youth <- csv3 %>% left_join(csv1, join_by(Code))

### Rename long column names to be more simple
combined_gdp <- combined_gdp %>% rename("GDP per capita" = "GDP.per.capita..PPP..constant.2017.international...")
combined_youth <- combined_youth %>% rename("Youth NEET" = "Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.")

### Calculate GDP growth rate based on GDP per capita data
combined_gdp <- combined_gdp %>%
  mutate(`GDP Growth Rate` = ((`GDP per capita` - lag(`GDP per capita`)) / lag(`GDP per capita`)) * 100)

### Create new data frame containing mean GDP growth rate for every continent
growth_rate <- combined %>%
  group_by(Code) %>%
  summarize("Mean GDP Growth Rate" = mean(`GDP Growth Rate`, na.rm = TRUE)) %>%
  right_join(csv1) %>%
  na.omit()
