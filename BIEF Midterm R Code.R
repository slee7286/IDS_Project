library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)

setwd('C:/Users/slee7/OneDrive - Imperial College London/Big Issues in Economics and Finance/BIEF Midterm Work/WID_Data_Metadata')

# Read files
csv1 <- read.csv(file = "Income Inequality Data.csv",
                              sep = ",",
                              header=FALSE)
csv2 <- read.csv(file = "International Trade Data 1.csv",
                 sep = ",",
                 header=FALSE)
csv3 <- read.csv(file = "Income Growth & International Trade Data.csv",
                 sep = ",",
                 header=FALSE)
csv4 <- read.csv(file = "Wealth Inequality Data.csv",
                 sep = ",",
                 header=FALSE)

# Data Wrangling
csv1 <- csv1 %>% mutate(V2 = NULL)
csv4 <- csv4 %>% mutate(V2 = NULL)
csv2 <- csv2 %>% mutate(V3 = NULL)
csv3 <- csv3 %>% mutate(V3 = NULL)


colnames(csv1) <- c("Country","Indicator","Year","Value")
colnames(csv2) <- c("Country","Indicator","Year","Value")
colnames(csv3) <- c("Country","Indicator","Year","Value")
colnames(csv4) <- c("Country","Indicator","Year","Value")


income_inequality <- csv1 %>% mutate(Indicator = case_when(
  Indicator == 'p99p100' ~ 'Top1_I',
  Indicator == 'p90p100' ~ 'Top10_I',
  Indicator == 'p50p90' ~ 'Mid40_I',
  Indicator == 'p0p50' ~ 'Bot50_I',
  Indicator == 'pall' ~ 'Gini_I',),
  Country = case_when(
    Country == 'Brazil ' ~ 'Brazil',
    Country == 'Spain ' ~ 'Spain'
  ))

wealth_inequality <- csv4 %>% mutate(Indicator = case_when(
  Indicator == 'p99p100' ~ 'Top1_W',
  Indicator == 'p90p100' ~ 'Top10_W',
  Indicator == 'p50p90' ~ 'Mid40_W',
  Indicator == 'p0p50' ~ 'Bot50_W',
  Indicator == 'pall' ~ 'Gini_W',),
  Country = case_when(
    Country == 'Brazil ' ~ 'Brazil',
    Country == 'Spain ' ~ 'Spain'
  ))

international_trade <- csv2 %>% mutate(Indicator = case_when(
  Indicator == 'mtbxrx_pall_999_i_BR\nExports of goods and services\nTotal population |  | ppp | constant (2024)\n' ~ 'Exports of Goods and Services',
  Indicator == 'mtbnnx_pall_999_i_BR\nTrade balance (exports-imports)\nTotal population |  | ppp | constant (2024)\n' ~ 'Trade Balance',
  Indicator == 'mtsnnx_pall_999_i_BR\nTrade balance services (exports-imports)\nTotal population |  | ppp | constant (2024)\n' ~ 'Trade Balance (Services)',
  Indicator == 'mtsmpx_pall_999_i_BR\nImports of services\nTotal population |  | ppp | constant (2024)\n' ~ 'Import (Services)',
  Indicator == 'mtbxrx_pall_999_i_ES\nExports of goods and services\nTotal population |  | ppp | constant (2024)\n' ~ 'Exports of Goods and Services',
  Indicator == 'mtbnnx_pall_999_i_ES\nTrade balance (exports-imports)\nTotal population |  | ppp | constant (2024)\n' ~ 'Trade Balance',
  Indicator == 'mtsnnx_pall_999_i_ES\nTrade balance services (exports-imports)\nTotal population |  | ppp | constant (2024)\n' ~ 'Trade Balance (Services)',
  Indicator == 'mtsmpx_pall_999_i_ES\nImports of services\nTotal population |  | ppp | constant (2024)\n' ~ 'Import (Services)',),
  Country = case_when(
    Country == 'Brazil ' ~ 'Brazil',
    Country == 'Spain ' ~ 'Spain'
  ))

international_trade2 <- csv3 %>% mutate(Indicator = case_when(
  Indicator == 'mnninc_pall_999_i_BR\nNational income\nTotal population |  | ppp | constant (2024)\n' ~ 'National Income',
  Indicator == 'mncanx_pall_999_i_BR\nCurrent Account\nTotal population |  | ppp | constant (2024)\n' ~ 'Current Account',
  Indicator == 'mtgnnx_pall_999_i_BR\nTrade balance goods (exports-imports)\nTotal population |  | ppp | constant (2024)\n' ~ 'Trade Balance (Goods)',
  Indicator == 'mtgmpx_pall_999_i_BR\nImports of goods\nTotal population |  | ppp | constant (2024)\n' ~ 'Import (Goods)',
  Indicator == 'mnninc_pall_999_i_ES\nNational income\nTotal population |  | ppp | constant (2024)\n' ~ 'National Income',
  Indicator == 'mncanx_pall_999_i_ES\nCurrent Account\nTotal population |  | ppp | constant (2024)\n' ~ 'Current Account',
  Indicator == 'mtgnnx_pall_999_i_ES\nTrade balance goods (exports-imports)\nTotal population |  | ppp | constant (2024)\n' ~ 'Trade Balance (Goods)',
  Indicator == 'mtgmpx_pall_999_i_ES\nImports of goods\nTotal population |  | ppp | constant (2024)\n' ~ 'Import (Goods)',),
  Country = case_when(
    Country == 'Brazil ' ~ 'Brazil',
    Country == 'Spain ' ~ 'Spain'
  ))

# Pivot into Tidy Format
income_inequality <- income_inequality %>%
  select(Country, Year, Indicator, Value) %>%
  pivot_wider(
    names_from = Indicator,
    values_from = Value
  )

international_trade <- international_trade %>%
  select(Country, Year, Indicator, Value) %>%
  pivot_wider(
    names_from = Indicator,
    values_from = Value
  )

international_trade2 <- international_trade2 %>%
  select(Country, Year, Indicator, Value) %>%
  pivot_wider(
    names_from = Indicator,
    values_from = Value
  )

wealth_inequality <- wealth_inequality %>%
  select(Country, Year, Indicator, Value) %>%
  pivot_wider(
    names_from = Indicator,
    values_from = Value
  )

# Create New Variables

# Imports of Goods and Services & Export (Services)
international_trade <- international_trade %>%
  group_by(Country) %>%
  mutate(`Imports of Goods and Services` = -(`Trade Balance` - `Exports of Goods and Services`),
         `Export (Services)` = `Trade Balance (Services)` + `Import (Services)`)

# Income Growth & Export (Goods)
international_trade2 <- international_trade2 %>%
  group_by(Country) %>%
  mutate(`Income Growth` = ((`National Income` - lag(`National Income`)) / lag(`National Income`)) * 100,
         `Export (Goods)` = `Trade Balance (Goods)` + `Import (Goods)`)

# Combine all datasets
brazil_combined <- full_join(international_trade[1:225,], international_trade2[1:225,], by = "Year") %>% full_join(income_inequality[1:205,], by = "Year") %>% full_join(wealth_inequality[1:204,], by = "Year")
brazil_combined <- brazil_combined[, -which(sapply(brazil_combined, function(col) any(col == "Brazil")))[-1]]

spain_combined <- full_join(international_trade[226:450,], international_trade2[226:450,], by = "Year") %>% full_join(income_inequality[206:410,], by = "Year") %>% full_join(wealth_inequality[205:408,], by = "Year")
spain_combined <- spain_combined[, -which(sapply(spain_combined, function(col) any(col == "Spain")))[-1]]

# Remove Unneeded Objects
rm(csv1,csv2,csv3,csv4,income_inequality,international_trade,international_trade2,wealth_inequality)
  
# List of Variables:
#   
# -Income Inequality
# Top 1% Income Share
# Top 10% Income Share
# Middle 40% Income Share
# Bottom 50% Income Share
# Gini Index (Income)
# 
# -International Trade
# Trade Balance
# Exports of Goods and Services
# Imports of Goods and Services
# Trade Balance (Services)
# Import (Services)
# Export (Services)
# Current Account
# Trade Balance(Goods)
# Import (Goods)
# Export (Goods)
# 
# -Income Growth
# National Income
# Income Growth
# 
# -Wealth Inequality
# Top 1% Wealth Share
# Top 10% Wealth Share
# Middle 40% Wealth Share
# Bottom 50% Wealth Share
# Gini Index (Wealth)

# Brazil: Exports of Goods and Services vs Top 1% Income Share

brazil_combined_income <- brazil_combined[203:225,]

scale_factor_1 <- (max(brazil_combined_income$Top1_I, na.rm=TRUE) - min(brazil_combined_income$Top1_I, na.rm=TRUE)) / (max(brazil_combined_income$'Exports of Goods and Services', na.rm=TRUE) - min(brazil_combined_income$'Exports of Goods and Services', na.rm=TRUE))

brazil_combined_income %>% ggplot(aes(x = Top1_I, y = `Exports of Goods and Services` * scale_factor_1, label=Year)) +
  geom_point(color = "blue", size = 3) +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Top 1% Income Share",
    y = "Exports of Goods and Services (Linearly Scaled)",
    title = "Brazil: Exports of Goods and Services vs Top 1% Income Share"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) + 
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) + 
  theme_minimal()

correlation_1 <- cor(brazil_combined_income$Top1_I, brazil_combined_income$`Exports of Goods and Services`, use = "complete.obs")
print(paste("Correlation coefficient:", correlation_1))

# Brazil: Exports of Goods and Services vs Top 1% Income Share

# Brazil: Gini Coefficient vs Exports of Goods and Services

scale_factor_2 <- (max(brazil_combined_income$Gini_I, na.rm=TRUE) - min(brazil_combined_income$Gini_I, na.rm=TRUE)) / (max(brazil_combined_income$'Exports of Goods and Services', na.rm=TRUE) - min(brazil_combined_income$'Exports of Goods and Services', na.rm=TRUE))

brazil_combined_income %>% ggplot(aes(x = Gini_I, y = `Exports of Goods and Services` * scale_factor_2, label=Year)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(color = "blue", size = 3) +
  geom_text_repel() +
  labs(
    x = "Income Gini Coefficient",
    y = "Exports of Goods and Services (Linearly Scaled)",
    title = "Brazil: Gini Coefficient vs Exports of Goods and Services"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) + 
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) + 
  theme_minimal()

correlation_1 <- cor(brazil_combined_income$Top1_I, brazil_combined_income$`Exports of Goods and Services`, use = "complete.obs")
print(paste("Correlation coefficient:", correlation_1))
