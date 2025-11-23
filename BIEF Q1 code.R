library(dplyr)
library(ggplot2)
library(tidyr)

#Load WID data files
spain_income_raw <- readLines("WID_Data_Spain_Income.csv")
spain_wealth_raw <- readLines("WID_Data_Spain_Wealth.csv")
brazil_income_raw <- readLines("WID_Data_Brazil_Income.csv")
brazil_wealth_raw <- readLines("WID_Data_Brazil_Wealth.csv")

extract_inequality <- function(raw_lines, country_label) {
  data_lines <- raw_lines[grepl("^\";", raw_lines)]
  data_lines <- gsub("^\"", "", data_lines)
  
  df <- read.csv(
    text = data_lines,
    sep = ";",
    header = FALSE,
    col.names = c("empty", "percentile", "year", "value"),
    strip.white = TRUE
  )
  
  df_clean <- df %>%
    select(percentile, year, value) %>%
    mutate(
      group = case_when(
        percentile == "p90p100" ~ "Top 10%",
        percentile == "p0p50"   ~ "Bottom 50%",
        TRUE ~ NA_character_
      ),
      year = as.numeric(year),
      value = as.numeric(value),
      country = country_label
    ) %>%
    filter(!is.na(group), !is.na(year), !is.na(value), year >= 1900) %>%
    select(country, group, year, value)
  
  return(df_clean)
}

spain_income <- extract_inequality(spain_income_raw, "Spain - Income")
spain_wealth <- extract_inequality(spain_wealth_raw, "Spain - Wealth")
brazil_income <- extract_inequality(brazil_income_raw, "Brazil - Income")
brazil_wealth <- extract_inequality(brazil_wealth_raw, "Brazil - Wealth")

all_data <- bind_rows(
  spain_income,
  spain_wealth,
  brazil_income,
  brazil_wealth
)


my_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey93", linewidth = 0.2),
    legend.position = "right",
    legend.key.width = unit(3, "cm")
  )

my_guides <- guides(
  linetype = guide_legend(
    override.aes = list(color = "black", linewidth = 0.8),
    keywidth = unit(3, "cm")
  )
)


#Figure 1 - Wealth inequality 
wealth_compare <- all_data %>%
  filter(grepl("Wealth", country)) %>%
  separate(country, into = c("country_name", "measure"), sep = " - ")

ggplot(wealth_compare, aes(x = year, y = value, color = country_name, linetype = group)) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  scale_color_manual(
    values = c("Brazil" = "#D32F2F", "Spain" = "#0F4C81"),
    name = "Country"
  ) +
  scale_linetype_manual(
    values = c("Bottom 50%" = "4242", "Top 10%" = "solid"),
    name = "Percentile Group"
  ) +
  my_guides +
  my_theme +
  labs(
    title = "Wealth Inequality: Brazil vs Spain",
    x = "Year",
    y = "Share of National Wealth"
  )

#Figure 2 - Income inequality
income_compare <- all_data %>%
  filter(grepl("Income", country)) %>%
  separate(country, into = c("country_name", "measure"), sep = " - ")

ggplot(income_compare, aes(x = year, y = value, color = country_name, linetype = group)) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  scale_color_manual(
    values = c("Brazil" = "#D32F2F", "Spain" = "#0F4C81"),
    name = "Country"
  ) +
  scale_linetype_manual(
    values = c("Bottom 50%" = "4242", "Top 10%" = "solid"),
    name = "Percentile Group"
  ) +
  my_guides +
  my_theme +
  labs(
    title = "Income Inequality: Brazil vs Spain",
    x = "Year",
    y = "Share of National Income"
  )

