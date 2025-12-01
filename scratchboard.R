#Tutorial 1 (9/10/2025)

install.packages("tidyverse")
install.packages("palmerpenguins")
install.packages("plotrix")
install.packages("nycflights13")
install.packages("car")
install.packages("ggthemes")
install.packages("ggrepel")
install.packages("ggpubr")
install.packages("dslabs")
install.packages("readr")
install.packages("english")
library(tidyverse)
library(palmerpenguins)
library(nycflights13)
library(plotrix)
library(car)
library(ggplot2)
library(ggthemes)
library(dslabs)
library(readr)
library(stringr)
library(english)
View(penguins)

1 / 200 * 30
(59+73+2) /3
sin(pi/2)

x <- 3*4
x = 3*4
x

primes <- c(2,3,4,7,11,13)
primes

primes * 2
primes - 1

this_is_a_really_long_name <- 2.5

r_rocks <- 2^3

r_rock
R_rocks

seq(from = 1, to = 10)
seq(1,10)

x <- "hello world"
x
x <- "hello"

# Exercises

my_variable <- 10
my_variable

libary(todyverse)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + geom_smooth(mapping = aes(x = displ, y = hwy)), method = "lm")

my_bar_plot <- ggplot(mpg, aes(x=class)) + geom_bar()
my_bar_plot

ggsave(filename = "mpg-plot.png", plot = my_bar_plot)
getwd()

library(nycflights13)

View(flights)

SHORTFLIGHTS <- flights |> filter(air_time < 60)
SHORTFLIGHTS

a <- 1
b <- 2
d <- 3
z <- (a + b)^2 / d
z

x <- seq(1,10)

mean(x, na.rm = TRUE)

flights

flights2 <- flights |>
  mutate(
    speed      = distance / air_time,
    dep_hour   = dep_time %/% 100,
    dep_minute = dep_time %% 100
  )
View(flights2)

# Pipes |>

# x |> f() is same as f(x)
# x |> f(y) is same as f(x,y)

# mutate creates new columns that are functions of existing column

flights |>
  filter(!is.na(arr_delay), !is.na(tailnum)) |>
  count(dest)

flights |>
  group_by(tailnum) |>
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

flights |>
  group_by(month) |>
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
  ) |>
  ggplot(aes(x = month, y = delay)) +
  geom_point() +
  geom_line()

flights |>
  group_by(dest) |>
  summarize(
    distance = mean(distance),
    speed = mean(distance / air_time, na.rm = TRUE)
  ) |>
  ggplot(aes(x = distance, y =speed)) +
  geom_smooth(
    method = "loess",
    span = 0.5,
    se = FALSE,
    color = "white",
    linewidth = 4
  ) +
  geom_point()

#Lecture 2 (13/10/2025)

draw.circle(3,2,c(1,0.66,0.33), border="orange", col=c("red","green","blue"),lty=1,lwd=1)

circle <- function(x,y){
  plot(1:5,seq(1,10,length=5),type="n",xlab="",ylab="",main="Test draw.circle")
  draw.circle(x,y,c(1,0.66,0.33),border="purple", col=c("#ff00ff","#ff77ff","#ffccff"),lty=1,lwd=1)
  draw.circle(2.5,8,0.6,border="red",lty=3,lwd=3)
  draw.circle(4,3,0.7,border="green",col="yellow",lty=1,
              density=5,angle=30,lwd=10)
  draw.circle(3.5,8,0.8,border="blue",lty=2,lwd=2)
}
circle(2,6)

#Tutorial 2 (16/10/2025)

circle <- function(x,y){
  draw.circle(x,y,c(1.0,0.66,0.33),border="red",
              col=c('red','blue','orange'),lty=1,lwd=1)
}
plot(1:5,seq(1,10,length=5),type="n",xlab="",ylab="",main="Test draw.circle") #create empty plot
library(plotrix)
par(pty='s')
circle(3,3)

circle_color <- function(x=3,y=3,a='red',b=c('red','blue','orange')){
  draw.circle(x,y,c(1.0,0.66,0.33),border=a,
              col=b,lty=1,lwd=1)
}
plot(1:5,seq(1,10,length=5),type="n",xlab="",ylab="",main="Test draw.circle") #create empty plot
library(plotrix)
par(pty='s')
circle_color(3,3,'green',c('red','blue','green'))

setwd("C:/Users/slee7/OneDrive - Imperial College London/Introduction to Data Science/Week 2/Week 2")
uk_gdp_andweeklyWage_na <- read.csv("data/uk_gdp_and_weeklyWage_na.csv")
head(uk_gdp_andweeklyWage_na)
is.na(uk_gdp_andweeklyWage_na)
sum(is.na(uk_gdp_andweeklyWage_na))
colSums(is.na(uk_gdp_andweeklyWage_na))
uk_gdp_andweeklyWage_no_NA <- na.omit(uk_gdp_andweeklyWage_na)
sum(is.na(uk_gdp_andweeklyWage_no_NA))
nrow(uk_gdp_andweeklyWage_na) - nrow(uk_gdp_andweeklyWage_no_NA)
compute_s_n <- function(n){
  sum <- 0
  for (i in 1:n){
    sum <- sum + i^2
  }
  sum
}
compute_s_n(10)
compute_s_n_vector <- function(n){
  seq <- seq(1,n)
  sum <- sum(seq * seq)
  sum
}
compute_s_n_vector(10)

# Lecture 3 (20/10/2025)

flights %>% arrange(desc(dep_delay))

exponent <- function(x,y){
  x^y
}
x <- 2
y <- 3
exponent(x,y)

y %>% exponent(x,.)

x <- c(-2,-1,0,1,2)
case_when(x < 0 ~ "Negative", x > 0 ~ "Positive", TRUE ~ "Zero")

flights %>% filter(!is.na(dep_delay)) %>%
  mutate(group= case_when(
    dep_delay < 0 ~ "Early departure",
    dep_delay == 0 ~ "On time",
    dep_delay > 60 ~ "Delayed more than 1 hour",
    TRUE ~ "Delayed up to 1 hour")) %>%
  select(dep_delay,group)

flights %>% filter(!is.na(dep_delay)) %>%
  mutate(group= case_when(
    dep_delay < 0 ~ "Early departure",
    between(dep_delay,0,10)~ "On time up to 10 mins",
    dep_delay > 10 ~ "Delayed more than 10 mins")) %>%
  select(dep_delay,group)

flights %>% group_by(month)

flights %>% group_by(year, month, day)

daily <- flights %>% group_by(year, month, day)
daily %>% ungroup()


flights %>% group_by(month) %>%
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE))

flights %>% group_by(month) %>%
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    nrows = n() )

flights %>% group_by(year, month, day) %>%
  summarise(nrows = n())

flights %>% group_by(year, month, day) %>% ungroup() |>
  summarise(nrows = n())

df <- data.frame(
  x = 1:5,
  y = c("a", "b", "a",
        "a", "b"),
  z = c("K", "K", "L",
        "L", "K")
)

df %>% group_by(y)

df %>% arrange(y)

df %>%
  group_by(y) %>%
  summarize(mean_x = mean(x))

df %>%
  group_by(y, z) %>%
  summarize(mean_x = mean(x))

df %>%
  group_by(y, z) %>%
  mutate(mean_x = mean(x))

cars <- mtcars
View(cars)
mpg_col <- select(cars,mpg)
mpg_col_2 <- cars %>% select(mpg)
# print(mpg_col)

cars_6_cyl <- filter(cars, cyl == 6)
cars_6_cyl_2 <- cars %>% filter(cyl == 6)
cars %>% filter(cyl == 6) %>% select(mpg)
# Main way to write pipes %>%

# Tutorial 3 (23/10/2025)
data("EuStockMarkets")
stock_data <- as.data.frame(EuStockMarkets)
stock_data$Date <- seq(as.Date("1991-01-01"), by = "days", length.out = nrow(stock_data))
head(stock_data)

mean_stock_data <- stock_data %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(
    mean_DAX = mean(DAX, na.rm = TRUE),
    mean_SMI = mean(SMI, na.rm = TRUE),
    mean_CAC = mean(CAC, na.rm = TRUE),
    mean_FTSE = mean(FTSE, na.rm = TRUE)
  ) 

max_DAX <- max(mean_stock_data %>% select(mean_DAX))
sorted_mean_stock_data <- arrange(mean_stock_data, desc(mean_DAX))
head(sorted_mean_stock_data)

data("LifeCycleSavings")
savings_data <- as.data.frame(LifeCycleSavings)
savings_data %>%
  mutate(savings_category = case_when(
    sr < 10 ~ "Low Savings",
    between(sr,10,20) ~ "Moderate Savings",
    sr > 20 ~ "High Savings"
    )
  ) %>%
  select(sr, savings_category) %>%
  head(10)

savings_rate <- savings_data %>%
  pull(sr)
print(savings_rate)

savings_rate <- savings_data %>%
  .$sr
head(savings_rate, 10)

# Lecture 4 (27/10/2025)

install.packages("dslabs")
library(dslabs)
data(murders)
head(murders)

setwd("C:/Users/slee7/OneDrive - Imperial College London/Introduction to Data Science/Week 4")
uk_gdp_and_weeklyWage <- read.csv("uk_gdp_and_weeklyWage2.csv", stringsAsFactors =  F)

# Categorize Weekly_pay into quartiles
uk_gdp_and_weeklyWage <- uk_gdp_and_weeklyWage %>%
  mutate(GDP_growth_rate = (GDP_m - lag(GDP_m)) / lag(GDP_m) * 100,
         weeklyWageQuantile = cut(Weekly_pay, quantile(Weekly_pay, probs = seq(0,1,0.25), type = 7, na.rm = T),
                                  include.lowest = T, labels = F))

# Categorize GDP_growth_rate into quartiles
uk_gdp_and_weeklyWage <- uk_gdp_and_weeklyWage %>%
  mutate(GDP_growth_rate_Quantile = cut(GDP_growth_rate, quantile(GDP_growth_rate, probs = seq(0,1,0.25), type = 7, na.rm = T),
                                        include.lowest = T, labels = F))

# Excludes the starting year, which doesn't have a GDP_growth_rate to calculate
uk_gdp_and_weeklyWage <- uk_gdp_and_weeklyWage %>%
  filter(!is.na(GDP_growth_rate)) 

# Scatterplot
uk_gdp_and_weeklyWage %>% ggplot(aes(x=Year, y=Weekly_pay)) + 
  geom_point( )

# Line Plot
uk_gdp_and_weeklyWage %>% ggplot(aes(x=Year, y=Weekly_pay)) + 
  geom_line( )

# Define ggplot() object on data table
p <- uk_gdp_and_weeklyWage %>% ggplot() 

# Add geom_point and geom_line geometries
p + geom_point(aes(x=Year, y=Weekly_pay)) + 
  geom_line(aes(x=Year, y=Weekly_pay)) 

# Global Aesthetic Mappings
p <- uk_gdp_and_weeklyWage %>% ggplot(aes(x=Year, y=Weekly_pay))
p + geom_point() + 
  geom_line() 

# We can set up using labs
p <- uk_gdp_and_weeklyWage %>% ggplot(aes(x=Year, y=Weekly_pay)) 
p + geom_point( ) + 
  geom_line()  +
  labs(
    title ="Weekly Wage between 2001 and 2022 (UK)",
    y = "Weekly Wage (£)"
  )

# Include theme and size
p <- uk_gdp_and_weeklyWage %>% ggplot(aes(x=Year, y=Weekly_pay)) 
p + geom_point(size=2) + # size = 2 for changing scatter points size
  geom_line()  +
  labs(
    title ="Weekly Wage between 2001 and 2022 (UK)",
    y = "Weekly Wage (£)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  ) 

# Exercise 1

# Create a new column "GDP_growth_rate"
uk_gdp_and_weeklyWage <- uk_gdp_and_weeklyWage %>%
  mutate(GDP_growth_rate = (GDP_m - lag(GDP_m)) / lag(GDP_m) * 100) %>%
  filter(!is.na(GDP_growth_rate)) 
  
# Define a ggplot object
p <- uk_gdp_and_weeklyWage %>% ggplot(aes(x=Year, y=GDP_growth_rate, colour='red'))

# Add layers to create the desired plot
p + geom_point() + 
  geom_line() 

# Exercise 1 End

# scale\_factor = (max_{2nd\ dataset} - min_{2nd\ dataset}) / (max_{1st\ dataset} -  min_{1st\ dataset}) 

scale_factor <- (max(uk_gdp_and_weeklyWage$GDP_growth_rate)-
                   min(uk_gdp_and_weeklyWage$GDP_growth_rate))/
  (max(uk_gdp_and_weeklyWage$Weekly_pay)-
     min(uk_gdp_and_weeklyWage$Weekly_pay))

scale_factor <- scale_factor/2 # further adjustments to the scale factor by trial and error

p <- uk_gdp_and_weeklyWage %>% ggplot(aes(x=Year, y=Weekly_pay)) 
p + geom_point(size=2) + 
  geom_line()  +
  geom_point(aes(y = GDP_growth_rate/scale_factor),size=2, colour = 'red') +
  geom_line(aes(y = GDP_growth_rate/scale_factor),colour = 'red') +
  labs(
    title ="Weekly Wage between 2001 and 2022 (UK)",
    y = "Weekly Wage (£)"
  ) +
  scale_y_continuous(sec.axis = sec_axis(~. * scale_factor, name= "GDP Growth Rate (%)")) +
  theme(axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"), 
        axis.title.y.right = element_text(colour = "red"), 
        axis.text.y.right = element_text(color = "red")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  )

# Exercise 2

head(uk_gdp_and_weeklyWage)
# Create a new column for GDP per capita "GDP_per_cap"
uk_gdp_and_weeklyWage <- uk_gdp_and_weeklyWage %>%
  mutate(GDP_per_cap = (GDP_m * 1000000 / Population)) %>%
  filter(!is.na(GDP_per_cap))
  
# Define a new scale factor for plotting "GDP_per_cap" and 
# "Weekly_Wage_pay" on the same plot
  
scale_factor <- (max(uk_gdp_and_weeklyWage$GDP_per_cap)-
                   min(uk_gdp_and_weeklyWage$GDP_per_cap))/
  (max(uk_gdp_and_weeklyWage$Weekly_pay)-
     min(uk_gdp_and_weeklyWage$Weekly_pay))
  
scale_factor = scale_factor*2 # further adjustments to the scale factor by trial and error

# Make the desired plot using ggplot()
head(uk_gdp_and_weeklyWage)
p <- uk_gdp_and_weeklyWage %>% ggplot(aes(x=Year, y=Weekly_pay)) 
p + geom_point(size=2) + 
  geom_line()  +
  geom_point(aes(y = GDP_per_cap/scale_factor),size=2, colour = 'red') +
  geom_line(aes(y = GDP_per_cap/scale_factor),colour = 'red') +
  labs(
    title ="Weekly Wage between 2001 and 2022 (UK)",
    y = "Weekly Wage (£)"
  ) +
  scale_y_continuous(sec.axis = sec_axis(~. *scale_factor, name= "GDP per Capita")) +
  theme(axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"), 
        axis.title.y.right = element_text(colour = "red"), 
        axis.text.y.right = element_text(color = "red")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  )

#Exercise 2 End

# Tutorial 4 (30/10/2025)

# Creating plot
p1 <- uk_gdp_and_weeklyWage %>%
  ggplot(aes(x = Year, y = Weekly_pay)) +
  geom_point(size = 2) +
  geom_line() +
  xlab("Year") +
  ylab("Weekly Wage (£)") +
  ggtitle("Weekly Wage between 2000 and 2022 (UK)")
print(p1)

p2 <- uk_gdp_and_weeklyWage %>%
  mutate(weeklyWageQuantile = as.factor(weeklyWageQuantile)) %>%
  ggplot(aes(x = Year, y = Weekly_pay, colour = weeklyWageQuantile)) + # map weeklyWageQuantile to col
  geom_point(size = 2) +
  geom_line(aes(group = weeklyWageQuantile)) + # ensure lines connect points within each weeklyWageQuantile
  xlab("Year") +
  ylab("Weekly Wage (£)") +
  ggtitle("Weekly Wage between 2000 and 2022 (UK)") +
  scale_colour_manual(values = c("blue", "red", "green", "orange")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )
print(p2)

# Exercise 2
data("LifeCycleSavings")
savings_data <- as.data.frame(LifeCycleSavings)
head(savings_data)

savings_data <- savings_data %>%
  mutate(savings_category = case_when(
    sr < 10 ~ "Low Savings",
    between(sr,10,20) ~ "Moderate Savings",
    sr > 20 ~ "High Savings"
  ))
head(savings_data)

p2 <- savings_data %>%
  ggplot(aes(x=dpi,y=sr, color = savings_category)) +
  geom_point(size = 2) + 
  scale_color_manual(values = c("Low Savings"="blue","Moderate Savings"="orange","High Savings"="green")) +
  ggtitle("Disposable Income vs Savings Rate") +
  xlab("Disposable Income") +
  ylab("Savings Rate") +
  labs(color = "Savings Category")
print(p2)

# Exercise 3

p3 <- savings_data %>%
  ggplot(aes(x=dpi,y=sr, color = savings_category)) +
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_manual(values = c("Low Savings"="blue","Moderate Savings"="orange","High Savings"="green")) +
  ggtitle("Disposable Income vs Savings Rate") +
  xlab("Disposable Income") +
  ylab("Savings Rate") +
  labs(color = "Savings Category")
print(p3)

# Lecture 5 (3/11/2025)

install.packages("dslabs")
library(dslabs)
data(murders)
head(murders)

p <- murders %>%
  ggplot(aes(population/10^6,
             total, label = abb)) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)")+ 
  ggtitle("US Gun Murders in 2010") +
  geom_point(aes(col=region), size = 3)
p

r <- murders %>%
  summarize(rate = sum(total)/sum(population)*10^6) %>%
  pull(rate)

p <- p +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  scale_color_discrete(name = "Region")
p

# ggrepel and ggthemes
library(ggthemes)
library(ggrepel)

murders %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

bins_5 <- murders %>%
  ggplot(aes(total)) +
  geom_histogram(bins = 5)
bins_10 <- murders %>%
  ggplot(aes(total)) +
  geom_histogram(bins = 10)
bins_20 <- murders %>%
  ggplot(aes(total)) +
  geom_histogram(bins = 20)
bins_30 <- murders %>%
  ggplot(aes(total)) +
  geom_histogram(bins = 30)


library(ggpubr)
ggarrange(bins_5, bins_10, bins_20, bins_30,
          labels=c("5 bins", "10 bins", "20 bins", "30 bins"),
          vjust=14.5, ncol = 2, nrow = 2)

murders %>% ggplot(aes(x=total, y = after_stat(density))) +
  geom_histogram(bins=10) +
  geom_density()

# Week 5 Handout
setwd("C:/Users/slee7/OneDrive - Imperial College London/Introduction to Data Science/Week 5")
uk_gdp_and_weeklyWage <- read.csv("uk_gdp_and_weeklyWage2.csv",
                                  stringsAsFactors = F)

uk_gdp_and_weeklyWage <- uk_gdp_and_weeklyWage %>%
  mutate(GDP_growth_rate = (GDP_m - lag(GDP_m)) / lag(GDP_m) * 100,
         weeklyWageQuantile = cut(Weekly_pay,
                                  quantile(Weekly_pay,
                                           probs = seq(0,1,0.25), type = 7),
                                  include.lowest = T, labels = F)
         )
uk_gdp_and_weeklyWage <- uk_gdp_and_weeklyWage %>%
  mutate(GDP_growth_rate_Quantile = cut(GDP_growth_rate,
                                        quantile(GDP_growth_rate,
                                                 probs = seq(0,1,0.25), type = 7, na.rm = T),
                                        include.lowest = T, labels = F))

uk_gdp_and_weeklyWage <- uk_gdp_and_weeklyWage %>%
  mutate(GDP_per_cap = GDP_m / Population *1000000)

uk_gdp_and_weeklyWage <- uk_gdp_and_weeklyWage %>% 
  mutate (wage_level = Weekly_pay > 500) 

view(uk_gdp_and_weeklyWage)

# alpha adjusts transparency
# geom_vline adds vertical line at mean
uk_gdp_and_weeklyWage %>% ggplot(aes(x=Weekly_pay)) +
  geom_histogram(bins = 6, color = "black", fill = "lightblue", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Weekly_pay)),
             color = "blue", linetype = "dashed", linewidth = 1) +
  xlab("Weekly Wage (£)") +
  ylab("Distribution of Weekly Wage") +
  ggtitle("Distribution of Weekly Wage") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# *Exercise 1* 
# Create a histogram of the GDP per capita with light green and dashed border lines, and a red dashed mean line with width 1.5. 

uk_gdp_and_weeklyWage %>%
  ggplot(aes(x = GDP_per_cap)) +
  geom_histogram(bins = 6, linetype = "dashed", color = "black", fill = "lightgreen") +
  geom_vline(aes(xintercept = mean(GDP_per_cap)), color = "red", linetype = "dashed", linewidth = 1.5) +
  theme(
    legend.position = "none"
  )

# *Exercise 2*
# Using a 7-bin histogram, plot the GDP in pink with a blue dashed mean line.

uk_gdp_and_weeklyWage %>%
  ggplot(aes(x = GDP_m)) +
  geom_histogram(bins = 7, linetype = "dashed", color = "black", fill = "pink") +
  geom_vline(aes(xintercept = mean(GDP_m)), color = "blue", linetype = "dashed", linewidth = 1.5) +
  theme(
    legend.position = "none"
  )

#*Exercise 1,2,3*
uk_gdp_and_weeklyWage %>%
  ggplot(aes(x = GDP_per_cap, y = after_stat(density))) +
  geom_histogram(bins = 6, linetype = "dashed", color = "black", fill = "lightgreen") +
  geom_density(alpha = 0.2, fill = "lightpink") +
  geom_vline(aes(xintercept = mean(GDP_per_cap)), color = "blue", linetype = "solid", linewidth = 1.5) +
  theme(
    legend.position = "none"
  )

uk_gdp_and_weeklyWage %>% 
  ggplot(aes(x = Weekly_pay, y = after_stat(density))) +
  geom_histogram(bins = 6, color = "black", fill = "lightblue", linetype = "dashed") +
  geom_density(alpha = 0.5, fill = "lightpink") + # Adjust density appearance
  geom_vline(aes(xintercept = mean(Weekly_pay)),
             color = "blue", linetype = "solid", size = 1) + # Adjust vline appearance
  labs(
    title = "Distribution of Weekly Wage",
    x = "Weekly Wage (£)",
    y = "Density"
  ) +
  theme_minimal() + # Use a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Adjust title appearance
    axis.text = element_text(size = 10), # Adjust axis text size
    axis.title = element_text(size = 12), # Adjust axis title size
    panel.grid.major = element_line(colour = "gray90"), # Adjust grid line color
    panel.grid.minor = element_blank(), # Remove minor grid lines
    legend.position = "none" # Remove legend
  )

# Tutorial 5 (06/11/2025)

uk_gdp_and_weeklyWage %>% ggplot(aes(x=Weekly_pay,y=after_stat(density))) +
  geom_histogram(bins = 6, color = "black", fill = "lightblue", linetype = "3313") +
  geom_density(fill = "yellow", alpha = 0.4, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(Weekly_pay), color = "Mean"), linetype = "solid", linewidth = 1) +
  geom_vline(aes(xintercept = median(Weekly_pay), color = "Median"), linetype = "dashed", linewidth = 1) +
  scale_color_manual(name = "Statistic", values = c("Mean" = "blue", "Median" = "red")) +
  labs(
    title = "Distribution of Weekly Wage",
    x = "Weekly Pay (£)",
    y = "Distribution of Weekly Wage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Adjust title appearance
    axis.text = element_text(size = 10, angle = 30, family = "serif"), # Adjust axis text size
    axis.title = element_text(size = 12, face = "italic"), # Adjust axis title size
    panel.grid.major = element_line(colour = "gray90"), # Adjust grid line color
    panel.grid.minor = element_blank(), # Remove minor grid lines
  )

# Lecture 6 (10/11/2025)

path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
head(wide_data)
new_tidy_data <- gather(wide_data, year, fertility, '1960':'2015')
head(new_tidy_data)
new_tidy_data2 <- wide_data %>% gather(year, fertility, -country)
head(new_tidy_data2)

class(new_tidy_data$year)

new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

new_tidy_data3 <- wide_data %>%
  gather(year, fertility, -country) %>%
  mutate(year = as.integer(year))
head(new_tidy_data3)

new_tidy_data %>% ggplot(aes(year, fertility, color=country)) +
  geom_point() +
  labs(
    title = "Fertility in South Korea and Germany 1960-2015",
    x = "Year",
    y = "Fertility"
  ) +
  scale_color_discrete(name = " ") +
  theme_economist()

# Lecture 7
head(diamonds)
# Carat is a measure of diamond weight; one carat is equivalent to 0.2 grams
# Clarity refers to how clear a diamond is. Diamonds often contain imperfections like cracks or mineral deposits. The fewer and less noticeable a diamond’s imperfections, the better its clarity. Clarity contains 8 ordered levels, from “I1” (the worst) to “IF” (the best).
# Color refers to the color of the diamond. Colorless diamonds are considered better than diamonds with a tint. The data set contains diamonds of 7 different colors, represented by different letters. D, E, F diamonds are considered colorless, while G, H, I, J diamonds have a very faint color
# Cut refers to how a rough diamond is shaped into a finished diamond. Better cuts create more symmetrical and luminous diamonds. cut has 5 ordered levels: “Fair,” “Good,” “Very Good,” “Premium,” “Ideal.”
# x, y, z, depth, and table are various measures of a diamond’s size, in millimeters.

ggplot(diamonds, aes(x=carat)) +
  geom_histogram(binwidth = 0.5)
smaller <- diamonds %>% filter(carat < 3)
ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(diamonds, aes(x=y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0,50))
unusual <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  select(price, x, y, z) %>%
  arrange(y)
unusual

# Drop the entire row with the strange values:
diamonds2 <- diamonds %>% filter(between(y,3,20))

# Replacing unusual values with missing values
diamonds2 <- diamonds %>% mutate(y =if_else(y < 2 | y > 20, NA, y))

ggplot(diamonds2, aes(x=x,y=y)) +
  geom_point()

new <- nycflights::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %>% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60))

new %>%
  ggplot(aes(x = sched_dep_time, after_state(density))) +
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4)

ggplot(diamonds2, aes(x = price, y = after_stat(density))) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

ggplot(diamonds2, aes(x = cut, y = price)) +
  geom_boxplot()

# Flip 90 degrees
ggplot(diamonds2, aes(x = price, y = cut)) +
  geom_boxplot()*

ggplot(diamonds2, aes(x = cut, y = color)) +
  geom_count()

diamonds2 %>% count(color, cut) %>% ggplot(aes(x = cut, y = color)) + 
  geom_tile(aes(fill = n))

ggplot(smaller, aes(x = carat, y = price)) +
  geom_point()

ggplot(smaller, aes(x = carat, y =price)) +
  geom_point(alpha = 1/100)

ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)), varwidth = TRUE)

# Correlation between price and weight
# Fair diamonds are bigger so more pricier?

new_tidy_data <- gather(wide_data, year, fertility, `1960`:`2015`)
head(new_tidy_data)

new_tidy_data <- wide_data %>% gather(year, fertility, -country)

# Tutorial 7 (20/11/2025)

setwd('C:/Users/slee7/OneDrive - Imperial College London/Introduction to Data Science/Week 7')

Eng_population <- read.csv("./data2/ONS-England_population.csv",
                           stringsAsFactors = F)
Wales_population <- read.csv("./data2/ONS-Wales_population.csv",
                             stringsAsFactors = F)
Eng_Wales_GDP_GR <- read.csv("./data2/ONS-England-Wales_GDP_GrowthRate.csv",
                             stringsAsFactors = F)

Eng_population <- Eng_population[8:58, ]
colnames(Eng_population) <- c("Year", "Population")
Eng_population <- Eng_population %>%
  mutate(Year = as.numeric(Year), Population = as.numeric(Population),
         Region = "England")
Wales_population <- Wales_population[8:58, ]
colnames(Wales_population) <- c("Year", "Population")
Wales_population <- Wales_population %>%
  mutate(Year = as.numeric(Year), Population = as.numeric(Population),
         Region = "Wales")

Eng_Wales_population <- rbind(Eng_population, Wales_population)
head(Eng_Wales_population)

inner_join_data1 <- Eng_Wales_GDP_GR %>%
  inner_join(Eng_Wales_population,
             by = c("calendar.years" = "Year", "Geography" = "Region"))
head(inner_join_data1)

left_join_data1 <- Eng_Wales_GDP_GR %>%
  left_join(Eng_Wales_population,
            by = c("calendar.years" = "Year", "Geography" = "Region"))
head(left_join_data1)

right_join_data1 <- Eng_Wales_GDP_GR %>%
  right_join(Eng_Wales_population,
             by = c("calendar.years" = "Year", "Geography" = "Region"))
head(right_join_data1)

full_join_data1 <- Eng_Wales_GDP_GR %>%
  full_join(Eng_Wales_population,
            by = c("calendar.years" = "Year", "Geography" = "Region"))
head(full_join_data1)

inner_join_data1 %>% ggplot(aes(x=calendar.years, y=GrowthRate, color=Geography, label=Geography)) +
  geom_point(aes(color = Geography, shape = Geography), size = 3) +
  geom_smooth(method = lm, aes(color = Geography, fill = Geography, lty = Geography)) +
  labs(
    x = "Years",
    y = "Growth Rate",
    title = "England and Wales Growth Rate by Year"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size=12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    legend.title = element_text(size = 12, face = "bold")
  ) +
  scale_x_continuous(breaks = 2013:2021,
                     limits = c(2013,2021)) +
  scale_color_manual(values = c("England" = "darkorange", "Wales" = "darkgreen")) +
  scale_shape_manual(values = c("England" = 16, "Wales" = 17)) +
  scale_(values = c("England" = "darkorange", "Wales" = "darkgreen"))

# Tutorial 8 (26/11/2025)

str_replace(string = "5ft8", pattern = "ft", replacement = "'")
str_replace_all(string = "between 5ft8 and 5ft10", pattern = "ft", replacement = "'")
str_detect(string = "5'11", pattern = "^[4-7]'[0-9]{1,2}$")
# ˆ means start of the string
# [4-7] matches one digit between 4 and 7
# ' matches the literal apostrophe
# [0-9]{1,2} matches one or two digits from 0 to 9
# $ means end of the string

str_trim(" 5'11 ")
str_to_lower("Five Feet Eight Inches")
# Regez patterns
# Digits: \d (any digit), \d{1,2} (one or two digits)
# Character classes: [4-7] (any number 4,5,6,7)
# Optional whitespace: \s* (zero or more spaces)
# Anchors: ^ (start of string) and $ (end of string)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  is.na(inches) | inches < smallest | inches > tallest
}

not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) & ((inches >= smallest & inches <= tallest) |
                             (inches/2.54 >= smallest & inches <= tallest))
  !ind
}

words_to_numbers <- function(s){
  s <- str_to_lower(s)
  for(i in 0:11){
    s <- str_replace_all(s, words(i), as.character(i))
  }
  s
}

convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>%
    str_replace_all("inches|in|''|\"|cm|and", "") %>%
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>%
    str_replace("^([56])'?$", "\\1'0") %>%
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>%
    str_trim()
}

# Exercise 1: Identify valid vs invalid heights
vec <- c("5'11","174","5'72","six foot one","200","4'12")
vec[str_detect(vec, "^[4-6]\\s*'\\s*\\d{1,2}")]

# Exercise 2: Clean Formats
raw <- c("Five foot eight inches", "5 ft 10", "6, 2", "170cm")
raw %>% words_to_numbers %>% convert_format()

# Exercise 3: Extract feet and inches
h <- c("5'10", "6'1\"", "5'8 inches")
data.frame(h) %>% extract(h, c("feet", "inches"), regex="(\\d)'(\\d{1,2})")
