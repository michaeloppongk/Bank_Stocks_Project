# Project: Stock Performance of Four Largest U.S. Banks (2022-2023)
# Author: [Your Name]
# Description: This project analyzes the stock performance of four major U.S. banks:
#              Bank of America (BAC), Citibank (Citi), JPMorgan Chase (JPM), and Wells Fargo (WFC)
#              from 2022 to 2023. Key metrics such as stock prices, volatility, and
#              percentage changes are visualized to provide insights into their performance.
# Tools: Tidyverse for data manipulation, ggplot2 for visualization, Plotly for interactive plots

# --------------------------------------------------------
# 1. Load Required Libraries
# --------------------------------------------------------

library(tidyverse)
library(plotly)  # For interactive visualization
library(gridExtra)  # For arranging multiple plots in a grid

# --------------------------------------------------------
# 2. Load and Clean Data
# --------------------------------------------------------

# Load bank stock data from CSV file
bank_data <- read.csv("bank_stocks_2022_2023.csv")

# Convert date column to Date type
bank_data$date <- as.Date(bank_data$date, format = "%m/%d/%Y")

# --------------------------------------------------------
# 3. Stock Price Visualization for 4 Banks
# --------------------------------------------------------

# Line plot showing stock prices of the four banks over time
p1 <- ggplot(bank_data, aes(x = date, y = close_price, color = bank_name)) +
  geom_line() +
  labs(title = "Stock Performance of Four Largest U.S. Banks (2022-2023)",
       x = "Date", 
       y = "Close Price (USD)",
       color = "Bank Name") +
  theme_classic() +
  theme(legend.position = "bottom")

# Convert the plot to an interactive version using Plotly
p1.a <- ggplotly(p1)

# --------------------------------------------------------
# 4. Calculate and Visualize Average Stock Prices
# --------------------------------------------------------

# Calculate the average close price across all banks for each date
average_prices <- bank_data %>%
  group_by(date) %>%
  summarize(average_price = mean(close_price))

# Plot with individual bank stock performance and average price
p2 <- ggplot() +
  geom_line(data = bank_data, aes(x = date, y = close_price, color = bank_name)) +
  labs(title = "Stock Performance of Four Largest U.S. Banks (2022-2023) with Average",
       x = "Date", 
       y = "Close Price (USD)",
       color = "Bank Name") +
  theme_classic() +
  theme(legend.position = "bottom")

# Add the average price line to the plot
p3 <- p2 + geom_line(data = average_prices, aes(x = date, y = average_price, color = "Average"))

# Convert to interactive plot with Plotly
p4 <- ggplotly(p3)

# --------------------------------------------------------
# 5. Summary Statistics for Stock Performance
# --------------------------------------------------------

# Calculate key statistics: mean, median, standard deviation, and percentage change
summary_stats <- bank_data %>%
  group_by(bank_name) %>%
  summarise(
    mean_price = mean(close_price),
    median_price = median(close_price),
    sd_price = sd(close_price),
    mean_perc_change = mean((close_price / lag(close_price) - 1) * 100, na.rm = TRUE),
    sd_perc_change = sd((close_price / lag(close_price) - 1) * 100, na.rm = TRUE)
  )

print(summary_stats)  # View summary statistics for each bank

# --------------------------------------------------------
# 6. Visualize Summary Statistics
# --------------------------------------------------------

# Bar plot for mean stock price
p_mean <- ggplot(summary_stats, aes(x = reorder(bank_name, -mean_price), y = mean_price, fill = bank_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Stock Price (2022-2023)", 
       x = "Bank", 
       y = "Mean Stock Price (USD)") +
  theme_minimal() +
  theme(legend.position = "none")

# Bar plot for median stock price
p_median <- ggplot(summary_stats, aes(x = reorder(bank_name, -median_price), y = median_price, fill = bank_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Median Stock Price (2022-2023)", 
       x = "Bank", 
       y = "Median Stock Price (USD)") +
  theme_minimal() +
  theme(legend.position = "none")

# Bar plot for standard deviation (volatility) of stock price
p_volatility <- ggplot(summary_stats, aes(x = reorder(bank_name, -sd_price), y = sd_price, fill = bank_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Price Volatility (Standard Deviation)", 
       x = "Bank", 
       y = "Standard Deviation (Price)") +
  theme_minimal() +
  theme(legend.position = "none")

# Bar plot for mean percentage change in stock price
p_mean_perc <- ggplot(summary_stats, aes(x = reorder(bank_name, -mean_perc_change), y = mean_perc_change, fill = bank_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Percentage Change in Stock Price (2022-2023)", 
       x = "Bank", 
       y = "Mean Percentage Change (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Bar plot for standard deviation of percentage change (volatility)
p_sd_perc <- ggplot(summary_stats, aes(x = reorder(bank_name, -sd_perc_change), y = sd_perc_change, fill = bank_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage Change Volatility (Standard Deviation)", 
       x = "Bank", 
       y = "Standard Deviation of Percentage Change (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Arrange all summary statistic plots in a grid
grid.arrange(p_mean, p_median, p_volatility, p_mean_perc, p_sd_perc, ncol = 2)

# --------------------------------------------------------
# 7. Conclusion
# --------------------------------------------------------

# In this analysis, we explored the stock performance of four major U.S. banks over a two-year period (2022-2023).
# Through visualization of stock prices, calculation of key summary statistics, and comparisons between banks,
# this project provides valuable insights into the stability and volatility of these financial institutions.
