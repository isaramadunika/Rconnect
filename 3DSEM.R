library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(caret)
library(prophet)

# Read the datasets
stock_data <- read.csv("C:\\Users\\shard\\Desktop\\R_Studio_Connect\\TeleCom_2009_2023_Daily.csv", stringsAsFactors = FALSE)
head(stock_data)

# Convert Date column
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Convert numeric columns
stock_data <- stock_data %>%
  mutate(
    Price = as.numeric(Price),
    Open = as.numeric(Open),
    High = as.numeric(High),
    Low = as.numeric(Low),
    Vol. = as.numeric(gsub("K", "", Vol.)) * 1000, # Convert volume to numeric
    Change.. = as.numeric(gsub("%", "", Change..)) # Remove percentage sign
  )

# Check for missing values and handle them
stock_data <- na.omit(stock_data)
summary(stock_data)

ggplot(stock_data, aes(x = Date, y = Price)) +
  geom_line(color = "blue") +
  labs(title = "Stock Price Over Time", x = "Date", y = "Price")

ggplot(stock_data, aes(x = Price)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Price Distribution", x = "Price", y = "Frequency")

correlation_matrix <- cor(stock_data %>% select(-Date))
print(correlation_matrix)

set.seed(123)
train_index <- createDataPartition(stock_data$Price, p = 0.8, list = FALSE)
train_data <- stock_data[train_index, ]
test_data <- stock_data[-train_index, ]

# Convert the Price column to a time series object
ts_data <- ts(train_data$Price, frequency = 252) # Assuming 252 trading days in a year

# Fit the ARIMA model
model <- auto.arima(ts_data)

summary(model)

