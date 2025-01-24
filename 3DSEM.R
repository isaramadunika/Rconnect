library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(caret)
library(prophet)

# Read the datasets
data_daily <- read.csv("C:\\Users\\shard\\Desktop\\R_Studio_Connect\\TeleCom_2009_2023_Daily.csv")
data_monthly <- read.csv("C:\\Users\\shard\\Desktop\\R_Studio_Connect\\TeleCom_2009_2023_Monthly.csv")
data_weekly <- read.csv("C:\\Users\\shard\\Desktop\\R_Studio_Connect\\TeleCom_2009_2023_Weekly.csv")

# Inspect structure and summary
str(data_daily)
summary(data_daily)

# Check for missing values
colSums(is.na(data_daily))
colSums(is.na(data_monthly))
colSums(is.na(data_weekly))

# Fill missing values with linear interpolation
data_daily <- data_daily %>% mutate_all(~ifelse(is.na(.), zoo::na.approx(.), .))
data_monthly <- data_monthly %>% mutate_all(~ifelse(is.na(.), zoo::na.approx(.), .))
data_weekly <- data_weekly %>% mutate_all(~ifelse(is.na(.), zoo::na.approx(.), .))

data_daily$Date <- as.Date(data_daily$Date)
data_monthly$Date <- as.Date(data_monthly$Date)
data_weekly$Date <- as.Date(data_weekly$Date)


# Define a function to remove outliers
remove_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x[x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)] <- NA
  return(x)
}

# Apply to numerical columns
data_daily <- data_daily %>% mutate_if(is.numeric, remove_outliers)
data_daily <- data_daily %>% mutate_all(~ifelse(is.na(.), zoo::na.approx(.), .))


# Daily trends
ggplot(data_daily, aes(x = Date, y = Close)) + 
  geom_line() + 
  ggtitle("Daily Stock Price Trends (2009-2023)") +
  xlab("Date") + ylab("Close Price")

# Monthly trends
ggplot(data_monthly, aes(x = Date, y = Close)) + 
  geom_line(color = "blue") + 
  ggtitle("Monthly Stock Price Trends (2009-2023)") +
  xlab("Date") + ylab("Close Price")

# Weekly trends
ggplot(data_weekly, aes(x = Date, y = Close)) + 
  geom_line(color = "red") + 
  ggtitle("Weekly Stock Price Trends (2009-2023)") +
  xlab("Date") + ylab("Close Price")


