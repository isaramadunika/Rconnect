library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(caret)
library(prophet)

# Read the datasets
data_daily <- read.csv("C:\\Users\\shard\\Desktop\\R_Studio_Connect\\TeleCom_2009_2023_Daily.csv")

#Cheake the dataset 
df = data.frame(data_daily)
df

#Print the head 
head(df)

# Inspect structure and summary
str(df)
summary(df)

# Convert Date to Date format and ensure proper ordering
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

# Check for missing values
colSums(is.na(df))  # Count of missing values in each column

# Fill missing values (if any)
df <- df %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), zoo::na.approx(.), .)))  # Interpolate missing numeric values

#Remove outliers using IQR (Interquartile Range) method
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)] <- NA
  return(x)
}

df <- df %>%
  mutate(across(where(is.numeric), remove_outliers))  # Apply to numeric columns


#Fill missing values again after removing outliers
df <- df %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), zoo::na.approx(.), .)))


