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

# Apply to numerical columns
data_daily <- data_daily %>% mutate_if(is.numeric, remove_outliers)
data_daily <- data_daily %>% mutate_all(~ifelse(is.na(.), zoo::na.approx(.), .))


