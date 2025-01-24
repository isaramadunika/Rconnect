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




