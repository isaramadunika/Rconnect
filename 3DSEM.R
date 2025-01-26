# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(caret)
library(randomForest)
library(lubridate)
library(forecast)


# Read the dataset
telecom_data <- read.csv("C:\\Users\\shard\\Desktop\\R_Studio_Connect\\TeleCom_2009_2023_Daily.csv", stringsAsFactors = FALSE)

# Convert the dataset columns to appropriate formats
telecom_data <- telecom_data %>% 
  mutate(Date = mdy(Date), 
         Price = as.numeric(Price), 
         Open = as.numeric(Open), 
         High = as.numeric(High), 
         Low = as.numeric(Low), 
         Volume = as.numeric(gsub("K", "", gsub("\\.", "", Vol.))) * 1000, 
         Change = as.numeric(gsub("%", "", Change..)) / 100)

# Check the structure of the cleaned data
print(head(telecom_data))

# Check for rows with NA values in the Volume column
na_volume_rows <- telecom_data %>% filter(is.na(Volume))
print(na_volume_rows)

# Remove rows with NA values in the Volume column
telecom_data <- telecom_data %>% filter(!is.na(Volume))
# Verify the cleaned dataset
print(head(telecom_data))

# Improved Line Plot with Smoothing for Stock Price Trend Over Time
ggplot(telecom_data, aes(x = Date, y = Price)) +
  geom_line(color = "steelblue", size = 0.2) +
  geom_smooth(method = "loess", se = TRUE, color = "red", linetype = "dashed") +
  labs(
    title = "SLT Stock Price Over Time",
    subtitle = "Line chart displaying the price trend over the years with smoothed trendline",
    x = "Date",
    y = "Stock Price (LKR)"
  ) +
  theme_light() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
    axis.title = element_text(face = "bold")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  # Adjust for readability

correlation_matrix <- cor(stock_data %>% select(-Date))

ggplot(telecom_data, aes(x = Price)) +
  geom_histogram(bins = 50, fill = "#3498db", color = "white", alpha = 0.7) +  # Soft blue color for bars
  labs(
    title = "Distribution of Stock Prices",
    x = "Price (LKR)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  # Bigger text for readability
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered title
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 12),  # Larger axis tick labels
    panel.grid.major = element_line(color = "gray90"),  # Subtle major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines for simplicity
    plot.margin = unit(c(10, 20, 10, 20), "pt")  # Add margins to make the plot more readable
  )


ggplot(telecom_data, aes(x = Volume, y = Price, color = Volume)) +
  geom_point(alpha = 0.7) +  # Adjust transparency for better visibility
  labs(
    title = "Volume vs Price Relationship",
    x = "Trading Volume",
    y = "Price (LKR)"
  ) +
  scale_x_log10() +  # Log scale for volume due to large range
  scale_color_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = median(telecom_data$Volume)) +  # Color gradient with mid
  theme_minimal() +
  theme(
    text = element_text(size = 14),  # Larger text for readability
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered title
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 12),  # Larger axis tick labels
    panel.grid.major = element_line(color = "gray90"),  # Subtle major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    plot.margin = unit(c(10, 20, 10, 20), "pt")  # Correct margin function with unit() for ggplot
  )

# Convert data to time series object
ts_data <- ts(telecom_data$Price, frequency = 252)  # 252 trading days per year


# Fit ARIMA model
model <- auto.arima(ts_data)

# Generate forecast until 2030
forecast_periods <- (2030 - 2023) * 252  # Number of trading days until 2030
future_forecast <- forecast(model, h = forecast_periods)

# Plot the forecast
plot(future_forecast, main = "TeleCom Stock Price Forecast to 2030",
     xlab = "Time", ylab = "Price")

# Print model summary
print(summary(model))

# Display the forecast plot and model summary
plot(future_forecast, main = "TeleCom Stock Price Forecast to 2030",
     xlab = "Time", ylab = "Price")

# Print model summary
print(summary(model))

# Plot the forecast with confidence intervals
plot(future_forecast, main = "TeleCom Stock Price Forecast to 2030",
     xlab = "Year", ylab = "Price",
     col.main = "blue", col.lab = "darkgreen")


# Convert forecast to dataframe for ggplot
forecast_df <- data.frame(
  Time = time(future_forecast$mean),
  Forecast = as.numeric(future_forecast$mean),
  Lower80 = as.numeric(future_forecast$lower[,1]),
  Upper80 = as.numeric(future_forecast$upper[,1]),
  Lower95 = as.numeric(future_forecast$lower[,2]),
  Upper95 = as.numeric(future_forecast$upper[,2])
)

# Add historical data
historical_df <- data.frame(
  Time = time(ts_data),
  Price = as.numeric(ts_data)
)

# Create the plot
ggplot() +
  # Historical data
  geom_line(data = historical_df, aes(x = Time, y = Price, color = "Historical"), size = 1) +
  # Forecast
  geom_line(data = forecast_df, aes(x = Time, y = Forecast, color = "Forecast"), size = 1) +
  # Confidence intervals
  geom_ribbon(data = forecast_df, aes(x = Time, ymin = Lower95, ymax = Upper95, fill = "95% CI"), alpha = 0.2) +
  geom_ribbon(data = forecast_df, aes(x = Time, ymin = Lower80, ymax = Upper80, fill = "80% CI"), alpha = 0.3) +
  # Customize colors and labels
  scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red")) +
  scale_fill_manual(values = c("95% CI" = "gray70", "80% CI" = "gray50")) +
  labs(title = "TeleCom Stock Price Forecast to 2030",
       subtitle = "Historical Data (2009-2023) and Forecast (2024-2030)",
       x = "Year",
       y = "Price",
       color = "Data Type",
       fill = "Confidence Interval") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))
# Calculate accuracy metrics for the ARIMA model
accuracy_metrics <- accuracy(model)
print(accuracy_metrics)


# Calculate forecast accuracy metrics
# Get the forecast values and confidence intervals for 2030
forecast_2030 <- tail(future_forecast$mean, 252)  # Last year of forecast
lower_95 <- tail(future_forecast$lower[,2], 252)  # 95% confidence interval lower bound
upper_95 <- tail(future_forecast$upper[,2], 252)  # 95% confidence interval upper bound

# Calculate prediction interval width
interval_width <- upper_95 - lower_95

# Calculate summary statistics for 2030 prediction
prediction_summary <- data.frame(
  Mean_Price = mean(forecast_2030),
  Min_Price = min(forecast_2030),
  Max_Price = max(forecast_2030),
  Avg_95CI_Width = mean(interval_width)
)

print("Prediction Summary for 2030:")
print(prediction_summary)

# Calculate the coefficient of variation (CV) for the forecast
cv <- sd(forecast_2030) / mean(forecast_2030) * 100
print(paste("Coefficient of Variation for 2030 forecast:", round(cv, 2), "%"))

# Calculate the average prediction interval coverage
coverage <- mean((upper_95 - lower_95) / forecast_2030) * 100
print(paste("Average 95% Prediction Interval Coverage:", round(coverage, 2), "%"))

# Plot the final year forecast with confidence intervals
last_year_df <- data.frame(
  Time = seq_along(forecast_2030),
  Forecast = forecast_2030,
  Lower = lower_95,
  Upper = upper_95
)

ggplot(last_year_df, aes(x = Time)) +
  geom_line(aes(y = Forecast, color = "Forecast"), size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = "95% CI"), alpha = 0.2) +
  scale_color_manual(values = c("Forecast" = "blue")) +
  scale_fill_manual(values = c("95% CI" = "gray70")) +
  labs(title = "Detailed 2030 Stock Price Forecast",
       subtitle = "With 95% Confidence Intervals",
       x = "Trading Days in 2030",
       y = "Predicted Price",
       color = "Legend",
       fill = "Confidence Interval") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Create yearly summary table
# First, let's get the actual data for 2023
telecom_data$Year <- format(telecom_data$Date, "%Y")
actual_2023 <- telecom_data[telecom_data$Year == "2023", ]
actual_2023_avg <- mean(actual_2023$Price)

# Get yearly predictions
yearly_predictions <- data.frame(
  Year = 2023:2030,
  Price = c(actual_2023_avg, tapply(future_forecast$mean[1:(7*252)], 
                                    rep(1:7, each=252), mean)),
  Lower_CI = c(actual_2023_avg, tapply(future_forecast$lower[1:(7*252),2], 
                                       rep(1:7, each=252), mean)),
  Upper_CI = c(actual_2023_avg, tapply(future_forecast$upper[1:(7*252),2], 
                                       rep(1:7, each=252), mean))
)

# Calculate year-over-year changes
yearly_predictions$YoY_Change <- c(NA, diff(yearly_predictions$Price))
yearly_predictions$YoY_Change_Pct <- c(NA, diff(yearly_predictions$Price)/lag(yearly_predictions$Price)[-1] * 100)

# Format the table
yearly_summary <- data.frame(
  Year = yearly_predictions$Year,
  Average_Price = round(yearly_predictions$Price, 2),
  YoY_Change = round(yearly_predictions$YoY_Change, 2),
  YoY_Change_Pct = round(yearly_predictions$YoY_Change_Pct, 2),
  Lower_CI = round(yearly_predictions$Lower_CI, 2),
  Upper_CI = round(yearly_predictions$Upper_CI, 2)
)

# Print the summary table
print("Yearly Stock Price Predictions and Changes (2023-2030):")
print(yearly_summary)

# Create an enhanced visualization
library(ggplot2)

# Combine historical and forecast data
historical_data <- data.frame(
  Date = telecom_data$Date,
  Price = telecom_data$Price,
  Type = "Historical"
)

# Create sequence of dates for forecast
last_date <- max(telecom_data$Date)
forecast_dates <- seq(last_date, by="day", length.out=length(future_forecast$mean))
forecast_data <- data.frame(
  Date = forecast_dates,
  Price = future_forecast$mean,
  Lower = future_forecast$lower[,2],
  Upper = future_forecast$upper[,2],
  Type = "Forecast"
)
# Combine the datasets
daily_stock_data <- rbind(
  actual_2023[, c("Date", "Price", "Type")],
  forecast_data[, c("Date", "Price", "Type")]
)

# Print the combined table
print("Daily Stock Price Data (2023-2030):")
print(head(daily_stock_data, 20))  # Show the first 20 rows as a preview

# Save the full table to a CSV file
write.csv(daily_stock_data, "SLT_Daily_Stock_Data_2023_2030.csv", row.names = FALSE)


# Create the enhanced plot
ggplot() +
  # Historical data
  geom_line(data=historical_data, aes(x=Date, y=Price, color="Historical"), size=1) +
  # Forecast
  geom_line(data=forecast_data, aes(x=Date, y=Price, color="Forecast"), size=1) +
  # Confidence intervals
  geom_ribbon(data=forecast_data, 
              aes(x=Date, ymin=Lower, ymax=Upper, fill="95% CI"), 
              alpha=0.2) +
  # Customize colors and labels
  scale_color_manual(values=c("Historical"="blue", "Forecast"="red")) +
  scale_fill_manual(values=c("95% CI"="gray70")) +
  labs(title="TeleCom Stock Price Historical Data and Forecast (2009-2030)",
       subtitle="Historical Data (Blue) and Forecast (Red) with 95% Confidence Interval",
       x="Year",
       y="Price",
       color="Data Type",
       fill="Confidence Interval") +
  theme_minimal() +
  theme(legend.position="bottom",
        plot.title=element_text(size=14, face="bold"),
        plot.subtitle=element_text(size=12)) +
  scale_x_date(date_breaks="2 years", date_labels="%Y")




# Combine actual 2023 data with forecasted data for 2024-2030
# Extract actual 2023 data
actual_2023 <- telecom_data[telecom_data$Year == "2023", c("Date", "Price")]
actual_2023$Type <- "Actual"

# Extract forecasted data for 2024-2030
forecast_dates <- seq(max(telecom_data$Date) + 1, by = "day", length.out = length(future_forecast$mean))
forecast_data <- data.frame(
  Date = forecast_dates,
  Price = future_forecast$mean,
  Lower = future_forecast$lower[, 2],
  Upper = future_forecast$upper[, 2],
  Type = "Forecast"
)



# Combine the datasets
daily_stock_data <- rbind(
  actual_2023[, c("Date", "Price", "Type")],
  forecast_data[, c("Date", "Price", "Type")]
)

# Print the combined table
print("Daily Stock Price Data (2023-2030):")
print(head(daily_stock_data, 20))  # Show the first 20 rows as a preview

# Create a summary table by month and year
daily_stock_data$YearMonth <- format(daily_stock_data$Date, "%Y-%m")
monthly_summary <- aggregate(Price ~ YearMonth, data = daily_stock_data, 
                             FUN = function(x) c(
                               Mean = mean(x),
                               Min = min(x),
                               Max = max(x)
                             ))



# Convert the results to a more readable format
monthly_summary_df <- data.frame(
  YearMonth = monthly_summary$YearMonth,
  Mean_Price = round(monthly_summary$Price[,1], 2),
  Min_Price = round(monthly_summary$Price[,2], 2),
  Max_Price = round(monthly_summary$Price[,3], 2)
)



# Print the monthly summary
print("Monthly Summary of Stock Prices (2023-2030):")
print(head(monthly_summary_df, 100))  # Show first 20 months as preview


monthly_summary_df$Year <- substr(monthly_summary_df$YearMonth, 1, 4)
monthly_summary_df$Month <- substr(monthly_summary_df$YearMonth, 6, 7)

ggplot(monthly_summary_df, aes(x = Month, y = Year, fill = Mean_Price)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = median(monthly_summary_df$Mean_Price)) +
  labs(title = "Monthly Stock Price Heatmap (2023-2030)",
       x = "Month",
       y = "Year",
       fill = "Average Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the full table to a CSV file
write.csv(daily_stock_data, "SLT_Monthly_Summery_Data_2023_2030.csv", row.names = FALSE)
















