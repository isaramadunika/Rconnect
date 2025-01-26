# Load the necessary libraries
library(shiny)
library(ggplot2)
library(forecast)
library(dplyr)
library(lubridate)

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Telecom Stock Price Analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h4("Stock Price Forecast"),
      p("This dashboard presents the analysis of SLT Telecom Stock Price over time, along with its forecast."),
      # Add options for interactive features
      selectInput("plot_type", "Choose Plot Type:",
                  choices = c("Price Over Time", "Distribution of Stock Prices", "Volume vs Price Relationship", "Forecast")),
      sliderInput("forecast_years", "Select Forecast Years:",
                  min = 2023, max = 2030, value = 2025, step = 1),
      downloadButton("download_data", "Download Data")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Stock Price Analysis", 
                 plotOutput("price_plot"),
                 plotOutput("volume_price_plot")),
        tabPanel("Forecast", 
                 plotOutput("forecast_plot"),
                 tableOutput("forecast_summary"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to get the forecast data based on user input
  forecast_data <- reactive({
    # Forecast period
    forecast_periods <- (input$forecast_years - 2023) * 252  # Number of trading days for selected years
    future_forecast <- forecast(model, h = forecast_periods)
    
    # Convert to dataframe for ggplot
    forecast_df <- data.frame(
      Time = time(future_forecast$mean),
      Forecast = as.numeric(future_forecast$mean),
      Lower80 = as.numeric(future_forecast$lower[,1]),
      Upper80 = as.numeric(future_forecast$upper[,1]),
      Lower95 = as.numeric(future_forecast$lower[,2]),
      Upper95 = as.numeric(future_forecast$upper[,2])
    )
    forecast_df
  })
  
  # Render the selected plot based on user input
  output$price_plot <- renderPlot({
    if(input$plot_type == "Price Over Time") {
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
        scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    }
  })
  
  # Volume vs Price Relationship plot
  output$volume_price_plot <- renderPlot({
    ggplot(telecom_data, aes(x = Volume, y = Price, color = Volume)) +
      geom_point(alpha = 0.7) +
      labs(
        title = "Volume vs Price Relationship",
        x = "Trading Volume",
        y = "Price (LKR)"
      ) +
      scale_x_log10() +
      scale_color_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = median(telecom_data$Volume)) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 12),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(10, 20, 10, 20), "pt")
      )
  })
  
  # Render forecast plot
  output$forecast_plot <- renderPlot({
    forecast_df <- forecast_data()
    ggplot() +
      geom_line(data = forecast_df, aes(x = Time, y = Forecast, color = "Forecast"), size = 1) +
      geom_ribbon(data = forecast_df, aes(x = Time, ymin = Lower95, ymax = Upper95, fill = "95% CI"), alpha = 0.2) +
      geom_ribbon(data = forecast_df, aes(x = Time, ymin = Lower80, ymax = Upper80, fill = "80% CI"), alpha = 0.3) +
      scale_color_manual(values = c("Forecast" = "red")) +
      scale_fill_manual(values = c("95% CI" = "gray70", "80% CI" = "gray50")) +
      labs(
        title = "TeleCom Stock Price Forecast",
        subtitle = "Historical Data (2009-2023) and Forecast",
        x = "Year",
        y = "Price",
        color = "Data Type",
        fill = "Confidence Interval"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Render forecast summary table
  output$forecast_summary <- renderTable({
    forecast_df <- forecast_data()
    prediction_summary <- data.frame(
      Mean_Price = mean(forecast_df$Forecast),
      Min_Price = min(forecast_df$Forecast),
      Max_Price = max(forecast_df$Forecast)
    )
    prediction_summary
  })
  
  # Download button for forecast data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("forecast_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(forecast_data(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)