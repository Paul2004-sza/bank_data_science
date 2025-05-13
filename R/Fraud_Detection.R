library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(leaflet)
library(caret)
library(solitude)
library(h2o)       
library(geosphere) 

transactions <- read.csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_transactions_cleaned.csv", stringsAsFactors = FALSE)
customer_patterns <- read.csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/processed/Transaction/customer_usual_patterns.csv", stringsAsFactors = FALSE)

transactions$transaction_date <- parse_date_time(transactions$transaction_date, orders = c("mdY HMS", "mdY HM", "Ymd HMS"))

str(transactions$transaction_date)

# Extract time-based features
transactions$hour <- lubridate::hour(transactions$transaction_date)
transactions$day_of_week <- wday(transactions$transaction_date, label = TRUE)
transactions$is_weekend <- ifelse(transactions$day_of_week %in% c("Sat", "Sun"), 1, 0)

# Merge with customer usual patterns
data <- transactions %>%
  left_join(customer_patterns, by = "account_id")

# Calculate time since last transaction (fraud-related signal)
data <- data %>%
  arrange(account_id, transaction_date) %>%
  group_by(account_id) %>%
  mutate(time_since_last = as.numeric(difftime(transaction_date, lag(transaction_date), units = "hours"))) %>%
  ungroup()

head(data)

# Prepare data for isolation forest
iso_data <- data %>%
  select(amount, hour, is_weekend) %>%
  mutate(amount = log(amount + 1))  # Log transform for better performance

# Train isolation forest
iso_model <- isolationForest$new()
iso_model$fit(iso_data)

# Get anomaly scores
anomaly_scores <- iso_model$predict(iso_data)
data$iso_anomaly_score <- anomaly_scores$anomaly_score

# Flag anomalies (top 1%)
data$iso_anomaly_flag <- ifelse(data$iso_anomaly_score > quantile(data$iso_anomaly_score, 0.99), 1, 0)

h2o.init()

#Select columns
ae_data <- data %>%
  select(amount, hour, is_weekend)

#Log-transform amount (if not already done)
ae_data$amount <- log(ae_data$amount + 1)

#Scale manually and ensure it's a data frame
ae_data_scaled <- as.data.frame(scale(ae_data))

#Convert to H2O frame
ae_data_h2o <- as.h2o(ae_data_scaled)


#Train autoencoder
autoencoder <- h2o.deeplearning(
  x = names(ae_data),
  training_frame = ae_data_h2o,
  autoencoder = TRUE,
  hidden = c(10, 2, 10),  # Bottleneck layer with 2 neurons
  epochs = 50,
  activation = "Tanh"
)

# Get reconstruction error
reconstruction <- h2o.anomaly(autoencoder, ae_data_h2o)
data$ae_reconstruction_error <- as.vector(reconstruction)

# Flag anomalies (top 1%)
data$ae_anomaly_flag <- ifelse(data$ae_reconstruction_error > quantile(data$ae_reconstruction_error, 0.99), 1, 0)

#Large amount compared to customer's usual pattern
data$amount_anomaly <- ifelse(
  data$amount > (data$avg_amount + 3 * data$std_amount), 1, 0)

#Unusual transaction type
data$type_anomaly <- ifelse(
  data$type != data$most_common_type, 1, 0)

#Unusual channel
data$channel_anomaly <- ifelse(
  data$channel != data$preferred_channel, 1, 0)

#Unusual time (assuming usual_hour is a single hour)
data$time_anomaly <- ifelse(
  abs(data$hour - data$usual_hour) > 4, 1, 0)  # More than 4 hours from usual

#Unusual location (simplified)
data$location_anomaly <- ifelse(
  data$location != data$top_location, 1, 0)

# Combine all anomaly flags
data$combined_anomaly_score <- (
  data$iso_anomaly_flag + data$ae_anomaly_flag + 
    data$amount_anomaly + data$type_anomaly + 
    data$channel_anomaly + data$time_anomaly + 
    data$location_anomaly
)

# Final fraud flag (threshold can be adjusted)
data$fraud_flag <- ifelse(data$combined_anomaly_score >= 3, 1, 0)
# Assuming we have a 'is_fraud' column in our data
if("is_fraud" %in% colnames(data)) {

  set.seed(42)
  train_index <- createDataPartition(data$is_fraud, p = 0.8, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]

  features <- c("amount", "hour", "is_weekend", "type_anomaly", 
                "channel_anomaly", "time_anomaly", "location_anomaly")
  
  # Train Random Forest
  rf_model <- train(
    x = train_data[, features],
    y = as.factor(train_data$is_fraud),
    method = "rf",
    trControl = trainControl(method = "cv", number = 5),
    importance = TRUE
  )
  
  test_data$rf_pred <- predict(rf_model, newdata = test_data[, features])
  test_data$rf_prob <- predict(rf_model, newdata = test_data[, features], type = "prob")[,2]
  
  confusionMatrix(test_data$rf_pred, as.factor(test_data$is_fraud))
  
  varImpPlot(rf_model$finalModel)
}
# Ensure location column is character and handle NAs
data$location <- as.character(data$location)
data$location[is.na(data$location)] <- "Unknown"

# Prepare location frequency data
location_freq <- data %>%
  count(location, sort = TRUE) %>%
  slice_max(order_by = n, n = 20)  # Use slice_max instead of top_n (which is deprecated)

# Plot heatmap-style bar chart
ggplot(location_freq, aes(x = reorder(location, n), y = n, fill = n)) +
  geom_col() +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_flip() +
  labs(title = "Transaction Frequency by Location",
       x = "Location",
       y = "Number of Transactions") +
  theme_minimal()

# Ensure transaction_date and amount are in correct format
data$transaction_date <- as.Date(data$transaction_date)
data$amount <- as.numeric(data$amount)

# Aggregate by day
daily_trans <- data %>%
  mutate(date = as.Date(transaction_date)) %>%
  group_by(date) %>%
  summarise(
    total_amount = sum(amount, na.rm = TRUE),
    avg_amount = mean(amount, na.rm = TRUE),
    fraud_count = sum(fraud_flag, na.rm = TRUE)
  )

# Time series plot
plot_ly(daily_trans, x = ~date) %>%
  add_lines(y = ~total_amount, name = "Total Amount") %>%
  add_markers(
    y = ~total_amount,
    marker = list(color = ifelse(daily_trans$fraud_count > 0, "red", "green"),
                  size = ~fraud_count * 5),
    name = "Fraud Count",
    hoverinfo = "text",
    text = ~paste("Date:", date,
                  "<br>Total Amount:", round(total_amount, 2),
                  "<br>Fraud Cases:", fraud_count)
  ) %>%
  layout(
    title = "Daily Transaction Amount with Fraud Cases Highlighted",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Total Amount")
  )

# Trim spaces from location names in both datasets
fraud_transactions$location <- stringr::str_trim(fraud_transactions$location)
city_coords$location <- stringr::str_trim(city_coords$location)

# Join the fraud_transactions with the city_coords to get latitude and longitude
fraud_transactions <- fraud_transactions %>%
  left_join(city_coords, by = "location")

# Filter out rows where lat or lng are NA or outside Myanmar's boundaries
fraud_transactions <- fraud_transactions %>%
  filter(!is.na(lat) & !is.na(lng)) %>%
  filter(lat >= 10 & lat <= 28, lng >= 92 & lng <= 101)

# Check for any remaining fraud transactions with invalid coordinates
fraud_transactions_outside_myanmar <- fraud_transactions %>%
  filter(lat < 10 | lat > 28 | lng < 92 | lng > 101)

# Print out transactions outside Myanmar for debugging
if(nrow(fraud_transactions_outside_myanmar) > 0) {
  print(fraud_transactions_outside_myanmar)
} else {
  print("All data points are within Myanmar's boundaries.")
}

# Combine fraud times and amounts to create a "fraud intensity" score
fraud_transactions <- fraud_transactions %>%
  mutate(fraud_intensity = amount * combined_anomaly_score)

# Create a color scale based on the "fraud_intensity"
fraud_color <- colorNumeric(
  palette = "YlOrRd",  # Color palette (yellow to red)
  domain = fraud_transactions$fraud_intensity  # Scale based on fraud intensity
)

# Now plot the valid fraud transactions on the map with dynamic colors
leaflet(fraud_transactions) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    radius = ~log(amount + 1) * 2,  # Scale radius based on amount
    color = ~fraud_color(fraud_intensity),  # Color based on fraud intensity
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>Amount:</b> $", formatC(amount, format = "f", digits = 2),
      "<br><b>Type:</b> ", type,
      "<br><b>Date:</b> ", as.character(transaction_date),
      "<br><b>Location:</b> ", location,
      "<br><b>Fraud Intensity:</b> ", round(fraud_intensity, 2)
    )
  ) %>%
  addLegend("bottomright", 
            pal = fraud_color,  # Use the fraud color scale
            values = ~fraud_intensity,
            title = "Fraud Intensity",
            labFormat = labelFormat(prefix = "$"))

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Fraud Detection Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Anomaly Detection", tabName = "anomaly", icon = icon("exclamation-triangle")),
      menuItem("Transaction Map", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_transactions"),
                valueBoxOutput("fraud_cases"),
                valueBoxOutput("fraud_rate")
              ),
              fluidRow(
                box(plotlyOutput("daily_plot"), width = 12)
              )
      ),
      tabItem(tabName = "anomaly",
              fluidRow(
                box(plotOutput("heatmap"), width = 6),
                box(plotOutput("anomaly_plot"), width = 6)
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(leafletOutput("fraud_map"), width = 12)
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$total_transactions <- renderValueBox({
    valueBox(nrow(data), "Total Transactions", icon = icon("credit-card"))
  })
  
  output$fraud_cases <- renderValueBox({
    valueBox(sum(data$fraud_flag), "Potential Fraud Cases", 
             icon = icon("exclamation"), color = "red")
  })
  
  output$fraud_rate <- renderValueBox({
    rate <- round(mean(data$fraud_flag) * 100, 2)
    valueBox(paste0(rate, "%"), "Fraud Rate", 
             icon = icon("percent"), color = "yellow")
  })
  
  output$daily_plot <- renderPlotly({
    plot_ly(daily_trans) %>%
      add_lines(x = ~date, y = ~total_amount) %>%
      add_markers(x = ~date, y = ~total_amount, 
                  color = ~fraud_count, 
                  size = ~fraud_count * 5)
  })
  
  output$heatmap <- renderPlot({
    ggplot(location_freq, aes(x = reorder(location, n), y = n, fill = n)) +
      geom_col() +
      coord_flip()
  })
  
  output$fraud_map <- renderLeaflet({
    leaflet(fraud_transactions) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = ~log(amount) * 2,
        color = "red",
        fillOpacity = 0.7
      )
  })
}

shinyApp(ui, server)

write.csv(data, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/processed_transactions_with_flags.csv", row.names = FALSE)

high_risk <- data %>% filter(fraud_flag == 1)
write.csv(high_risk, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/high_risk_transactions.csv", row.names = FALSE)

# daily summary
daily_summary <- data %>%
  mutate(date = as.Date(transaction_date)) %>%
  group_by(date) %>%
  summarise(
    total_transactions = n(),
    total_amount = sum(amount),
    fraud_cases = sum(fraud_flag),
    fraud_amount = sum(amount[fraud_flag == 1])
  )

write.csv(daily_summary, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/daily_fraud_summary.csv", row.names = FALSE)

customer_risk <- data %>%
  group_by(account_id) %>%
  summarise(
    total_transactions = n(),
    fraud_count = sum(fraud_flag),
    fraud_ratio = mean(fraud_flag),
    total_amount = sum(amount),
    fraud_amount = sum(amount[fraud_flag == 1])) %>%
  arrange(desc(fraud_ratio))

write.csv(customer_risk, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/customer_risk_profiles.csv", row.names = FALSE)

# Ensure fraud flag is set as supervised label
data$is_fraud <- data$fraud_flag

if (exists("test_data")) {
  required_cols <- c("rf_pred", "rf_prob")
  missing <- setdiff(required_cols, colnames(test_data))
  
  if (length(missing) == 0) {
    model_output <- test_data %>%
      select(transaction_id, account_id, amount, is_fraud, rf_pred, rf_prob)
    
    write.csv(model_output, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/model_predictions.csv", row.names = FALSE)
    print("Model_predictions.csv successfully saved.")
  } else {
    warning(paste("test_data exists but missing columns:", paste(missing, collapse = ", ")))
  }
}

# For anomaly detection models
anomaly_scores <- data %>%
  select(transaction_id, account_id, amount, 
         iso_anomaly_score, ae_reconstruction_error, 
         combined_anomaly_score, fraud_flag)

write.csv(anomaly_scores, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/anomaly_scores.csv", row.names = FALSE)

if(exists("test_data")) {
  performance_analysis <- test_data %>%
    mutate(
      classification = case_when(
        is_fraud == 1 & rf_pred == 1 ~ "True Positive",
        is_fraud == 0 & rf_pred == 1 ~ "False Positive",
        is_fraud == 1 & rf_pred == 0 ~ "False Negative",
        TRUE ~ "True Negative"
      )
    ) %>%
    filter(classification %in% c("False Positive", "False Negative"))
  
  write.csv(performance_analysis, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/model_errors_analysis.csv", row.names = FALSE)
}
location_risk <- data %>%
  group_by(location) %>%
  summarise(
    transaction_count = n(),
    fraud_count = sum(fraud_flag),
    fraud_rate = mean(fraud_flag),
    avg_amount = mean(amount)) %>%
  filter(transaction_count > 10) %>%  # Only locations with sufficient data
  arrange(desc(fraud_rate))

write.csv(location_risk, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/location_risk_analysis.csv", row.names = FALSE)

output_dir <- file.path("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data", paste0("output_", Sys.Date()))
if(!dir.exists(output_dir)) dir.create(output_dir)

write.csv(data, file.path(output_dir, paste0("processed_", Sys.Date(), ".csv")), row.names = FALSE)

write.csv(data, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/output_2025-05-10/processed_data.csv.gz", row.names = FALSE)

saveRDS(iso_model, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/isolation_forest_model.rds")

log_data <- data.frame(
  timestamp = Sys.time(),
  records_processed = nrow(data),
  fraud_cases_detected = sum(data$fraud_flag)
)
suppressWarnings(
  write.table(log_data, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/processing_log.csv", 
              sep = ",", 
              col.names = !file.exists("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/processing_log.csv"), 
              append = TRUE, 
              row.names = FALSE)
)

h2o.shutdown(prompt = FALSE)