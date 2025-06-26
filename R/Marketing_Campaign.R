library(tidyverse)
library(caret)
library(e1071)
library(xgboost)
library(plotly)
library(lubridate)
library(heatmaply)
library(pROC)
library(dplyr)
library(car)  
library(Matrix)
library(ggplot2)

set.seed(123)

customers <- read.csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/b_customers.csv")
campaigns <- read.csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/b_marketing_campaigns.csv")
responses <- read_csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_campaign_responses.csv")
costs <- read.csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/b_campaign_marketing_with_cost.csv")

campaigns <- campaigns %>%
  mutate(start_date = as.Date(start_date),
         end_date = as.Date(end_date))

responses <- responses %>%
  mutate(response_date = as.Date(response_date))

# Merge responses with campaigns and customers
merged_data <- responses %>%
  left_join(campaigns, by = "campaign_id")%>%
  left_join(customers, by = "customer_id")

# channel usage count per customer
channel_counts <- merged_data %>%
  group_by(customer_id, channel) %>%
  summarise(channel_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = channel, values_from = channel_count, values_fill = 0)

# Response history per customer
response_history <- merged_data %>%
  group_by(customer_id) %>% 
  summarise(total_responses = sum(responded),
            total_campaigns = n(),
            response_rate = total_responses/ total_campaigns, .groups = "drop")

# contacts in the last campaign
last_campaign_id <- campaigns %>% 
  arrange(desc(end_date)) %>% 
  slice(1) %>% 
  pull(campaign_id)

last_campaign_contacts <- responses %>% 
  filter(campaign_id == last_campaign_id) %>% 
  group_by(customer_id) %>% 
  summarise(last_campaign_contacts = n(), .groups = "drop")

#Final feature dataset
final_data <- customers %>% 
  left_join(response_history, by = "customer_id") %>% 
  left_join(channel_counts, by = "customer_id") %>% 
  left_join(last_campaign_contacts, by = "customer_id") %>% 
  mutate(across(where(is.numeric), ~replace_na(.,0)),
         responded = ifelse(total_responses > 0,1,0)) %>% 
  select(-customer_id, -name, -email, -phone, -join_date, -date_of_birth)

# visualization got marketing campaign funnel
funnel_data <- data.frame(
  stage = c("Reached", "Opened", "Converted"),
  count = c(
    nrow(customers),                             # Unique customers contacted
    n_distinct(responses$customer_id),           # Unique customers who opened
    sum(final_data$total_responses > 0)          # Unique customers who responded
  )
)

# Calculate conversion percentage from "Reached"
funnel_data$percent <- round(100 * funnel_data$count / funnel_data$count[1], 1)

# Combine stage, count, and percent into label
funnel_data$label <- paste0(
  funnel_data$stage, "\n",
  funnel_data$count, " (", funnel_data$percent, "%)"
)

# Funnel plot
plot_ly(
  data = funnel_data,
  type = "funnelarea",
  text = ~label,              #combined label
  values = ~count,
  marker = list(
    colors = c("#0073C2", "#FFC300", "#28B463"),
    line = list(color = "#FFFFFF", width = 2)
  )
) %>% layout(
  title = list(text = "Marketing Campaign Funnel", x = 0.5, font = list(size = 20)),
  showlegend = FALSE,
  margin = list(l = 50, r = 50, t = 80, b = 50)
)

# heatmap by channel and age group
heatmap_data <- merged_data %>% 
  mutate(age_group = cut(age, breaks = seq(20, 80, by = 10))) %>% 
  group_by(age_group, channel) %>% 
  summarise(response_rate = mean(responded), .groups = "drop") %>% 
  pivot_wider(names_from = channel, values_from = response_rate, values_fill = 0)

heatmaply(as.matrix(heatmap_data[,-1]),
          row_side_colors = heatmap_data$age_group,
          xlab = "Channel", ylab = "Age Group",
          main = "Response Rate Heatmap by Channel and Age Group")

# Manual stratified split
data_0 <- final_data[final_data$responded == "0", ]
data_1 <- final_data[final_data$responded == "1", ]

idx_0 <- sample(nrow(data_0), size = floor(0.8 * nrow(data_0)))
idx_1 <- sample(nrow(data_1), size = floor(0.8 * nrow(data_1)))

train <- rbind(data_0[idx_0, ], data_1[idx_1, ])
test <- rbind(data_0[-idx_0, ], data_1[-idx_1, ])

train <- train[sample(nrow(train)), ]
test <- test[sample(nrow(test)), ]

cat("Train label distribution:\n")
print(table(train$responded))
cat("Test label distribution:\n")
print(table(test$responded))

# Define predictors based on training data (excluding target)
predictors <- names(train)[names(train) != "responded"]

# Prepare prediction dataset
new_data <- customers %>%
  left_join(response_history, by = "customer_id") %>%
  left_join(channel_counts, by = "customer_id") %>%
  left_join(last_campaign_contacts, by = "customer_id") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  select(customer_id, all_of(predictors))

# Create model matrix for prediction
final_matrix <- model.matrix(~ . -customer_id -1, data = new_data)

# Get feature names stored in the trained xgboost model
model_features <- xgb_model$feature_names

# Add any missing columns in final_matrix
missing_cols <- setdiff(model_features, colnames(final_matrix))
if(length(missing_cols) > 0) {
  for (col in missing_cols) {
    final_matrix <- cbind(final_matrix, 0)
    colnames(final_matrix)[ncol(final_matrix)] <- col
  }
}

# Remove extra columns not in model_features
extra_cols <- setdiff(colnames(final_matrix), model_features)
if(length(extra_cols) > 0) {
  final_matrix <- final_matrix[, !(colnames(final_matrix) %in% extra_cols), drop = FALSE]
}

# Reorder columns to match model_features exactly
final_matrix <- final_matrix[, model_features, drop = FALSE]

# Ensure colnames are set properly
colnames(final_matrix) <- model_features

# Create xgb.DMatrix and predict
dtest <- xgb.DMatrix(data = final_matrix)
predicted_prob <- predict(xgb_model, dtest)

# Combine predictions with customer IDs
final_predictions <- new_data %>%
  mutate(predicted_prob = predicted_prob,
         predicted_class = ifelse(predicted_prob > 0.5, 1, 0))


print(final_predictions)
write_csv(final_predictions, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/campaign/campaign_predictions.csv")

# Analysis by income bracket
joined_data <- final_predictions %>%
  left_join(customers, by = "customer_id")

income_analysis <- joined_data %>%
  filter(!is.na(income.y)) %>% 
  mutate(income_bracket = cut(income.y, 
                              breaks = quantile(income.y, probs = seq(0, 1, 0.2), na.rm = TRUE),
                              labels = c("Low", "Medium-Low", "Medium", "Medium-High", "High"),
                              include.lowest = TRUE)) %>%
  group_by(income_bracket) %>%
  summarise(Conversion_Rate = mean(predicted_prob, na.rm = TRUE), .groups = "drop")

dev.new()
dev.off()

ggplot(income_analysis, aes(x = income_bracket, y = Conversion_Rate)) +
  geom_col(fill = "steelblue") +
  labs(title = "Predicted Conversion Rates by Income Bracket",
       x = "Income Bracket", y = "Conversion Rate") +
  theme_minimal()

print(income_analysis)
write_csv(income_analysis, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/campaign/income_conversion_rate.csv")

library(dplyr)
library(ggplot2)

#Merge transactions with accounts to get customer_id
transactions_full <- merge(
  transactions,
  accounts[, c("account_id", "customer_id")],
  by = "account_id"
)

responses_yes <- responses[responses$responded == TRUE, ]

#Merge responses with transactions using customer_id
merged_data <- merge(responses_yes, transactions_full, by = "customer_id")
head(merged_data)

# Revenue statistics per customer, then average
revenue_stats <- merged_data %>%
  group_by(customer_id) %>%
  summarise(total_spent = sum(amount)) %>%
  summarise(average_revenue_per_response = mean(total_spent))

print(revenue_stats)

# Calculate response count per campaign
response_summary <- responses %>%
  filter(responded == TRUE) %>%
  group_by(campaign_id) %>%
  summarise(responders = n())

# Merge campaign cost with response count
roi_df <- merge(response_summary, costs, by = "campaign_id")

# Add revenue per response & calculate ROI
#hard-coded value
revenue_per_response <- 358386752

roi_df <- roi_df %>%
  mutate(
    total_revenue = responders * revenue_per_response,
    roi = (total_revenue - cost_mmk) / cost_mmk
  )

print(roi_df)
write.csv(roi_df, file = "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/campaign/roi_result.csv", row.names = FALSE )
write.csv(revenue_stats, file = "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/campaign/revenue_stats.csv", row.names = FALSE )

ggplot(roi_df, aes(x = reorder(campaign_name, -roi), y = roi, fill = channel)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "ROI per Marketing Campaign",
    x = "Campaign",
    y = "ROI (Return on Investment)"
  ) +
  theme_minimal()
library(dplyr)
library(ggplot2)

#  Personalized offers summary by customer and channel
personalized_offers <- merged_data %>%
  group_by(customer_id, channel) %>%
  summarise(
    total_spent = sum(amount),
    transactions = n(),
    last_location = last(location),
    .groups = "drop"
  )

ggplot(personalized_offers, aes(x = reorder(channel, -total_spent), y = total_spent, fill = channel)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Total Spent by Channel", x = "Channel", y = "Total Spent") +
  theme_minimal()

# Assign A/B testing groups if not already in `responses`
set.seed(123)  # For reproducibility
responses$group <- ifelse(runif(nrow(responses)) > 0.5, "A", "B")

# Calculate response stats by A/B group
ab_test <- responses %>%
  group_by(group) %>%
  summarise(
    total = n(),
    responded = sum(responded == TRUE),
    response_rate = responded / total,
    .groups = "drop"
  )

print(ab_test)
write.csv(ab_test, file = "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/campaign/response_rate_AB_test_g.csv", row.names = FALSE )
ggplot(ab_test, aes(x = group, y = response_rate, fill = group)) +
  geom_col() +
  geom_text(aes(label = scales::percent(response_rate, accuracy = 0.1)), vjust = -0.5) +
  labs(title = "Response Rate by A/B Test Group", x = "Group", y = "Response Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
