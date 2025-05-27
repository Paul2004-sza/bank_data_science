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

library(caret)
library(dplyr)
library(e1071)
library(xgboost)
library(pROC)
library(car)

set.seed(123)

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

# Upsample train data to balance classes
train_balanced <- upSample(
  x = train %>% select(-responded),
  y = train$responded,
  yname = "responded"
)

# Safely remove columns that exist
cols_to_remove <- c(
  "total_campaigns", "response_rate", "App Notification", "Email", "Phone",
  "SMS", "last_campaign_contacts", "education_level", "occupation", "region"
)

cols_existing <- intersect(cols_to_remove, colnames(train_balanced))
train_clean <- train_balanced %>% select(-all_of(cols_existing))

# Define numeric columns for scaling (check they exist)
numeric_cols <- intersect(c("age", "income", "total_responses"), colnames(train_clean))

# Functions to scale numeric columns consistently
scale_numeric_train <- function(df, numeric_cols) {
  centers <- sds <- numeric(length(numeric_cols))
  names(centers) <- names(sds) <- numeric_cols
  
  df_scaled <- df
  
  for (col in numeric_cols) {
    centers[col] <- mean(df[[col]], na.rm = TRUE)
    sds[col] <- sd(df[[col]], na.rm = TRUE)
    df_scaled[[col]] <- (df[[col]] - centers[col]) / sds[col]
  }
  
  list(scaled_df = df_scaled, centers = centers, sds = sds)
}

scale_numeric_test <- function(df, numeric_cols, centers, sds) {
  df_scaled <- df
  for (col in numeric_cols) {
    df_scaled[[col]] <- (df[[col]] - centers[col]) / sds[col]
  }
  df_scaled
}

# Scale train_clean numeric columns 
scaled_train_res <- scale_numeric_train(train_clean, numeric_cols)
train_clean <- scaled_train_res$scaled_df
train_centers <- scaled_train_res$centers
train_sds <- scaled_train_res$sds

# Prepare test_clean with same columns as train_clean 
test_clean <- test %>% select(colnames(train_clean))

# Scale test_clean numeric columns using train params
test_clean <- scale_numeric_test(test_clean, numeric_cols, train_centers, train_sds)

# Ensure factors in test_clean have same levels as train_clean
factor_cols <- names(which(sapply(train_clean, is.factor)))

for (col in factor_cols) {
  test_clean[[col]] <- factor(test_clean[[col]], levels = levels(train_clean[[col]]))
}

# Set responded factor levels consistently
train_clean$responded <- factor(train_clean$responded, levels = c("0", "1"))
test_clean$responded <- factor(test_clean$responded, levels = c("0", "1"))

#Fit logistic regression on cleaned train data
logit_model_clean <- glm(responded ~ ., data = train_clean, family = "binomial")

print(vif(logit_model_clean))  # Check multicollinearity

# Train Naive Bayes on the same cleaned train_clean dataset 
nb_model <- naiveBayes(responded ~ ., data = train_clean)

# Prepare matrices for XGBoost (use train_clean/test_clean) 
xgb_train <- model.matrix(responded ~ . -1, data = train_clean)
xgb_label <- as.numeric(as.character(train_clean$responded))

xgb_test <- model.matrix(responded ~ . -1, data = test_clean)

xgb_model <- xgboost(
  data = xgb_train,
  label = xgb_label,
  nrounds = 100,
  objective = "binary:logistic",
  verbose = 0
)

# Evaluation function 
evaluate_model <- function(model, test_data, model_type) {
  if (model_type == "logit") {
    pred <- predict(model, newdata = test_data, type = "response")
  } else if (model_type == "nb") {
    pred <- predict(model, newdata = test_data, type = "raw")[, "1"]
  } else if (model_type == "xgb") {
    test_matrix <- model.matrix(responded ~ . -1, data = test_data)
    pred <- predict(model, test_matrix)
  } else {
    stop("Unknown model type")
  }
  
  true <- as.numeric(as.character(test_data$responded))
  
  if (length(unique(true)) < 2) {
    warning("Test set has only one class; ROC AUC not calculated.")
    auc_value <- NA
  } else {
    roc_obj <- pROC::roc(true, pred)
    auc_value <- pROC::auc(roc_obj)
  }
  
  acc <- mean((pred > 0.5) == true)
  
  data.frame(
    Model = toupper(model_type),
    AUC = round(auc_value, 3),
    Accuracy = round(acc, 3)
  )
}

# Evaluate all models
results <- dplyr::bind_rows(
  evaluate_model(logit_model_clean, test_clean, "logit"),
  evaluate_model(nb_model, test_clean, "nb"),
  evaluate_model(xgb_model, test_clean, "xgb")
)

print(results)

write.csv(results, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/Model_auc_accuracy.csv", row.names = FALSE)

library(dplyr)
library(tidyr)
library(readr)
library(xgboost)

# Define predictors based on training data (excluding target)
predictors <- names(train)[names(train) != "responded"]

# Prepare prediction dataset by joining feature tables and replacing NA with 0
new_data <- customers %>%
  left_join(response_history, by = "customer_id") %>%
  left_join(channel_counts, by = "customer_id") %>%
  left_join(last_campaign_contacts, by = "customer_id") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  select(customer_id, all_of(predictors))

# Create model matrix for prediction (exclude customer_id)
final_matrix <- model.matrix(~ . -customer_id -1, data = new_data)

# Get feature names stored in the trained xgboost model
model_features <- xgb_model$feature_names

# Add any missing columns in final_matrix (set to zero)
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

# Ensure colnames are set properly (just in case)
colnames(final_matrix) <- model_features

# Create xgb.DMatrix for prediction
dtest <- xgb.DMatrix(data = final_matrix)

# Generate predictions
predicted_prob <- predict(xgb_model, dtest)

# Combine predictions with customer IDs
final_predictions <- new_data %>%
  mutate(predicted_prob = predicted_prob,
         predicted_class = ifelse(predicted_prob > 0.5, 1, 0))

# Print predictions
print(final_predictions)

# Save to CSV
write_csv(final_predictions, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/campaign_predictions.csv")

library(dplyr)
library(ggplot2)
library(readr) 

#join final_predictions with customers data
joined_data <- final_predictions %>%
  left_join(customers, by = "customer_id")

# Filter out rows with missing income.y (income from customers)
# Use income.y explicitly because after join, income columns are suffixed with .x and .y
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

# plot the predicted conversion rates by income bracket
ggplot(income_analysis, aes(x = income_bracket, y = Conversion_Rate)) +
  geom_col(fill = "steelblue") +
  labs(title = "Predicted Conversion Rates by Income Bracket",
       x = "Income Bracket", y = "Conversion Rate") +
  theme_minimal()
print(income_analysis)
write_csv(income_analysis, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/income_conversion_rate.csv")

library(pROC)

# Check the distribution of the response variable in test data
print(table(test_clean$responded))

# Convert response to a factor with levels matching actual data type
# Assuming test_clean$responded is numeric 0/1; adjust if character
true_labels <- factor(test_clean$responded, levels = c(0, 1))

# Double-check levels and counts
print(levels(true_labels))
print(table(true_labels))

# Make sure there are both classes present
if(length(unique(true_labels)) < 2) {
  stop("Error: test set does not contain both classes (0 and 1). Cannot compute ROC.")
}

# Logistic Regression predicted probabilities
logit_probs <- predict(logit_model_clean, newdata = test_clean, type = "response")

# Naive Bayes predicted probabilities for class "1"
nb_probs <- predict(nb_model, newdata = test_clean, type = "raw")[, "1"]

# Prepare test data matrix for XGBoost (exclude response)
test_matrix <- model.matrix(responded ~ . -1, data = test_clean)

# XGBoost predicted probabilities
xgb_probs <- predict(xgb_model, test_matrix)

# Create ROC objects
roc_logit <- roc(true_labels, logit_probs)
roc_nb <- roc(true_labels, nb_probs)
roc_xgb <- roc(true_labels, xgb_probs)

# Plot ROC curves
plot(roc_logit, col = "blue", main = "ROC Curves for Models", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate")
plot(roc_nb, add = TRUE, col = "green", lwd = 2)
plot(roc_xgb, add = TRUE, col = "red", lwd = 2)

# Add legend with AUC values
legend("bottomright",
       legend = c(
         paste("Logistic Regression (AUC =", round(auc(roc_logit), 3), ")"),
         paste("Naive Bayes (AUC =", round(auc(roc_nb), 3), ")"),
         paste("XGBoost (AUC =", round(auc(roc_xgb), 3), ")")
       ),
       col = c("blue", "green", "red"),
       lty = 1,
       lwd = 2,
       cex = 0.8)

# Print AUC values to console
cat("Logistic Regression AUC:", round(auc(roc_logit), 4), "\n")
cat("Naive Bayes AUC:", round(auc(roc_nb), 4), "\n")
cat("XGBoost AUC:", round(auc(roc_xgb), 4), "\n")
