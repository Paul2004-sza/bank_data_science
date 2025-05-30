library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(pROC)
library(e1071)
library(xgboost)
library(Matrix)
library(readr)

customers <- read.csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/b_customers.csv")
campaigns <- read.csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/b_marketing_campaigns.csv")
responses <- read_csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_campaign_responses.csv")

# Merge datasets
data <- responses %>%
  left_join(customers, by = "customer_id") %>%
  left_join(campaigns, by = "campaign_id")

# Clean and engineer features
data_clean <- data %>%
  mutate(
    responded = as.factor(responded),
    gender = as.factor(gender),
    marital_status = as.factor(marital_status),
    education_level = as.factor(education_level),
    occupation = as.factor(occupation),
    region = as.factor(region)
  ) %>%
  select(responded, age, income, gender, marital_status, education_level, occupation, region) %>%
  na.omit()

# Train-test split
set.seed(123)
train_index <- createDataPartition(data_clean$responded, p = 0.7, list = FALSE)
train <- data_clean[train_index, ]
test <- data_clean[-train_index, ]


#Combine ROC Curves 
log_roc <- roc(test$responded, log_pred_prob)
nb_roc <- roc(test$responded, nb_pred_prob)
xgb_roc <- roc(test$responded, xgb_pred_prob)

# Plot all ROC curves
plot(log_roc, col = "blue", main = "ROC Curves - All Models", lwd = 2)
plot(nb_roc, col = "green", add = TRUE, lwd = 2)
plot(xgb_roc, col = "red", add = TRUE, lwd = 2)

legend_labels <- paste0(
  results$Model, "\n",
  "Sens: ", round(results$Sensitivity, 3), ", ",
  "Spec: ", round(results$Specificity, 3)
)

legend("bottomright", legend = legend_labels,
       col = c("blue", "green", "red"), lwd = 2, bty = "n", cex = 0.8)

#Collect Performance Metrics
results <- data.frame(
  Model = c("Logistic Regression", "Naive Bayes", "XGBoost"),
  Accuracy = c(log_cm$overall["Accuracy"], nb_cm$overall["Accuracy"], xgb_cm$overall["Accuracy"]),
  AUC = c(auc(log_roc), auc(nb_roc), auc(xgb_roc)),
  Sensitivity = c(log_cm$byClass["Sensitivity"], nb_cm$byClass["Sensitivity"], xgb_cm$byClass["Sensitivity"]),
  Specificity = c(log_cm$byClass["Specificity"], nb_cm$byClass["Specificity"], xgb_cm$byClass["Specificity"])
)

write.csv(results, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/model_comparison_results.csv", row.names = FALSE)

cat("Model Performance Summary:\n")
print(results)
