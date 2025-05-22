library(tidyverse)
library(lubridate)
library(caret)
library(xgboost)
library(randomForest)
library(rpart)
library(rpart.plot)
library(pROC)
library(SHAPforxgboost)
library(data.table)
library(ggplot2)
library(broom)
library(caret)
library(car)

loan <- read_csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_loans.csv")
customers <- read_csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/b_customers.csv")
credit <- read_csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_credit_scores.csv")

latest_credit <- credit %>%
  group_by(customer_id) %>%
  filter(date_recorded == max(date_recorded)) %>%
  select(customer_id, score, risk_level)

# Merge all datasets
df <- loan %>%
  left_join(customers, by = "customer_id") %>%
  left_join(latest_credit, by = "customer_id") %>%
  filter(!is.na(score))

# Convert loan status to binary target (1 = default, 0 = repaid)
df <- df %>%
  mutate(default = ifelse(status == "Defaulted", 1, 0))

# Convert categorical to factor
df <- df %>%
  mutate(
    gender = as.factor(gender),
    marital_status = as.factor(marital_status),
    education_level = as.factor(education_level),
    occupation = as.factor(occupation),
    loan_type = as.factor(loan_type),
    term_months = as.numeric(term_months)
  )
df_model <- df %>%
  select(default, loan_amount, interest_rate, term_months, income, age, score, risk_level)

# Convert risk level to numeric ordinal
df_model$risk_level <- factor(df_model$risk_level, levels = c("Low", "Medium", "High"), ordered = TRUE)

# Remove NAs
df_model <- na.omit(df_model)

# Split into training and test set
set.seed(123)
n <- nrow(df_model)
train_indices <- sample(1:n, size = 0.8 * n)
train <- df_model[train_indices, ]
test <- df_model[-train_indices, ]
table(df_model$default)
table(df$default)


log_model <- glm(default ~ ., data = train, family = binomial)
summary(log_model)

# Tidy the model for visualization with confidence intervals
coef_df <- tidy(log_model, conf.int = TRUE)

# Plot Coefficients with 95% Confidence Intervals
ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(color = "#0072B2", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "darkgray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Logistic Regression Coefficients (95% CI)",
       x = "Predictor",
       y = "Estimate") +
  theme_minimal(base_size = 14)

# check multicollinearity
cat("Variance Inflation Factors:\n")
vif_values <- vif(log_model)
print(vif_values)

# Predict probabilities on the test set
prob <- predict(log_model, newdata = test, type = "response")

# ROC Curve and AUC
roc_obj <- roc(test$default, prob)

# OptionalSave ROC plot as PNG
png("roc_curve.png", width = 800, height = 600)
plot(roc_obj, col = "darkorange", lwd = 3, main = "ROC Curve")
abline(a = 0, b = 1, col = "gray", lty = 2)
dev.off()

# Display AUC
auc_score <- auc(roc_obj)
cat("AUC Score:", auc_score, "\n")

# Confusion Matrix (threshold = 0.5)
true_class <- factor(test$default, levels = c(0, 1))
pred_class <- factor(ifelse(prob > 0.5, 1, 0), levels = c(0, 1))
confusion <- confusionMatrix(pred_class, true_class)
print(confusion)

# Odds Ratios with 95% CI
coef_df <- coef_df %>%
  mutate(
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  )

# Plot Odds Ratios
ggplot(coef_df, aes(x = reorder(term, OR), y = OR)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_errorbar(aes(ymin = OR_low, ymax = OR_high), width = 0.2, color = "gray40") +
  coord_flip() +
  scale_y_log10() +  # Optionallog scale for odds ratios
  labs(title = "Odds Ratios with 95% CI",
       x = "Predictor",
       y = "Odds Ratio (log scale)") +
  theme_minimal(base_size = 14)

try(dev.off(), silent = TRUE)

tree_model <- rpart(default ~ ., data = train, method = "class", 
                    control = rpart.control(cp = 0.01, minsplit = 10))

rpart.plot(tree_model,
           type = 2,            # split labels below nodes
           extra = 104,         # probability of second class (default=1) + percentage + class
           fallen.leaves = TRUE,
           box.palette = "RdYlGn",
           shadow.col = "gray",
           branch.lty = 3,
           roundint = FALSE,
           main = "Decision Tree for Loan Default Prediction")


# Train Random Forest model
rf_model <- randomForest(as.factor(default) ~ ., 
                         data = train, 
                         importance = TRUE, 
                         ntree = 500,
                         mtry = 3,
                         nodesize = 5,
                         maxnodes = 30)

print(rf_model)

# Extract and sort importance
importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  Importance = importance(rf_model, type = 1)[, 1]
)
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

# variable importance plot
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(title = "Variable Importance - Random Forest (MeanDecreaseAccuracy)",
       x = "Variables",
       y = "Importance Score") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

# Prepare data matrices for xgboost (with dummy encoding)
train_x <- model.matrix(default ~ . -1, data = train)
test_x <- model.matrix(default ~ . -1, data = test)
train_y <- train$default
test_y <- test$default

# Create xgb.DMatrix objects
train_matrix <- xgb.DMatrix(data = train_x, label = train_y)
test_matrix <- xgb.DMatrix(data = test_x, label = test_y)

# Train model with set seed for reproducibility
set.seed(123)
xgb_model <- xgboost(
  data = train_matrix,
  nrounds = 100,
  objective = "binary:logistic",
  eval_metric = "auc",
  verbose = 0
)

#Predict probabilities and calculate ROC + AUC
xgb_pred_prob <- predict(xgb_model, test_matrix)
xgb_roc <- roc(test_y, xgb_pred_prob)
auc_xgb <- auc(xgb_roc)
cat("XGBoost AUC:", round(auc_xgb, 4), "\n")

# Plot ROC curve with clean margins and device reset
while(dev.cur() > 1) dev.off() 
par(mar = c(4, 4, 2, 2))        
plot(xgb_roc, col = "darkorange", lwd = 3, main = "XGBoost ROC Curve")
abline(a = 0, b = 1, col = "gray", lty = 2)

# Classification using 0.5 threshold, confusion matrix
xgb_pred_class <- factor(ifelse(xgb_pred_prob > 0.5, 1, 0), levels = c(0, 1))
true_class <- factor(test_y, levels = c(0, 1))
confusion_xgb <- confusionMatrix(xgb_pred_class, true_class)
print(confusion_xgb)

#Feature importance plot
print(head(importance_matrix))
print(nrow(importance_matrix))

num_features <- min(10, nrow(importance_matrix))

xgb.plot.importance(importance_matrix[1:num_features, ], measure = "Gain",
                    rel_to_first = TRUE,
                    xlab = "Relative Importance",
                    main = "Top Important Features - XGBoost")



#summary and check first few predictions
print(head(xgb_pred_prob))
print(summary(xgb_pred_prob))

library(dplyr)
library(xgboost)
library(caret)
library(pROC)
library(SHAPforxgboost)
library(rpart.plot)
library(randomForest)
library(readr)

# Logistic regression
log_model <- glm(default ~ ., data = train, family = binomial)
summary(log_model)

# Random Forest model & variable importance plot
rf_model <- randomForest(as.factor(default) ~ ., data = train, importance = TRUE)
varImpPlot(rf_model)
dev.off()

# Prepare data for XGBoost (one-hot encoding)
train_matrix <- model.matrix(default ~ . -1, data = train)
train_label <- train$default

test_matrix <- model.matrix(default ~ . -1, data = test)
test_label <- test$default

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

# Train XGBoost
xgb_model <- xgboost(data = dtrain,
                     nrounds = 100,
                     objective = "binary:logistic",
                     verbose = 0)

# Predict and evaluate
xgb_pred <- predict(xgb_model, dtest)
predicted <- ifelse(xgb_pred > 0.5, 1, 0)
confusion <- confusionMatrix(as.factor(predicted), as.factor(test_label))
print(confusion)

# ROC curve and AUC
roc_obj <- roc(test$default, xgb_pred)
plot(roc_obj, main = "ROC Curve - XGBoost")
auc_val <- auc(roc_obj)
print(paste("AUC:", auc_val))

# SHAP values with SHAPforxgboost
shap_values <- shap.values(xgb_model = xgb_model, X_train = train_matrix)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = train_matrix)

# SHAP summary plot
shap.plot.summary(shap_long)

# Predict on a new applicant
new_applicant <- data.frame(
  loan_amount = 500000,
  interest_rate = 0.08,
  term_months = 36,
  income = 60000,
  age = 35,
  score = 620,
  risk_level = factor("Medium", levels = c("Low", "Medium", "High"), ordered = TRUE)
)

# One-hot encode new applicant to match training matrix columns
new_matrix <- model.matrix(~ . - 1, data = new_applicant)
dnew <- xgb.DMatrix(data = new_matrix)

new_pred <- predict(xgb_model, dnew)
print(paste("Predicted probability of default for new applicant:", new_pred))

# Saving files csv
write_csv(df, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/cleaned_loan_data.csv")
write_csv(df_model, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/model_dataset.csv")
write_csv(train, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/train_dataset.csv")
write_csv(test, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/test_dataset.csv")

# Test predictions
test_with_preds <- test %>%
  mutate(predicted_prob = xgb_pred,
         predicted_class = ifelse(xgb_pred > 0.5, 1, 0))
write_csv(test_with_preds, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/test_predictions.csv")

# SHAP output
shap_df <- as.data.frame(shap_values$shap_score)
write_csv(shap_df, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/shap_scores.csv")

write_csv(shap_long, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/shap_long_summary.csv")
