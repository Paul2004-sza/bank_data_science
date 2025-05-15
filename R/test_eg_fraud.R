source("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/R/fraud_detection_logger.R")

# Test the function:
log_transaction_and_check_fraud(
  account_id = "A007",
  amount = 420000,
  type = "POS",
  channel = "Online",
  location = "Mandalay",
  fraud_score = 0.91,  # Optional, if NULL will randomize
  model_name = "XGBoost",
  model_version = "v1.2"
)