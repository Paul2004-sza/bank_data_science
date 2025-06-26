library(tidyverse)
library(lubridate)

customers <- read.csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/b_customers.csv")
accounts <- read.csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_accounts.csv")
transactions <- read.csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_transactions_cleaned.csv")
campaigns <- read.csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_campaign_responses.csv")

customers$join_date <- ymd(customers$join_date)
customers$date_of_birth <- ymd(customers$date_of_birth)
accounts$open_date <- ymd(accounts$open_date)
transactions$transaction_date <- ymd_hms(transactions$transaction_date)
campaigns$response_date <- ymd(campaigns$response_date)

glimpse(customers)
glimpse(accounts)
glimpse(transactions)
glimpse(campaigns)

today <- as.Date("2025-4-25")

last_txn <- transactions %>%
  left_join(accounts, by = "account_id") %>%
  group_by(customer_id) %>%
  summarise(last_transaction_date = max(as.Date(transaction_date), na.rm = TRUE)) %>%
  mutate(days_since_last_txn = as.numeric(today - last_transaction_date))

account_status <- accounts %>%
  group_by(customer_id) %>%
  summarise(all_closed = all(status == "Closed"))

customer_churn <- customers %>%
  left_join(last_txn, by = "customer_id") %>%
  left_join(account_status, by = "customer_id") %>%
  mutate(
    churned = case_when(
      is.na(days_since_last_txn) & all_closed == TRUE ~ 1,
      is.na(days_since_last_txn) & all_closed == FALSE ~ NA_real_,
      days_since_last_txn > 90 & all_closed == TRUE ~ 1,
      TRUE ~ 0
    )
  )
head(customer_churn)
write.csv(customer_churn, "C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/ML_data/Customer_Segm/churn_labeled_customers.csv", row.names = FALSE)