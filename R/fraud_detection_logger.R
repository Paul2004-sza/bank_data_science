library(DBI)
library(RMySQL) # or RMariaDB

CLIENT_LOCAL_FILES <- 4L

readRenviron("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/.Renviron")

connect_db <- function() {
  dbConnect(
    RMySQL::MySQL(),  # or RMariaDB::MariaDB()
    dbname   = Sys.getenv("MYSQL_DB"),
    host     = Sys.getenv("MYSQL_HOST"),
    port     = as.integer(Sys.getenv("MYSQL_PORT")),
    user     = Sys.getenv("MYSQL_USER"),
    password = Sys.getenv("MYSQL_PASSWORD"),
    client.flag = CLIENT_LOCAL_FILES
  )
}

log_transaction_and_check_fraud <- function(account_id, amount, type, channel, location,
                                            fraud_score = NULL,
                                            model_name = "ManualCheck", model_version = "v1.0") {
  con <- connect_db()
  
  new_trans <- data.frame(
    account_id = account_id,
    amount = amount,
    type = type,
    channel = channel,
    location = location,
    transaction_date = Sys.time()
  )
  
  dbWriteTable(con, "transactions", new_trans, append = TRUE, row.names = FALSE)
  last_id <- dbGetQuery(con, "SELECT LAST_INSERT_ID() AS transaction_id")$transaction_id
  
  if (is.null(fraud_score)) fraud_score <- runif(1, 0, 1)
  
  fraud_flag <- fraud_score > 0.85
  
  if (fraud_flag) {
    alert <- data.frame(
      transaction_id = last_id,
      fraud_score = fraud_score,
      fraud_flag = TRUE
    )
    dbWriteTable(con, "fraud_alerts", alert, append = TRUE, row.names = FALSE)
    message("FRAUD ALERT for transaction ID: ", last_id)
  } else {
    message("Transaction OK. ID: ", last_id)
  }
  
  model_log <- data.frame(
    transaction_id = last_id,
    model_name = model_name,
    model_version = model_version,
    fraud_score = fraud_score
  )
  dbWriteTable(con, "model_logs", model_log, append = TRUE, row.names = FALSE)
  
  dbDisconnect(con)
}


