CREATE DATABASE fraud_detection;
USE fraud_detection;

CREATE TABLE IF NOT EXISTS transactions (
    transaction_id INT AUTO_INCREMENT PRIMARY KEY,
    account_id VARCHAR(50),
    amount DECIMAL(12, 2),
    type VARCHAR(50),
    channel VARCHAR(50),
    location VARCHAR(100),
    transaction_date DATETIME,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS fraud_alerts (
    alert_id INT AUTO_INCREMENT PRIMARY KEY,
    transaction_id INT,
    fraud_score FLOAT,
    fraud_flag BOOLEAN,
    detected_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (transaction_id) REFERENCES transactions(transaction_id)
);

CREATE TABLE IF NOT EXISTS model_logs (
    log_id INT AUTO_INCREMENT PRIMARY KEY,
    transaction_id INT,
    model_name VARCHAR(100),
    model_version VARCHAR(20),
    fraud_score FLOAT,
    prediction_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (transaction_id) REFERENCES transactions(transaction_id)
);
