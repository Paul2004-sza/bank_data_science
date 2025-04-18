use bank_db;
CREATE TABLE b_customers (
    customer_id INT PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(100),
    gender ENUM('Male', 'Female', 'Other'),
    date_of_birth DATE,
    age INT,
    marital_status ENUM('Single', 'Married', 'Divorced', 'Widowed'),
    education_level VARCHAR(50),
    occupation VARCHAR(100),
    income DECIMAL(12,2),
    region VARCHAR(100),
    join_date DATE
);
CREATE TABLE c_accounts (
    account_id INT PRIMARY KEY AUTO_INCREMENT,
    customer_id INT,
    account_type ENUM('Savings', 'Current', 'Credit', 'Loan'),
    open_date DATE,
    status ENUM('Active', 'Closed', 'Frozen'),
    balance DECIMAL(15,2),
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);
CREATE TABLE c_transactions (
    transaction_id INT PRIMARY KEY AUTO_INCREMENT,
    account_id INT,
    transaction_date DATETIME,
    amount DECIMAL(12,2),
    type ENUM('Debit', 'Credit'),
    channel ENUM('ATM', 'POS', 'Online', 'Branch'),
    merchant VARCHAR(100),
    location VARCHAR(100),
    FOREIGN KEY (account_id) REFERENCES accounts(account_id)
);
CREATE TABLE c_loans (
    loan_id INT PRIMARY KEY AUTO_INCREMENT,
    customer_id INT,
    loan_type ENUM('Personal', 'Home', 'Car', 'Education', 'Business'),
    loan_amount DECIMAL(15,2),
    interest_rate DECIMAL(5,2),
    term_months INT,
    start_date DATE,
    end_date DATE,
    status ENUM('Approved', 'Rejected', 'Paid', 'Defaulted'),
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);
CREATE TABLE c_credit_scores (
    score_id INT PRIMARY KEY AUTO_INCREMENT,
    customer_id INT,
    score INT,
    date_recorded DATE,
    risk_level ENUM('Low', 'Medium', 'High'),
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);
CREATE TABLE b_marketing_campaigns (
    campaign_id INT PRIMARY KEY AUTO_INCREMENT,
    campaign_name VARCHAR(100),
    start_date DATE,
    end_date DATE,
    channel ENUM('Email', 'SMS', 'Phone', 'App Notification')
);
CREATE TABLE c_campaign_responses (
    response_id INT PRIMARY KEY AUTO_INCREMENT,
    campaign_id INT,
    customer_id INT,
    response ENUM('Yes', 'No'),
    response_date DATE,
    FOREIGN KEY (campaign_id) REFERENCES marketing_campaigns(campaign_id),
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);
CREATE TABLE c_fraud_alerts (
    alert_id INT PRIMARY KEY AUTO_INCREMENT,
    transaction_id INT,
    flagged_reason VARCHAR(255),
    model_confidence DECIMAL(5,2),
    is_confirmed BOOLEAN DEFAULT FALSE,
    flagged_at DATETIME,
    FOREIGN KEY (transaction_id) REFERENCES transactions(transaction_id)
);
