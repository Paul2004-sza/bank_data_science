CREATE TABLE tran_sum_location_by_month (
    id INT AUTO_INCREMENT PRIMARY KEY,
    location VARCHAR(100),
    date_time DATETIME,
    transaction_count INT,
    total_amount DECIMAL(15, 2),
    average_amount DECIMAL(15, 2)
);
