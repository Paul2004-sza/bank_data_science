SELECT 
    location, 
    SUM(amount) AS total_transaction_amount
FROM 
    c_transactions_cleaned
GROUP BY 
    location
ORDER BY 
    total_transaction_amount DESC;
-- Sum the transaction amount per location