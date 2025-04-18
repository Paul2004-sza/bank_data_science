use bank_db;
SELECT 
    location, 
    COUNT(*) AS number_of_transactions
FROM 
    c_transactions_cleaned
GROUP BY 
    location
ORDER BY 
    number_of_transactions DESC;
-- Count the number of transactions per location
