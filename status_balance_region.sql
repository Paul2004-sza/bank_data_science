SELECT 
    c.region,
    a.status,
    SUM(a.balance) AS total_balance
FROM 
    c_accounts a
JOIN 
    b_customers c ON a.customer_id = c.customer_id
GROUP BY 
    c.region, a.status
ORDER BY 
    total_balance DESC;
