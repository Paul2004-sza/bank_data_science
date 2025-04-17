SELECT 
    c.region,
    a.status,
    COUNT(*) AS status_count
FROM 
    c_accounts a
JOIN 
    b_customers c ON a.customer_id = c.customer_id
GROUP BY 
    c.region, a.status
ORDER BY 
    c.region, a.status;
