WITH monthly_summary AS (
    SELECT 
        location,
        DATE_FORMAT(transaction_date, '%Y-%m') AS month,
        COUNT(*) AS transaction_count,
        SUM(amount) AS total_amount
    FROM c_transactions_cleaned
    GROUP BY location, DATE_FORMAT(transaction_date, '%Y-%m')
),
ranked_summary AS (
    SELECT *,
           RANK() OVER (PARTITION BY location ORDER BY transaction_count DESC) AS txn_rank_desc,
           RANK() OVER (PARTITION BY location ORDER BY transaction_count ASC) AS txn_rank_asc,
           RANK() OVER (PARTITION BY location ORDER BY total_amount DESC) AS amt_rank_desc,
           RANK() OVER (PARTITION BY location ORDER BY total_amount ASC) AS amt_rank_asc
    FROM monthly_summary
)
SELECT 
    location,

    -- Peak by transactions
    MAX(CASE WHEN txn_rank_desc = 1 THEN month END) AS peak_transaction_month,
    MAX(CASE WHEN txn_rank_desc = 1 THEN transaction_count END) AS peak_transaction_count,

    -- Least by transactions
    MAX(CASE WHEN txn_rank_asc = 1 THEN month END) AS least_transaction_month,
    MAX(CASE WHEN txn_rank_asc = 1 THEN transaction_count END) AS least_transaction_count,

    -- Peak by amount
    MAX(CASE WHEN amt_rank_desc = 1 THEN month END) AS peak_amount_month,
    MAX(CASE WHEN amt_rank_desc = 1 THEN total_amount END) AS peak_total_amount,

    -- Least by amount
    MAX(CASE WHEN amt_rank_asc = 1 THEN month END) AS least_amount_month,
    MAX(CASE WHEN amt_rank_asc = 1 THEN total_amount END) AS least_total_amount

FROM ranked_summary
GROUP BY location;
