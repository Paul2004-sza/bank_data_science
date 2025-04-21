import pandas as pd
import os
from dotenv import load_dotenv
from sqlalchemy import create_engine # engine = create_engin(f"mysql+pymysql://{user}:{password}@{host}/{name}")

load_dotenv()

DB_USER = os.getenv("DB_USER")
DB_PASSWORD = os.getenv("DB_PASSWORD")
DB_HOST = os.getenv("DB_HOST")
DB_NAME = os.getenv("DB_NAME")

if DB_PASSWORD:
    DB_PASSWORD = DB_PASSWORD.replace('@', '%40')

engine = create_engine(f"mysql+pymysql://{DB_USER}:{DB_PASSWORD}@{DB_HOST}/{DB_NAME}")

query = """
WITH monthly_summary AS (
    SELECT 
        location,
        DATE_FORMAT(transaction_date, '%%Y-%%m') AS month,
        COUNT(*) AS transaction_count,
        SUM(amount) AS total_amount
    FROM c_transactions_cleaned
    GROUP BY location, DATE_FORMAT(transaction_date, '%%Y-%%m')
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
"""

df = pd.read_sql(query, engine)

for location in df['location'].unique():
    location_df = df[df['location'] == location]
    file_name = f"{location}_date_pl_tran_amount_summary.csv"
    location_df.to_csv(file_name, index=False)
    print(f"CSV file saved: {file_name}")

all_locations_file = "all_loc_date_peak_least_tran_amount_summary.csv"
df.to_csv(all_locations_file, index=False)
print(f"CSV file saved for all locations: {all_locations_file}")
