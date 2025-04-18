import pandas as pd
import os
from dotenv import load_dotenv
from sqlalchemy import create_engine
import plotly.express as px

load_dotenv()

DB_USER = os.getenv("DB_USER")
DB_PASSWORD = os.getenv("DB_PASSWORD")
DB_HOST = os.getenv("DB_HOST")
DB_NAME = os.getenv("DB_NAME")

if DB_PASSWORD:
    DB_PASSWORD = DB_PASSWORD.replace('@', '%40')

engine = create_engine(f"mysql+pymysql://{DB_USER}:{DB_PASSWORD}@{DB_HOST}/{DB_NAME}")

def execute_query(query, engine):
    return pd.read_sql(query, engine)

monthly_query = """
SELECT 
    DATE_FORMAT(transaction_date, '%%Y-%%m') AS date_time,
    COUNT(*) AS transaction_count,
    SUM(amount) AS total_amount,
    AVG(amount) AS average_amount,
    MIN(amount) AS min_amount,
    MAX(amount) AS max_amount
FROM c_transactions_cleaned
GROUP BY DATE_FORMAT(transaction_date, '%%Y-%%m')
ORDER BY date_time;
"""


type_monthly_query = """
SELECT 
    type,
    DATE_FORMAT(transaction_date, '%%Y-%%m') AS date_time,
    COUNT(*) AS transaction_count,
    SUM(amount) AS total_amount
FROM c_transactions_cleaned
GROUP BY type, DATE_FORMAT(transaction_date, '%%Y-%%m')
ORDER BY type, date_time;
"""

location_monthly_query = """
SELECT 
    location,
    DATE_FORMAT(transaction_date, '%%Y-%%m') AS date_time,
    COUNT(*) AS transaction_count,
    SUM(amount) AS total_amount,
    AVG(amount) AS average_amount
FROM c_transactions_cleaned
GROUP BY location, DATE_FORMAT(transaction_date, '%%Y-%%m')
ORDER BY location, date_time;
"""

top_locations_query = """
SELECT 
    location,
    COUNT(*) AS transaction_count,
    SUM(amount) AS total_amount,
    COUNT(*) / (SELECT COUNT(*) FROM c_transactions_cleaned) * 100 AS percentage_of_total
FROM c_transactions_cleaned
GROUP BY location
ORDER BY transaction_count DESC
LIMIT 10;
"""

channel_query = """
SELECT 
    channel,
    COUNT(*) AS transaction_count,
    SUM(amount) AS total_amount,
    AVG(amount) AS average_amount
FROM c_transactions_cleaned
GROUP BY channel
ORDER BY transaction_count DESC;
"""

try:
    print("Running transaction analysis...")

    # Monthly analysis
    monthly_df = execute_query(monthly_query, engine)
    monthly_df.to_csv('monthly_transactions_summary.csv', index=False)

    # Monthly by type
    type_monthly_df = execute_query(type_monthly_query, engine)
    type_monthly_df.to_csv('monthly_transactions_by_type.csv', index=False)

    # Monthly by location
    location_monthly_df = execute_query(location_monthly_query, engine)
    location_monthly_df.to_csv('monthly_transactions_by_location.csv', index=False)

    # Top locations
    top_locations_df = execute_query(top_locations_query, engine)
    top_locations_df.to_csv('top_transaction_locations.csv', index=False)

    # Channel analysis
    channel_df = execute_query(channel_query, engine)
    channel_df.to_csv('transaction_channel_analysis.csv', index=False)

    # Generate additional summary files
    # Peak transaction months
    peak_months = monthly_df.sort_values('transaction_count', ascending=False).head(5)
    peak_months.to_csv('peak_transaction_months.csv', index=False)

    # Highest revenue months
    revenue_months = monthly_df.sort_values('total_amount', ascending=False).head(5)
    revenue_months.to_csv('highest_revenue_months.csv', index=False)

    # Top performing locations by month
    top_location_months = location_monthly_df.sort_values(['date_time', 'total_amount'],
                                                          ascending=[True, False])
    top_location_months.to_csv('top_locations_by_month.csv', index=False)

    print("Analysis completed successfully!")
    print("Generated CSV files:")
    print("- monthly_transactions_summary.csv")
    print("- monthly_transactions_by_type.csv")
    print("- monthly_transactions_by_location.csv")
    print("- top_transaction_locations.csv")
    print("- transaction_channel_analysis.csv")
    print("- peak_transaction_months.csv")
    print("- highest_revenue_months.csv")
    print("- top_locations_by_month.csv")

    # Monthly Transaction Count - Interactive visualizations
    fig1 = px.bar(
        monthly_df,
        x='date_time',
        y='transaction_count',
        title='📊 Monthly Transaction Count',
        labels={'date_time': 'Month', 'transaction_count': 'Number of Transactions'},
        color='transaction_count',
        color_continuous_scale='Blues'
    )
    fig1.update_layout(xaxis_tickangle=-45)
    fig1.write_html("monthly_transaction_count_interactive.html")

    fig2 = px.bar(
        monthly_df,
        x='date_time',
        y='total_amount',
        title='💰 Monthly Transaction Amount',
        labels={'date_time': 'Month', 'total_amount': 'Total Amount'},
        color='total_amount',
        color_continuous_scale='Greens'
    )
    fig2.update_layout(xaxis_tickangle=-45)
    fig2.write_html("monthly_transaction_amount_interactive.html")

    print("Generated interactive visualizations:")
    print("- monthly_transaction_count_interactive.html")
    print("- monthly_transaction_amount_interactive.html")


except Exception as e:
    print(f"An error occurred: {e}")
