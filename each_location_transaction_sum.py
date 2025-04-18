import pandas as pd

df = pd.read_csv("c_transactions_cleaned.csv")
df['transaction_date'] = pd.to_datetime(df['transaction_date'])
df['year_month'] = df['transaction_date'].dt.to_period('M').astype(str)

monthly_location_summary = df.groupby(['location', 'year_month']).agg(
    number_of_transactions=('transaction_id', 'count'),
    total_amount=('amount', 'sum')
).reset_index()

peak_transaction_months = monthly_location_summary.loc[
    monthly_location_summary.groupby('location')['number_of_transactions'].idxmax()
].rename(columns={
    'year_month': 'peak_transaction_month',
    'number_of_transactions': 'max_transactions'
})

peak_amount_months = monthly_location_summary.loc[
    monthly_location_summary.groupby('location')['total_amount'].idxmax()
].rename(columns={
    'year_month': 'peak_amount_month',
    'total_amount': 'max_total_amount'
})

summary = pd.merge(peak_transaction_months[['location', 'peak_transaction_month', 'max_transactions']],
                   peak_amount_months[['location', 'peak_amount_month', 'max_total_amount']],
                   on='location')


summary.to_csv("location_peak_transaction_summary.csv", index=False)

