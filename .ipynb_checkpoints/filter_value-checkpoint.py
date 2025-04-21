import pandas as pd

df = pd.read_csv('c_accounts.csv')

include_columns = ['account_type', 'status', 'open_date','customer_id','balance']

rows = []

rows.append({
    'Column': 'Total Accounts (account_id)',
    'Dtype': 'int64',
    'Value': '',
    'Count': df['account_id'].nunique()
})

for column in df.columns:
    if column in include_columns:
        dtype = df[column].dtype
        counts = df[column].value_counts(dropna=False)

        for value, count in counts.items():
            rows.append({
                'Column': column,
                'Dtype': str(dtype),
                'Value': value,
                'Count': count
            })

result_df = pd.DataFrame(rows)

result_df.to_csv('accounts_summary_with_totals.csv', index=False)
