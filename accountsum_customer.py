import pandas as pd

df = pd.read_csv('accounts.csv')

rows = []

rows.append({
    'Column': 'Total Rows',
    'Dtype': '',
    'Value': '',
    'Count': len(df)
})

for column in df.columns:
    dtype = df[column].dtype
    unique_count = df[column].nunique(dropna=False)

    rows.append({
        'Column': f'Unique Values in {column}',
        'Dtype': str(dtype),
        'Value': '',
        'Count': unique_count
    })

customer_counts = df['customer_id'].value_counts()

multiple_accounts = (customer_counts > 1).sum()

max_accounts = customer_counts.max()

account_distribution = customer_counts.value_counts().sort_index()

rows.append({
    'Column': 'Customers with >1 Account',
    'Dtype': 'int64',
    'Value': '',
    'Count': multiple_accounts
})

rows.append({
    'Column': 'Max Accounts by One Customer',
    'Dtype': 'int64',
    'Value': '',
    'Count': max_accounts
})

for num_accounts, num_customers in account_distribution.items():
    rows.append({
        'Column': 'Customers with X Accounts',
        'Dtype': 'int64',
        'Value': f'{num_accounts} accounts',
        'Count': num_customers
    })

result_df = pd.DataFrame(rows)
result_df.to_csv('accounts_summary_with_customer_analysis.csv', index=False)
