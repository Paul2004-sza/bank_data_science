import pandas as pd

df = pd.read_csv('c_accounts.csv')

rows = []

balance_series = df['balance']

rows.append({'Column': 'Balance', 'Dtype': 'float64', 'Value': 'Total Accounts', 'Count': balance_series.count()})
rows.append({'Column': 'Balance', 'Dtype': 'float64', 'Value': 'Total Sum', 'Count': balance_series.sum()})
rows.append({'Column': 'Balance', 'Dtype': 'float64', 'Value': 'Mean', 'Count': balance_series.mean()})
rows.append({'Column': 'Balance', 'Dtype': 'float64', 'Value': 'Median', 'Count': balance_series.median()})
rows.append({'Column': 'Balance', 'Dtype': 'float64', 'Value': 'Min', 'Count': balance_series.min()})
rows.append({'Column': 'Balance', 'Dtype': 'float64', 'Value': 'Max', 'Count': balance_series.max()})
rows.append({'Column': 'Balance', 'Dtype': 'float64', 'Value': 'Std Deviation', 'Count': balance_series.std()})

bins = [0, 5000000, 10000000, 20000000, 50000000, 100000000]
labels = ['<5M', '5M-10M', '10M-20M', '20M-50M', '>50M']
balance_distribution = pd.cut(balance_series, bins=bins, labels=labels, right=False).value_counts().sort_index()

for label, count in balance_distribution.items():
    rows.append({
        'Column': 'Balance Range',
        'Dtype': 'category',
        'Value': label,
        'Count': count
    })

result_df = pd.DataFrame(rows)
result_df.to_csv('balance_analysis.csv', index=False)
