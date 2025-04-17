import pandas as pd

df = pd.read_csv("c_accounts.csv")

status_summary = df['status'].value_counts().reset_index()
status_summary.columns = ['status', 'count']
status_summary.to_csv("status_summary.csv", index=False)


status_balance = df.groupby('status')['balance'].sum().reset_index()
status_balance.columns = ['status', 'total_balance']
status_balance = status_balance.sort_values(by='total_balance', ascending=False)
status_balance.to_csv("status_balance_analysis.csv", index=False)
