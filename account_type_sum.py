import pandas as pd

df = pd.read_csv("c_accounts.csv")

account_type_summary = df['account_type'].value_counts().reset_index()
account_type_summary.columns = ['account_type', 'count']
account_type_summary.to_csv("account_type_summary.csv", index=False)


account_type_balance = df.groupby('account_type')['balance'].sum().reset_index()
account_type_balance.columns = ['account_type', 'total_balance']
account_type_balance = account_type_balance.sort_values(by='total_balance', ascending=False)
account_type_balance.to_csv("account_type_balance_analysis.csv", index=False)
