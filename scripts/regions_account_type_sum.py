import pandas as pd

accounts = pd.read_csv('c_accounts.csv')
customers = pd.read_csv('b_customers.csv')

merged = pd.merge(accounts, customers, on='customer_id')

account_type_count = merged.groupby(['region', 'account_type']).size().reset_index(name='account_type_count')
account_type_count.to_csv("account_type_count_by_region.csv", index=False)

account_type_balance_region = merged.groupby(['region', 'account_type'])['balance'].sum().reset_index()
account_type_balance_region = account_type_balance_region.sort_values(by='balance', ascending=False)
account_type_balance_region.to_csv("balance_by_region_and_account_type.csv", index=False)
