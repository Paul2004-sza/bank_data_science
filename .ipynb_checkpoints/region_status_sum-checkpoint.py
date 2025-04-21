import pandas as pd

accounts = pd.read_csv('c_accounts.csv')
customers = pd.read_csv('b_customers.csv')

merged = pd.merge(accounts, customers, on='customer_id')

status_count = merged.groupby(['region', 'status']).size().reset_index(name='status_count')
status_count.to_csv("status_count_by_region.csv", index=False)

status_balance_region = merged.groupby(['region', 'status'])['balance'].sum().reset_index()
status_balance_region = status_balance_region.sort_values(by='balance', ascending=False)
status_balance_region.to_csv("balance_by_region_and_status.csv", index=False)

region_balance = merged.groupby('region')['balance'].sum().reset_index().sort_values(by='balance', ascending=False)
region_balance.to_csv("total_balance_by_region.csv", index=False)
