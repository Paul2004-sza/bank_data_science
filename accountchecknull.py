import pandas as pd

df = pd.read_csv('accounts.csv')

summary = {
    'Column': df.columns,
    'Non-Null Count': df.notnull().sum().values,
    'Dtype': df.dtypes.astype(str).values
}

summary_df = pd.DataFrame(summary)

summary_df.to_csv('nullnotfound_summary_account.csv', index=False)
