import pandas as pd
import numpy as np

df = pd.read_csv("C:/Users/Sut Zaw Aung/StockMarketDB/pythonProjectBank/data/row/c_transactions_cleaned.csv")

df['transaction_date'] = pd.to_datetime(df['transaction_date'])

df['hour'] = df['transaction_date'].dt.hour
df['weekday'] = df['transaction_date'].dt.day_name()

pattern_df = df.groupby('account_id').agg({
    'amount': ['mean', 'std'],
    'type': lambda x: x.mode()[0] if not x.mode().empty else np.nan,
    'channel': lambda x: x.mode()[0] if not x.mode().empty else np.nan,
    'hour': lambda x: x.mode()[0] if not x.mode().empty else np.nan,
    'merchant': lambda x: x.mode()[0] if not x.mode().empty else np.nan,
    'location': lambda x: x.mode()[0] if not x.mode().empty else np.nan
}).reset_index()

pattern_df.columns = [
    'account_id', 'avg_amount', 'std_amount', 'most_common_type',
    'preferred_channel', 'usual_hour', 'top_merchant', 'top_location'
]

pattern_df.to_csv("customer_usual_patterns.csv", index=False)

print("Customer usual pattern saved to customer_usual_patterns.csv")
