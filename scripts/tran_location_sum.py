import pandas as pd

df = pd.read_csv("c_transactions_cleaned.csv")

location_summary = df['location'].value_counts().reset_index()
location_summary.columns = ['location', 'number_of_transactions']

location_summary.to_csv("location_transaction_summary.csv", index=False)

amount_summary = df.groupby('location')['amount'].sum().reset_index()
amount_summary.columns = ['location', 'total_transaction_amount']

amount_summary.to_csv("location_transaction_amount_summary.csv", index=False)

top_location = location_summary.iloc[0]
bottom_location = location_summary.iloc[-1]

print("Location-wise Transaction Count:")
print(location_summary.to_string(index=False))

print(f"\nMost Active Location: {top_location['location']} with {top_location['number_of_transactions']} transactions.")
print(f"Least Active Location: {bottom_location['location']} with {bottom_location['number_of_transactions']} transactions.")

print("\nRecommendation:")
print(f"- Focus on strengthening operations and campaigns in {top_location['location']} to maintain high performance.")
print(f"- Analyze factors limiting transaction activity in {bottom_location['location']} and explore improvement strategies.")
print("\nA separate CSV file 'location_transaction_amount_summary.csv' contains the total transaction amount for each location.")
