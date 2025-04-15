import pandas as pd

df = pd.read_csv("transactions_cleaned.csv")

location_summary = df['location'].value_counts().reset_index()
location_summary.columns = ['location', 'number_of_transactions']

location_summary.to_csv("location_transaction_summary.csv", index=False)

top_location = location_summary.iloc[0]

bottom_location = location_summary.iloc[-1]

print("📊 Location-wise Transaction Count:")
print(location_summary.to_string(index=False))

print(f" Most Active Location: {top_location['location']} with {top_location['number_of_transactions']} transactions.")
print(f"Least Active Location: {bottom_location['location']} with {bottom_location['number_of_transactions']} transactions.")

print("\n📌 Recommendation:")
print(f"- Focus on strengthening operations and campaigns in {top_location['location']} to maintain high performance.")
print(f"- Analyze factors limiting transaction activity in {bottom_location['location']} and explore improvement strategies.")

