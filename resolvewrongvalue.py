import pandas as pd

df = pd.read_csv("transactions_row.csv")

regions = [
    "Yangon", "Mandalay", "Naypyidaw", "Bago", "Mawlamyine",
    "Taunggyi", "Monywa", "Myitkyina", "Pathein", "Sittwe"
]


valid_merchant_regions = {
    "City Mart": ["Yangon", "Mandalay", "Naypyidaw"],
    "Ocean Supercenter": ["Yangon", "Mandalay"],
    "Junction Square": ["Yangon"],
    "Myanmar Plaza": ["Yangon"],
    "Aung San Market": ["Yangon"], # do specific
    "Online Shop MM": regions,
    "KBZ Pay Merchant": regions,
    "Wave Pay Merchant": regions,
    "Mytel Payment": regions,
    "MPT Merchant": regions
}

df['merchant'] = df['merchant'].str.strip()
df['location'] = df['location'].str.strip().str.title()

df['merchant'] = df['merchant'].fillna('Unknown')

def correct_merchant(x):
    if isinstance(x, str):
        return next((k for k in valid_merchant_regions if x.lower() == k.lower()), x)
    return x

df['merchant'] = df['merchant'].apply(correct_merchant)

def fix_location(row):
    merchant = row['merchant']
    location = row['location']
    valid_locs = valid_merchant_regions.get(merchant, [])

    if location not in valid_locs:
        return valid_locs[0] if valid_locs else location
    return location

df['location'] = df.apply(fix_location, axis=1)

df.to_csv("c_transactions_cleaned.csv", index=False)

print("Transactions corrected and saved to 'c_transactions_cleaned.csv'")
