import random
from datetime import datetime, timedelta
import pandas as pd
import numpy as np
from faker import Faker

fake = Faker()

random.seed(42)
np.random.seed(42)

# Myanmar-specific configurations
myanmar_first_names_male = ["Aung", "Zaw", "Htun", "Kyaw", "Min", "Myo", "Thura", "Win", "Yan", "Naing"]
myanmar_first_names_female = ["Su", "Hla", "Khin", "Nilar", "Thandar", "May", "Yin", "Phyu", "Thet", "Nu"]
myanmar_last_names = ["Htun", "Win", "Oo", "Kyaw", "Aung", "Zaw", "Moe", "Lin", "Hlaing", "Myint"]

regions = ["Yangon", "Mandalay", "Naypyidaw", "Bago", "Mawlamyine", "Taunggyi", "Monywa", "Myitkyina", "Pathein",
           "Sittwe"]
occupations = ["Farmer", "Teacher", "Government Employee", "Business Owner", "Merchant", "Engineer",
               "Doctor", "Driver", "Fisherman", "Construction Worker", "Bank Employee", "Tour Guide"]
education_levels = ["Primary", "Middle School", "High School", "Diploma", "Bachelor", "Master", "PhD"]
merchants = ["City Mart", "Ocean Supercenter", "Junction Square", "Myanmar Plaza", "Aung San Market",
             "Online Shop MM", "KBZ Pay Merchant", "Wave Pay Merchant", "Mytel Payment", "MPT Merchant"]


# Function to generate Myanmar names
def generate_myanmar_name(gender):
    if gender == "Male":
        first_name = random.choice(myanmar_first_names_male)
    else:
        first_name = random.choice(myanmar_first_names_female)
    last_name = random.choice(myanmar_last_names)
    return f"{first_name} {last_name}"


# Helper function to convert date string to datetime object
def convert_to_datetime(date_str):
    return datetime.strptime(date_str, '%Y-%m-%d')


# Generate Customers Data
def generate_customers(num=1000):
    data = []
    for i in range(1, num + 1):
        gender = random.choice(["Male", "Female", "Other"])
        dob = fake.date_of_birth(minimum_age=18, maximum_age=80)
        age = datetime.now().year - dob.year
        join_date = fake.date_between(start_date='-10y', end_date='today')

        data.append({
            "customer_id": i,
            "name": generate_myanmar_name(gender),
            "gender": gender,
            "date_of_birth": dob.strftime('%Y-%m-%d'),
            "age": age,
            "marital_status": random.choice(["Single", "Married", "Divorced", "Widowed"]),
            "education_level": random.choice(education_levels),
            "occupation": random.choice(occupations),
            "income": round(random.uniform(300000, 5000000), 2),  # MMK
            "region": random.choice(regions),
            "join_date": join_date.strftime('%Y-%m-%d'),
            "email": fake.email(),
            "phone": fake.phone_number()
        })
    return pd.DataFrame(data)


# Generate Accounts Data
def generate_accounts(customers_df, avg_accounts_per_customer=1.5):
    data = []
    account_id = 1
    for _, customer in customers_df.iterrows():
        num_accounts = np.random.poisson(avg_accounts_per_customer - 1) + 1
        for _ in range(num_accounts):
            join_date_obj = convert_to_datetime(customer['join_date']).date()
            open_date = fake.date_between(start_date=join_date_obj, end_date='today')
            data.append({
                "account_id": account_id,
                "customer_id": customer['customer_id'],
                "account_type": random.choice(["Savings", "Current", "Credit", "Loan"]),
                "open_date": open_date.strftime('%Y-%m-%d'),
                "status": random.choices(["Active", "Closed", "Frozen"], weights=[0.85, 0.1, 0.05])[0],
                "balance": round(random.uniform(10000, 50000000), 2)  # MMK
            })
            account_id += 1
    return pd.DataFrame(data)


# Generate Transactions Data
def generate_transactions(accounts_df, avg_transactions_per_account=20):
    data = []
    transaction_id = 1
    for _, account in accounts_df.iterrows():
        if account['status'] != 'Active':
            continue

        num_transactions = np.random.poisson(avg_transactions_per_account)
        for _ in range(num_transactions):
            open_date_obj = convert_to_datetime(account['open_date'])
            transaction_date = fake.date_time_between(start_date=open_date_obj, end_date='now')
            amount = round(random.uniform(1000, 5000000), 2)
            transaction_type = random.choices(["Debit", "Credit"], weights=[0.6, 0.4])[0]

            data.append({
                "transaction_id": transaction_id,
                "account_id": account['account_id'],
                "transaction_date": transaction_date.strftime('%Y-%m-%d %H:%M:%S'),
                "amount": amount,
                "type": transaction_type,
                "channel": random.choice(["ATM", "POS", "Online", "Branch"]),
                "merchant": random.choice(merchants) if transaction_type == "Debit" else "",
                "location": random.choice(regions)
            })
            transaction_id += 1
    return pd.DataFrame(data)


# Generate Loans Data
def generate_loans(customers_df, loan_probability=0.3):
    data = []
    loan_id = 1
    for _, customer in customers_df.iterrows():
        if random.random() < loan_probability:
            loan_type = random.choice(["Personal", "Home", "Car", "Education", "Business"])
            loan_amount = round(random.uniform(1000000, 100000000), 2)  # MMK
            start_date = datetime.strptime(customer['join_date'], '%Y-%m-%d').date()  # Convert join_date to datetime.date
            start_date = fake.date_between(start_date=start_date, end_date='today')  # Use start_date as datetime.date
            term_months = random.choice([12, 24, 36, 60, 120])
            end_date = start_date + timedelta(days=term_months * 30)

            data.append({
                "loan_id": loan_id,
                "customer_id": customer['customer_id'],
                "loan_type": loan_type,
                "loan_amount": loan_amount,
                "interest_rate": round(random.uniform(8.0, 15.0), 2),
                "term_months": term_months,
                "start_date": start_date.strftime('%Y-%m-%d'),
                "end_date": end_date.strftime('%Y-%m-%d'),
                "status": random.choices(["Approved", "Rejected", "Paid", "Defaulted"], weights=[0.6, 0.1, 0.25, 0.05])[0]
            })
            loan_id += 1
    return pd.DataFrame(data)

# Generate Credit Scores Data
def generate_credit_scores(customers_df):
    data = []
    score_id = 1
    for _, customer in customers_df.iterrows():
        base_score = random.randint(300, 850)
        # Adjust score based on income and age
        score = min(850, max(300, base_score +
                             (customer['income'] / 1000000) * 10 +
                             (customer['age'] - 30) * 0.5))

        # Record multiple scores over time
        num_records = random.randint(1, 5)
        for i in range(num_records):
            # Convert 'join_date' to datetime.date
            start_date = datetime.strptime(customer['join_date'], '%Y-%m-%d').date()  # Convert join_date to datetime.date
            date_recorded = fake.date_between(start_date=start_date, end_date='today')  # Use datetime.date for start_date
            # Add some variation to the score
            current_score = min(850, max(300, int(score + random.gauss(0, 20))))
            risk_level = "Low" if current_score > 700 else "Medium" if current_score > 600 else "High"

            data.append({
                "score_id": score_id,
                "customer_id": customer['customer_id'],
                "score": current_score,
                "date_recorded": date_recorded.strftime('%Y-%m-%d'),
                "risk_level": risk_level
            })
            score_id += 1
    return pd.DataFrame(data)


# Generate Marketing Campaigns Data
def generate_marketing_campaigns(num=10):
    data = []
    for i in range(1, num + 1):
        start_date = fake.date_between(start_date='-2y', end_date='today')
        end_date = start_date + timedelta(days=random.randint(7, 60))
        data.append({
            "campaign_id": i,
            "campaign_name": f"Campaign_{i}_{start_date.year}",
            "start_date": start_date.strftime('%Y-%m-%d'),
            "end_date": end_date.strftime('%Y-%m-%d'),
            "channel": random.choice(["Email", "SMS", "Phone", "App Notification"])
        })
    return pd.DataFrame(data)


# Generate Campaign Responses Data
# Generate Campaign Responses Data
def generate_campaign_responses(marketing_campaigns_df, customers_df):
    data = []
    response_id = 1
    for _, campaign in marketing_campaigns_df.iterrows():
        # Convert campaign start_date and end_date to datetime.date
        start_date = datetime.strptime(campaign['start_date'], '%Y-%m-%d').date()
        end_date = datetime.strptime(campaign['end_date'], '%Y-%m-%d').date()

        # Generate responses from customers for each campaign
        for _, customer in customers_df.iterrows():
            response_date = fake.date_between(start_date=start_date, end_date=end_date)
            responded = random.choice([True, False])  # Randomly decide if customer responds to the campaign
            data.append({
                "response_id": response_id,
                "campaign_id": campaign['campaign_id'],
                "customer_id": customer['customer_id'],
                "response_date": response_date.strftime('%Y-%m-%d'),
                "responded": responded
            })
            response_id += 1
    return pd.DataFrame(data)


# Main script to generate and save all data
# Generate Fraud Alerts Data
def generate_fraud_alerts(transactions_df):
    data = []
    fraud_id = 1
    for _, transaction in transactions_df.iterrows():
        # Convert transaction_date to datetime and extract the date
        transaction_date = pd.to_datetime(transaction['transaction_date']).date()

        # Generate fraud alert date
        fraud_date = fake.date_between(start_date=transaction_date, end_date='today')

        # Randomly decide if the transaction is fraud
        is_fraud = random.choice([True, False])
        data.append({
            "fraud_id": fraud_id,
            "transaction_id": transaction['transaction_id'],
            "fraud_date": fraud_date.strftime('%Y-%m-%d'),
            "is_fraud": is_fraud
        })
        fraud_id += 1
    return pd.DataFrame(data)


def save_to_csv(df, filename):
    df.to_csv(filename, index=False)
    print(f"{filename} saved!")


# Main script to generate and save all data
if __name__ == "__main__":
    customers_df = generate_customers(1000)
    accounts_df = generate_accounts(customers_df)
    transactions_df = generate_transactions(accounts_df)
    loans_df = generate_loans(customers_df)
    credit_scores_df = generate_credit_scores(customers_df)
    marketing_campaigns_df = generate_marketing_campaigns()
    campaign_responses_df = generate_campaign_responses(marketing_campaigns_df, customers_df)
    fraud_alerts_df = generate_fraud_alerts(transactions_df)

    save_to_csv(customers_df, "customers.csv")
    save_to_csv(accounts_df, "accounts.csv")
    save_to_csv(transactions_df, "transactions.csv")
    save_to_csv(loans_df, "loans.csv")
    save_to_csv(credit_scores_df, "credit_scores.csv")
    save_to_csv(marketing_campaigns_df, "marketing_campaigns.csv")
    save_to_csv(campaign_responses_df, "campaign_responses.csv")
    save_to_csv(fraud_alerts_df, "fraud_alerts.csv")  # Save fraud alerts data
