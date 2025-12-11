import pandas as pd
import ast

# Read the CSV
df = pd.read_csv("prices-raw.csv")

# Helper function to process the Prices column
def process_prices(prices_str):
    # Convert string representation of list to actual list
    prices_list = ast.literal_eval(prices_str)
    # Convert '£' strings to floats
    nums = [float(p.replace('£', '').replace(',', '')) for p in prices_list]
    return nums

# Apply processing
df['raw'] = df['Prices'].apply(process_prices)

# Compute summary stats
df['First'] = df['raw'].apply(lambda x: x[0] if x else None)
df['Max'] = df['raw'].apply(lambda x: max(x) if x else None)
df['Min'] = df['raw'].apply(lambda x: min(x) if x else None)
df['Average'] = df['raw'].apply(lambda x: round(sum(x)/len(x), 2) if x else None)

# Optional: reorder columns
df = df[['Location', 'Date', 'First', 'Max', 'Min', 'Average', 'Scraped']]

def reorder_df(df):
    # Ensure Date column is datetime for proper sorting
    df['Date'] = pd.to_datetime(df['Date'])
    # Sort by Location then Date
    df_sorted = df.sort_values(by=['Location', 'Date']).reset_index(drop=True)
    return df_sorted

df = reorder_df(df)

df.to_csv("prices.csv", index=False)
