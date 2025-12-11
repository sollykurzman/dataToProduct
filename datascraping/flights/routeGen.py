import pandas as pd
import itertools
import random

#paths
airport_file = "airports.csv"
dates_file = "dates.csv"
output_file = "routes.csv"

#read csv
airports = pd.read_csv(airport_file)
dates = pd.read_csv(dates_file)

# Ensure columns are correct
airport_codes = airports["AirportCode"].dropna().unique().tolist()
dates_list = pd.to_datetime(dates["Date"].dropna()).sort_values().tolist()

#generate all unique pairs of airports (From, To)
pairs = []
for i in range(len(airport_codes)):
    for j in range(i + 1, len(airport_codes)):
        pairs.append((airport_codes[i], airport_codes[j]))

#create df
all_rows = []
for d in dates_list:
    random.shuffle(pairs)  # randomize order for each date
    for f, t in pairs:
        all_rows.append((f, t, d))

df = pd.DataFrame(all_rows, columns=["From", "To", "Date"])

#sort by date
df = df.sort_values(by="Date").reset_index(drop=True)

#save to csv
df.to_csv(output_file, index=False)

print(f"✅ Created {output_file} with {len(df)} rows.")
