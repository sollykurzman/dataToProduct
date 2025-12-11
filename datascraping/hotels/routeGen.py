import pandas as pd
from itertools import product

#config
cities_file = "cities.csv"
dates_file = "dates.csv"
output_file = "routes.csv"

#load csvs
cities_df = pd.read_csv(cities_file)
dates_df = pd.read_csv(dates_file)

cities = cities_df["CityName"].tolist()
dates = dates_df["Date"].tolist()

#create all combinations
pairs = list(product(cities, dates))

#reorder
ordered_pairs = []
for i, date in enumerate(dates):
    for city in cities:
        ordered_pairs.append((city, date))

#store to csv
routes_df = pd.DataFrame(ordered_pairs, columns=["Location", "Date"])
routes_df.to_csv(output_file, index=False)

print(f"✅ routes.csv created successfully with {len(routes_df)} rows.")
