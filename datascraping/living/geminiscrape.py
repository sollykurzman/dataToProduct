import requests
import json
import csv
from bs4 import BeautifulSoup

# User-Agent to avoid 403 Forbidden errors
headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
}

url = "https://www.numbeo.com/cost-of-living/region_rankings.jsp?title=2025-mid&region=150"

try:
    response = requests.get(url, headers=headers)
    response.raise_for_status()
except requests.exceptions.HTTPError as e:
    print(f"Error fetching URL: {e}")
    exit()

soup = BeautifulSoup(response.text, 'html.parser')

# 2. Locate the specific table
table = soup.find('table', {'id': 't2'})

if not table:
    print("Could not find the data table.")
    exit()

rows = table.find('tbody').find_all('tr')

print(f"Found {len(rows)} entries. Processing...")

# Data structures for storage
countries_data_json = {} # Hierarchical for JSON
flat_data_csv = []       # Flat list for CSV

# 3. Use enumerate to generate the rank automatically (start at 1)
for i, row in enumerate(rows, 1):
    cells = row.find_all('td')
    
    if len(cells) >= 8:
        # We calculate rank 'i' instead of scraping it
        rank = str(i) 
        
        full_city_name = cells[1].text.strip()
        
        # Handle city/country split
        if "," in full_city_name:
            parts = full_city_name.split(",")
            country = parts[-1].strip()
            city = ",".join(parts[:-1]).strip()
        else:
            city = full_city_name
            country = "Unknown"

        # Extract metrics
        metrics = {
            "rank": rank,
            "city": city,
            "cost_of_living_index": cells[2].text.strip(),
            "rent_index": cells[3].text.strip(),
            "cost_of_living_plus_rent_index": cells[4].text.strip(),
            "groceries_index": cells[5].text.strip(),
            "restaurant_price_index": cells[6].text.strip(),
            "local_purchasing_power_index": cells[7].text.strip()
        }

        if country not in countries_data_json:
            countries_data_json[country] = []
        countries_data_json[country].append(metrics)

        # For CSV, need the country inside the row
        csv_row = metrics.copy()
        csv_row['country'] = country
        flat_data_csv.append(csv_row)


# json_filename = "european_cities_cost_of_living.json"
# # Sort JSON by Country Key
# sorted_json = dict(sorted(countries_data_json.items()))

# with open(json_filename, 'w', encoding='utf-8') as f:
#     json.dump(sorted_json, f, indent=4, ensure_ascii=False)
# print(f"Saved JSON to {json_filename}")

#save to vsv
csv_filename = "european_cities_cost_of_living.csv"
csv_headers = ["rank", "country", "city", "cost_of_living_index", "rent_index", 
               "cost_of_living_plus_rent_index", "groceries_index", 
               "restaurant_price_index", "local_purchasing_power_index"]

with open(csv_filename, 'w', newline='', encoding='utf-8') as f:
    writer = csv.DictWriter(f, fieldnames=csv_headers)
    writer.writeheader()
    writer.writerows(flat_data_csv)

print(f"Saved CSV to {csv_filename}")