import csv
from datetime import datetime, timedelta

#config
start_date_str = "2025-12-02"
gap_days = 2
num_dates = 183
output_file = "dates-main-2.csv"

#generator
start_date = datetime.strptime(start_date_str, "%Y-%m-%d")
dates = [(start_date + timedelta(days=i * gap_days)).strftime("%Y-%m-%d") for i in range(num_dates)]

#store
with open(output_file, "w", newline="", encoding="utf-8") as f:
    writer = csv.writer(f)
    writer.writerow(["Date"])
    for d in dates:
        writer.writerow([d])

print(f"✅ CSV file '{output_file}' created with {num_dates} dates spaced {gap_days} days apart.")
