import pandas as pd
import sys
import os

def filter_csv(file_path):
    target_cities = [
        "Vienna", "Prague", "Copenhagen", "Paris", "Munich", 
        "Berlin", "Dublin", "Amsterdam", "Madrid", "Barcelona", 
        "Zurich", "London", "Edinburgh", "Glasgow", "Stockholm", "Tallinn"
    ]
    
    # The name of the column in your CSV containing city names
    city_column_name = 'city'

    if not os.path.exists(file_path):
        print(f"Error: The file '{file_path}' was not found.")
        return

    print(f"Reading file: {file_path}...")
    try:
        df = pd.read_csv(file_path)
    except Exception as e:
        print(f"Error reading CSV: {e}")
        return

    # Check if the city column exists
    # normalize column names to lowercase for the check to be safe
    df.columns = [c.strip() for c in df.columns] # Clean whitespace from headers
    lower_cols = [c.lower() for c in df.columns]
    
    if city_column_name.lower() not in lower_cols:
        print(f"Error: Could not find a column named '{city_column_name}' in the CSV.")
        print(f"Available columns: {list(df.columns)}")
        return

    # Find the exact case-sensitive column name used in the file
    actual_col_name = df.columns[lower_cols.index(city_column_name.lower())]

    # Create a standardized list for matching (lowercase, stripped of whitespace)
    target_clean = [city.lower().strip() for city in target_cities]

    # Filter the dataframe
    # convert the column to string, lowercase it, and strip whitespace before checking
    filtered_df = df[df[actual_col_name].astype(str).str.lower().str.strip().isin(target_clean)]

    # ---------------------------------------------------------
    # 4. SAVE RESULT
    # ---------------------------------------------------------
    output_filename = "filtered_cities_output.csv"
    filtered_df.to_csv(output_filename, index=False)

    print("-" * 30)
    print("FILTERING COMPLETE")
    print("-" * 30)
    print(f"Original rows: {len(df)}")
    print(f"Filtered rows: {len(filtered_df)}")
    print(f"Saved to:      {output_filename}")
    
    # Preview
    if not filtered_df.empty:
        print("\nPreview of filtered cities found:")
        print(filtered_df[actual_col_name].unique())
    else:
        print("\nWarning: The filtered result is empty. No matching cities were found.")

if __name__ == "__main__":
    # Check if a path was provided via command line
    if len(sys.argv) > 1:
        input_path = sys.argv[1]
        filter_csv(input_path)
    else:
        # If not running from command line, you can manually paste the path here
        print("No file path provided.")
        #  manual_path = input("Please paste the path to your CSV file: ").strip().strip('"').strip("'")
        manual_path = "merged_cost_of_living_data.csv"  # Example default path
        filter_csv(manual_path)