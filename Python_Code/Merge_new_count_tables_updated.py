import pandas as pd
import argparse

def merge_csv_files(file1, file2, output_file):
    # Read the two CSV files into pandas DataFrames
    df1 = pd.read_csv(file1)
    df2 = pd.read_csv(file2)
    
    # Create a combined list of ids from both files
    all_ids = list(df1['id']) + list(df2['id'])
    
    # Create a dictionary to track occurrences of each ID
    id_count = {}
    
    # Function to handle renaming of duplicate ids
    def rename_id(id):
        if id in id_count:
            id_count[id] += 1
            return f"{id}_{chr(ord('a') + id_count[id] - 1)}"  # Add suffix (_a, _b, _c, etc.)
        else:
            id_count[id] = 0  # First occurrence, no suffix
            return id
    
    # Apply the renaming function to the 'id' columns in both DataFrames
    df1['id'] = df1['id'].apply(rename_id)
    df2['id'] = df2['id'].apply(rename_id)
    
    # Merge the two DataFrames on the 'id' column (outer join)
    merged_df = pd.merge(df1, df2, on='id', how='outer')
    
    # Replace NaN values with 0 in the merged DataFrame
    merged_df = merged_df.fillna(0)
    
    # Convert numeric columns to integers to remove decimals
    for col in merged_df.columns:
        if merged_df[col].dtype == 'float64':  # Check if the column is numeric
            merged_df[col] = merged_df[col].astype(int)
    
    # Save the merged DataFrame to a new CSV file
    merged_df.to_csv(output_file, index=False)
    
    print(f"Merged CSV file saved to: {output_file}")

# Set up command-line argument parsing
def parse_args():
    parser = argparse.ArgumentParser(description="Merge two CSV files and rename overlapping IDs")
    parser.add_argument("file1", help="Path to the first CSV file")
    parser.add_argument("file2", help="Path to the second CSV file")
    parser.add_argument("output", help="Path to the output CSV file")
    return parser.parse_args()

# Main function to handle merging
if __name__ == "__main__":
    args = parse_args()
    merge_csv_files(args.file1, args.file2, args.output)