import sys
import argparse
import pandas as pd
import glob

# Shared metadata columns used to join samples
META_COLS = [
    "ID",
    "ID-PWDNA",
    "First Author Last name",
    "Publication Year",
    "MOLEC_METHOD",
]

def merge_gene_count_files(input_files, output_file="merged_gene_matrix.tsv"):
    dfs = []

    for file_path in input_files:
        # Detect delimiter (tab vs comma)
        with open(file_path, "r", encoding="utf-8") as f:
            first_line = f.readline()
            sep = "\t" if "\t" in first_line else ","

        # Read CSV/TSV
        df = pd.read_csv(file_path, sep=sep)

        # Convert metadata columns to string type
        for col in META_COLS:
            if col in df.columns:
                df[col] = df[col].astype(str)

        dfs.append(df)

    if not dfs:
        print("Error: No input files provided.")
        return

    # Outer join on common metadata columns
    merged_df = dfs[0]
    for df in dfs[1:]:
        join_keys = [c for c in META_COLS if c in merged_df.columns and c in df.columns]
        merged_df = pd.merge(merged_df, df, on=join_keys, how="outer")

    # Identify gene columns and fill missing values with 0
    gene_cols = [c for c in merged_df.columns if c not in META_COLS]
    for g in gene_cols:
        merged_df[g] = merged_df[g].fillna(0).astype(int)

    # Save output matrix
    out_sep = "\t" if output_file.endswith(".tsv") else ","
    merged_df.to_csv(output_file, sep=out_sep, index=False)
    print(f"Successfully merged {len(input_files)} files into '{output_file}' with {len(merged_df)} total rows.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Merge multiple gene count tables into a single matrix."
    )
    parser.add_argument(
        "-i",
        "--input",
        nargs="+",
        required=True,
        help="Input CSV/TSV files (space-separated or wildcard)",
    )
    parser.add_argument(
        "-o",
        "--output",
        default="merged_gene_matrix.tsv",
        help="Output filename (default: merged_gene_matrix.tsv)",
    )

    args = parser.parse_args()

    # Expand any wildcard paths (e.g. 'gene_table/*.csv') for Windows compatibility
    expanded_files = []
    for item in args.input:
        matched = glob.glob(item)
        if matched:
            expanded_files.extend(matched)
        else:
            # If no wildcard matches, keep original path so errors are handled properly
            expanded_files.append(item)

    merge_gene_count_files(expanded_files, args.output)