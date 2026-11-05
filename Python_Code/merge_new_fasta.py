import argparse

def read_fasta(file_name):
    """Reads a FASTA file and returns a dictionary of headers and sequences."""
    sequences = {}
    with open(file_name, 'r') as file:
        header = ''
        sequence = ''
        for line in file:
            line = line.strip()
            if line.startswith('>'):  # header line
                if header:
                    sequences[header] = sequence
                header = line
                sequence = ''
            else:  # sequence line
                sequence += line
        if header:  # Don't forget to add the last sequence
            sequences[header] = sequence
    return sequences

def write_fasta(sequences, file_name):
    """Writes the dictionary of headers and sequences to a FASTA file."""
    with open(file_name, 'w') as file:
        for header, sequence in sequences.items():
            file.write(f"{header}\n{sequence}\n")

def merge_fasta(file1, file2, output_file):
    """Merges two FASTA files and renames headers with suffixes for duplicates."""
    # Read the FASTA files into dictionaries
    sequences1 = read_fasta(file1)
    sequences2 = read_fasta(file2)

    # Merge the sequences and handle duplicate headers
    merged_sequences = sequences1.copy()
    header_count = {}  # Track the count of headers to append suffixes

    for header, sequence in sequences2.items():
        if header in merged_sequences:
            # If the header already exists, we need to add a suffix
            if header not in header_count:
                # First duplicate, add _a
                header_count[header] = 1
                new_header = f"{header}_a"
            else:
                # For subsequent duplicates, increment the count and add suffix
                header_count[header] += 1
                suffix = chr(96 + header_count[header])  # 'b' for 2, 'c' for 3, etc.
                new_header = f"{header}_{suffix}"

            # Add the new header and sequence to merged_sequences
            merged_sequences[new_header] = sequence
        else:
            # If the header does not exist, just add it
            merged_sequences[header] = sequence

    # Write the merged sequences to the output file
    write_fasta(merged_sequences, output_file)

    print(f"Merging completed. The merged file is saved as {output_file}.")

if __name__ == "__main__":
    # Set up argument parsing
    parser = argparse.ArgumentParser(description="Merge two FASTA files while handling duplicate headers.")
    parser.add_argument("file1", help="Path to the first FASTA file.")
    parser.add_argument("file2", help="Path to the second FASTA file.")
    parser.add_argument("output", help="Path to the output merged FASTA file.")

    # Parse the command-line arguments
    args = parser.parse_args()

    # Run the merge_fasta function with the provided arguments
    merge_fasta(args.file1, args.file2, args.output)