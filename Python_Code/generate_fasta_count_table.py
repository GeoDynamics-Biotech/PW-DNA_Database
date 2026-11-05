import sys
import pandas as pd
from Bio import SeqIO

def process_bioinformatics_data(fasta_input, sample_txt, fasta_output, csv_output):
    # --- TASK 1: Clean FASTA Headers ---
    cleaned_records = []
    sequence_ids = []

    print(f"Reading {fasta_input} and cleaning headers...")
    try:
        for record in SeqIO.parse(fasta_input, "fasta"):
            # Split by '.' or ' ' and take the first part (Accession Number)
            clean_id = record.description.split('.')[0].split(' ')[1] if ' ' in record.description and record.description.startswith('>') else record.description.split('.')[0].split(' ')[0]
            
            # Simple fallback for standard Accession.Version format:
            clean_id = record.id.split('.')[0]
            
            record.id = clean_id
            record.description = "" 
            
            cleaned_records.append(record)
            sequence_ids.append(clean_id)

        # Save the cleaned FASTA
        SeqIO.write(cleaned_records, fasta_output, "fasta")
        print(f"Success: Cleaned FASTA saved to '{fasta_output}'")

        # --- TASK 2: Generate Count Table ---
        print(f"Reading {sample_txt} and generating count table...")
        
        with open(sample_txt, 'r') as f:
            samples = [line.strip() for line in f if line.strip()]

        # Create the matrix (Rows = Sequence IDs, Columns = Samples)
        df = pd.DataFrame(1, index=sequence_ids, columns=samples)
        df.index.name = 'id'
        df.reset_index(inplace=True)

        # Save to CSV
        df.to_csv(csv_output, index=False)
        print(f"Success: Count table saved to '{csv_output}'")

    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    # Check if the correct number of arguments are provided
    if len(sys.argv) != 5:
        print("Usage: python generate_fasta_count_table.py <input.fasta> <samples.txt> <output.fasta> <output_count_table.csv>")
    else:
        # Assign arguments to variables
        input_f = sys.argv[1]
        samples_t = sys.argv[2]
        output_f = sys.argv[3]
        output_c = sys.argv[4]
        
        process_bioinformatics_data(input_f, samples_t, output_f, output_c)