import sys
import os
import re
from collections import defaultdict

def process_gene_pipeline(input_file, output_file):
    sample_genes = defaultdict(set)
    extracted_count = 0
    malformed_count = 0

    print("🚀 Starting pipeline with robust encoding handling...")

    # Try reading with utf-8 first, fall back to utf-16 if bytes are weird
    encoding_to_try = 'utf-8'
    try:
        with open(input_file, 'r', encoding='utf-8') as f:
            f.readline()
    except UnicodeDecodeError:
        encoding_to_try = 'utf-16'
        print("⚠️ UTF-8 decoding failed. Switching to UTF-16 encoding.")

    with open(input_file, 'r', encoding=encoding_to_try, errors='ignore') as f:
        for line in f:
            # Skip comments or empty lines
            if line.startswith('#') or not line.strip():
                continue

            parts = line.strip().split()
            # HMMER domtblout has at least 4 essential columns up to the query name
            if len(parts) < 4:
                malformed_count += 1
                continue  

            target_name = parts[0]
            gene_name = parts[3]

            # Clean target name ':' prefix if present
            if ':' in target_name:
                sample_name = target_name.split(':', 1)[1]
            else:
                sample_name = target_name

            # Strip RAST suffix
            cleaned_sample = re.sub(r'\.RAST\.CDS\.\d+$', '', sample_name)

            sample_genes[cleaned_sample].add(gene_name)
            extracted_count += 1

    print(f"📊 Extracted and cleaned {extracted_count} hits.")
    if malformed_count > 0:
        print(f"⚠️ Skipped {malformed_count} short/malformed lines.")

    if extracted_count == 0:
        print("❌ Error: 0 hits extracted! Check if your file encoding or column structure matches.")
        sys.exit(1)

    print("🔄 Pivoting data and writing output...")
    all_samples = sorted(sample_genes.keys())
    all_genes = sorted({gene for genes in sample_genes.values() for gene in genes})

    with open(output_file, 'w', encoding='utf-8') as outfile:
        for sample in all_samples:
            gene_hits = [gene for gene in all_genes if gene in sample_genes[sample]]
            outfile.write(f"{sample}\t" + "\t".join(gene_hits) + "\n")

    print(f"✅ Pipeline complete! Output written to: {output_file}")

def main():
    if len(sys.argv) != 3:
        print("Usage: python pipeline_gene_hits.py <raw_input_file.txt> <final_output_file.txt>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    if not os.path.isfile(input_file):
        print(f"❌ Error: File not found -> {input_file}")
        sys.exit(1)

    process_gene_pipeline(input_file, output_file)

if __name__ == "__main__":
    main()