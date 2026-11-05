import sys
import random

# Function to generate randomized quality scores between 20 and 35 (Phred+33 V2 encoding)
def generate_random_quality_scores(sequence_length):
    # Generate random quality scores between 20 and 35
    return ''.join(chr(random.randint(53, 68)) for _ in range(sequence_length))

# Function to convert FASTA to FASTQ
def fasta_to_fastq(fasta_filename, fastq_filename):
    with open(fasta_filename, 'r') as fasta_file, open(fastq_filename, 'w') as fastq_file:
        # Read the lines from the FASTA file
        lines = fasta_file.readlines()
        
        # Initialize variables
        sequence = ''
        header = ''
        
        for line in lines:
            # Skip empty lines or comments
            line = line.strip()
            if not line:
                continue
            
            # Check if the line is a header (starts with '>')
            if line.startswith('>'):
                # If we were previously reading a sequence, write it to the FASTQ file
                if sequence:
                    # Generate random quality scores for the current sequence
                    quality_scores = generate_random_quality_scores(len(sequence))
                    
                    # Write to the FASTQ file
                    fastq_file.write(f'@{header}\n')
                    fastq_file.write(f'{sequence}\n')
                    fastq_file.write(f'+\n')
                    fastq_file.write(f'{quality_scores}\n')
                    
                # Update the header and reset the sequence for the next one
                header = line[1:]  # Remove '>' from the header
                sequence = ''
            else:
                # Otherwise, accumulate the sequence data
                sequence += line
        
        # After the loop, write the last sequence to the FASTQ file
        if sequence:
            quality_scores = generate_random_quality_scores(len(sequence))
            fastq_file.write(f'@{header}\n')
            fastq_file.write(f'{sequence}\n')
            fastq_file.write(f'+\n')
            fastq_file.write(f'{quality_scores}\n')

# Check if correct number of arguments are provided
if len(sys.argv) != 3:
    print("Usage: python fasta_to_fastq.py <input_fasta_file> <output_fastq_file>")
    sys.exit(1)

# Get input and output filenames from command line arguments
fasta_filename = sys.argv[1]
fastq_filename = sys.argv[2]

# Call the function to convert FASTA to FASTQ
fasta_to_fastq(fasta_filename, fastq_filename)