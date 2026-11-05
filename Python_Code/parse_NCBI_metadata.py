import re
import sys
from collections import OrderedDict


def parse_samples(text):
    # Split on lines like "1:", "2:", etc.
    blocks = re.split(r'\n(?=\d+: )', text.strip())
    samples = []

    for block in blocks:
        lines = block.splitlines()

        # First line: "1: Sample title"
        header_match = re.match(r'\d+:\s*(.*)', lines[0])
        sample_title = header_match.group(1).strip()

        metadata = OrderedDict()
        metadata["Sample title"] = sample_title

        current_section = None

        for line in lines[1:]:
            line = line.rstrip()

            if not line:
                continue

            # Section headers (e.g. "Attributes:")
            if re.match(r'^[A-Za-z ].*:$', line):
                current_section = line[:-1]
                metadata[current_section] = ""
                continue

            # Attribute lines like: /strain="C57BL/6J"
            attr_match = re.match(r'\s*/([^=]+)="?(.*?)"?$', line)
            if attr_match:
                key = f"/{attr_match.group(1).strip()}"
                value = attr_match.group(2).strip()
                metadata[key] = value
                continue

            # Key-value lines like: "Organism: Mus musculus"
            if ":" in line and current_section is None:
                key, value = line.split(":", 1)
                metadata[key.strip()] = value.strip()
            else:
                # Append multiline section content
                if current_section:
                    metadata[current_section] = (
                        metadata[current_section] + " " + line
                    ).strip()

        samples.append(metadata)

    return samples


def write_table(samples, output_file):
    # Collect all metadata keys across samples
    all_keys = OrderedDict()
    for sample in samples:
        for key in sample:
            all_keys.setdefault(key, None)

    with open(output_file, "w", encoding="utf-8") as out:
        # Header row
        header = ["Metadata"] + [s["Sample title"] for s in samples]
        out.write("\t".join(header) + "\n")

        # Data rows
        for key in all_keys:
            row = [key]
            for sample in samples:
                row.append(sample.get(key, ""))
            out.write("\t".join(row) + "\n")


def main():
    if len(sys.argv) != 3:
        print("Usage: python parse_NCBI_metadata.py <input.txt> <output.txt>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    with open(input_file, "r", encoding="utf-8") as f:
        text = f.read()

    samples = parse_samples(text)
    write_table(samples, output_file)

    print(f"Parsed {len(samples)} samples")
    print(f"Output written to: {output_file}")


if __name__ == "__main__":
    main()
