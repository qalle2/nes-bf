# read pattern table data from text file and print it in ASM6 format

def generate_lines():
    # generate non-empty non-comment lines from a text file
    with open("pt-data.txt", "rt", encoding="ascii") as handle:
        handle.seek(0)
        for line in handle:
            line = line.strip()
            if line and not line.startswith(";"):
                yield line

def generate_bytes():
    # generate lines from generate_lines() as bytes
    for line in generate_lines():
        if len(line) != 8:
            sys.exit("Error: line length is not 8.")
        if line.count(".") + line.count("O") < 8:
            sys.exit("Error: line contains forbidden characters.")
        yield int(line.replace(".", "0").replace("O", "1"), 2)

def main():
    # read text file as bytes
    ptBytes = bytearray(generate_bytes())
    if len(ptBytes) % 8:
        sys.exit("Error: number of bytes is not a multiple of 8.")

    # print bytes in ASM6 format
    for i in range(0, len(ptBytes), 8):
        print(
            16 * " " + "hex "
            + " ".join(format(b, "02x") for b in ptBytes[i:i+8])
            + f"  ; tile ${i//8+0x20:02x}"
        )

main()
