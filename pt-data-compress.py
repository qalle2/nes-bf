# read pattern table data from text file and print it in ASM6 format

import collections, sys

def generate_lines():
    # generate non-empty non-comment lines from a text file
    with open("pt-data.txt", "rt", encoding="ascii") as handle:
        handle.seek(0)
        for line in handle:
            line = line.strip()
            if line and not line.startswith(";"):
                yield line

def generate_bytes():
    # generate lines from generate_lines() as 8-bit ints
    for line in generate_lines():
        if len(line) != 8:
            sys.exit("Error: line length is not 8.")
        if line.count(".") + line.count("O") < 8:
            sys.exit("Error: line contains forbidden characters.")
        yield int(line.replace(".", "0").replace("O", "1"), 2)

def compress_tile(tileBytes):
    # encode 8 bytes into 2-9 bytes:
    # - 1 byte:    in which byte positions does the most common byte occur
    #              (0 = end of data)
    # - 1 byte:    the most common byte
    # - 0-7 bytes: other bytes in order, with duplicates
    # this algorithm is from Nintendo's Zelda: Oracle of Seasons/Ages; see
    # https://www.nesdev.org/wiki/Tile_compression

    byteCounts = collections.Counter(tileBytes)
    # how many times do the most common bytes occur
    maxCount = byteCounts.most_common(1)[0][1]
    # what is the first of them
    commonestByte = [b for b in tileBytes if byteCounts[b] == maxCount][0]
    # in which byte positions does it occur
    commonestPositions = [i for (i, b) in enumerate(tileBytes) if b == commonestByte]
    # what are the other bytes (in order, with duplicates)
    otherBytes = [tileBytes[i] for i in range(8) if i not in commonestPositions]
    # encode as bytes
    return bytes(
        [sum(1 << (7 - p) for p in commonestPositions), commonestByte] + otherBytes
    )

def main():
    # read text file as bytes
    ptBytes = bytearray(generate_bytes())
    if len(ptBytes) % 8:
        sys.exit("Error: number of bytes is not a multiple of 8.")

    # compress tiles and print them in ASM6 format
    totalSize = 0
    for i in range(0, len(ptBytes), 8):
        compressed = compress_tile(ptBytes[i:i+8])
        totalSize += len(compressed)
        print(16 * " " + f"hex {compressed.hex():18}  ; tile ${i//8+0x20:02x}")

    print(16 * " " + f"hex {'00':18}  ; end of data")
    print("; total size:", totalSize + 1)

main()
