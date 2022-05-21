import sys, os

if len(sys.argv) != 3:
    sys.exit("Args: FILE SIZE")

(filename, size) = (sys.argv[1], int(sys.argv[2], 10))

with open(filename, "rb") as handle:
    handle.seek(-size, os.SEEK_END)
    data = handle.read()

print(f'Last {size} bytes of "{filename}":')
print()

for i in range(0, len(data), 32):
    print(data[i:i+32].hex())
