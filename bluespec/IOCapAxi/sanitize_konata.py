# Read lines from stdin
import sys

kanata_n = 0
kanata_file = None

for line in sys.stdin:
    if line == "Kanata\t0004\n":
        if (kanata_file is not None):
            kanata_file.close()
        kanata_file = open(f"kanata{kanata_n}.txt", "w")
        kanata_n += 1
    if kanata_file:
        kanata_file.write(line)

if (kanata_file is not None):
    kanata_file.close()