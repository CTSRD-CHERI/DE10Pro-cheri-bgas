# Read lines from stdin
import sys
import re

replace = re.compile(r"([RW]#)[ ]*(\d+)")

kanata_n = 0
kanata_file = None

for line in sys.stdin:
    if line == "Kanata\t0004\n":
        if (kanata_file is not None):
            kanata_file.close()
        kanata_file = open(f"kanata{kanata_n}.txt", "w")
        kanata_n += 1
    if kanata_file:
        kanata_file.write(replace.sub(lambda match: f"{match.group(1)}{match.group(2)}", line))

if (kanata_file is not None):
    kanata_file.close()