# Read lines from stdin
import os
import sys
import re
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("directory")
parser.add_argument("--pattern", default="", help="Pattern the test names have to match to be recorded", required=False)
args = parser.parse_args()

os.makedirs(args.directory, exist_ok=True)

konata_test_name = re.compile("Kanata\t0004(\s+\/\/\s+(.+))?")
# Strip unnecessary whitespace from numbers
num_strip = re.compile(r"([RW]#|<|,)[ ]*(\d+)")
arg_pattern = re.compile(args.pattern) if args.pattern else None

kanata_n = 0
kanata_file = None

for line in sys.stdin:
    if line.startswith("Kanata\t0004"):
        kanata_n += 1
        if (kanata_file is not None):
            kanata_file.close()

        # Pattern match on the name
        if arg_pattern:
            # Extract the name and test against arg_pattern
            m = konata_test_name.match(line)
            # There must a) be a name and b) it must match
            if not (m and m.group(2) and (arg_pattern.search(m.group(2)))):
                # If any of those conditions don't hold, don't record this test
                print("SKIPPING KONATA")
                kanata_file = None
                continue

        # We have not cancelled the recording of this test, so open a new file and record it
        print("CAPTURING KONATA")
        kanata_file = open(os.path.join(args.directory, f"kanata{kanata_n}.txt"), "w")
    if kanata_file:
        kanata_file.write(num_strip.sub(lambda match: f"{match.group(1)}{match.group(2)}", line))

if (kanata_file is not None):
    kanata_file.close()