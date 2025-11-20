import argparse
from dataclasses import dataclass, asdict
import subprocess
import tomllib
import tomli_w
import datetime
import os
from typing import Any, Dict, Generator, List, TextIO
import re

def git_hash() -> str:
    return subprocess.check_output(["git", "rev-parse", "--short", "HEAD"], encoding="utf-8")

def file_timestamp(path) -> str:
    seconds_since_epoch: float = os.path.getmtime(path)
    dt = datetime.datetime.fromtimestamp(seconds_since_epoch, datetime.timezone.utc)
    return dt.isoformat(sep="_", timespec="seconds")

def file_line_regex_matches(f: TextIO, r: re.Pattern) -> Generator:
    for line in f:
        m = r.match(line)
        if m:
            yield m

def file_line_one_regex_match(f: TextIO, r: re.Pattern) -> re.Match:
    m = None
    for found_m in file_line_regex_matches(f, r):
        if m is None:
            m = found_m
        else:
            raise RuntimeError(f"Found multiple matches for regex {r} in {f}")
    if m is None:
        raise RuntimeError(f"Found no match for regex {r} in {f}")
    return m

@dataclass
class LatencyStats:
    dut: str
    timestamp: str

    aw_mean_latency_0cav_4flit: int
    aw_mean_latency_1cav_4flit: int
    aw_mean_latency_2cav_4flit: int
    aw_throughput_0cav_1flit: int
    aw_throughput_1cav_1flit: int
    aw_throughput_2cav_1flit: int

# group 1 = project name
PROJECT_NAME_PATTERN = re.compile(r'^project_name := "(\w+)"')
# group 1 = DUT name
DUT_PATTERN = re.compile(r'^dut := "(\w+)"')
# group 1 = Fmax
# group 2 = Restricted Fmax
FMAX_PATTERN = re.compile(r'; (\d+\.\d\d) MHz\s+; (\d+\.\d\d) MHz\s+;\s+CLK_FAST\s+;\s+;\s+.+ Model\s+;')
# group 1 = ALMs needed
# group 2 = total ALMs on device
LUTS_PATTERN = re.compile(r'^; Logic utilization \(ALMs needed / total ALMs on device\)\s+; ([\d,]+)\s+/\s+([\d,]+)')

def project_stats(dut: str) -> LatencyStats:
    results_toml = os.path.join("results", f"{dut}.toml")
    results_timestamp = file_timestamp(results_toml)

    with open(results_toml, "rb") as f:
        results = tomllib.load(f)
    
    aw_mean_latency_4flit = {}
    aw_throughput_1flit = {}

    for cavs in range(3):
        aw_mean_latency_4flit[cavs] = results["tests"][f"Stream of 10000 librust random valid Cap2024_11 {cavs}-caveat 4-flit transactions"]["aw_aw_latency_mean"]
        aw_throughput_1flit[cavs] = results["tests"][f"Stream of 10000 librust random valid Cap2024_11 {cavs}-caveat 1-flit transactions"]["aw_throughput"]

    return LatencyStats(
        dut=dut,
        timestamp=results_timestamp,

        aw_mean_latency_0cav_4flit = aw_mean_latency_4flit[0],
        aw_mean_latency_1cav_4flit = aw_mean_latency_4flit[1],
        aw_mean_latency_2cav_4flit = aw_mean_latency_4flit[2],
        aw_throughput_0cav_1flit = aw_throughput_1flit[0],
        aw_throughput_1cav_1flit = aw_throughput_1flit[1],
        aw_throughput_2cav_1flit = aw_throughput_1flit[2],
    )

RELEVANT_PROJECTS = {
    # "single_checker_1per": "mkSingleChecker3_1percycle",
    # "single_checker_2per": "mkSingleChecker3_2percycle",
    "full_exposer_0checkers_1per": "mkCombinedIOCapExposerV6_0pool_1percycle_KeyManager2V1_Tb"
}
RELEVANT_PROJECTS.update({
    f"full_exposer_{n}checkers_{p}per": f"mkCombinedIOCapExposerV6_blockinvalid_{n}pool_{p}percycle_KeyManager2V1_Tb"
    for (n, p) in (
        (1, 1),
        (2, 1),
        (3, 1),
        (4, 1),
        (4, 2),
    )
})

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("results_dir", type=str, help="Directory of historical result files, gets a new file added with the current timestamp on the name")
    parser.add_argument("results_file", type=str, help="Most up-to-date results file location outside of {results_dir}, gets overwritten with the same TOML")

    args = parser.parse_args()

    cur_timestamp = datetime.datetime.now(datetime.timezone.utc).isoformat(sep="_", timespec="seconds")

    toml: Dict[str, Any] = {
        "timestamp": cur_timestamp,
        "generated_by": "gen_report.py",
        "git_hash": git_hash(),
    }
    for (project_shortname, dut) in RELEVANT_PROJECTS.items():
        toml[project_shortname] = asdict(project_stats(dut))
    
    with open(os.path.join(args.results_dir, f"hardware_latency_{cur_timestamp}.toml"), "wb") as f:
        tomli_w.dump(toml, f)
    with open(args.results_file, "wb") as f:
        tomli_w.dump(toml, f)