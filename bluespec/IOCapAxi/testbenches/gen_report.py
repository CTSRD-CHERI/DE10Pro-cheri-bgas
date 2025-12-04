import argparse
from collections import defaultdict
from dataclasses import dataclass, asdict
from math import nan
import math
from statistics import median
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

def all_eq_excl_nan(xs):
    xs = [
        x
        for x in xs
        if not math.isnan(x)
    ]
    if len(xs) == 0:
        print("warning: found all-nan list")
        return True
    assert f"all were not equal: {xs}", all(x == xs[0] for x in xs)
    return xs[0]

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

    # Not means, have to be the same throughout
    kmngr_aw_b_latency: int
    kmngr_ar_r_latency: int

    upload_no_dma_status_latency: int
    upload_no_dma_debug_enablekey_latency: int
    upload_no_dma_debug_state_valid_latency: int
    upload_under_dma_status_latency: int
    upload_under_dma_debug_enablekey_latency: int
    upload_under_dma_debug_state_valid_latency: int

    revoke_no_dma_killkey_latency: int
    revoke_no_dma_state_invalidnotrevoked_latency: int
    revoke_no_dma_state_invalid_latency: int

    revoke_under_dma_killkey_latency: int
    revoke_under_dma_state_invalidnotrevoked_latency: int
    revoke_under_dma_4flits_debug_invalid_latency: List[int]
    revoke_under_dma_8flits_debug_invalid_latency: List[int]
    revoke_under_dma_12flits_debug_invalid_latency: List[int]
    revoke_under_dma_16flits_debug_invalid_latency: List[int]
    revoke_under_dma_20flits_debug_invalid_latency: List[int]
    revoke_under_dma_24flits_debug_invalid_latency: List[int]

    n_revokes_per_cycle_rolling: int
    n_uploads_per_cycle_rolling: int
    n_revokes_per_cycle_alone: int
    n_uploads_per_cycle_alone: int

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

def project_stats(results_toml: str, dut: str) -> LatencyStats:
    results_timestamp = file_timestamp(results_toml)

    with open(results_toml, "rb") as f:
        results = tomllib.load(f)
    
    aw_mean_latency_4flit = {}
    aw_throughput_1flit = {}

    for cavs in range(3):
        aw_mean_latency_4flit[cavs] = results["tests"][f"Stream of 10000 librust random valid Cap2024_11 {cavs}-caveat 4-flit transactions"]["aw_aw_latency_mean"]
        aw_throughput_1flit[cavs] = results["tests"][f"Stream of 10000 librust random valid Cap2024_11 {cavs}-caveat 1-flit transactions"]["aw_throughput"]

    # Keymngr stats
    kmngr_aw_b_latencies = [
        test["keymngr_aw_b_latency_mean"]
        for name, test in results["tests"].items()
    ]
    kmngr_ar_r_latencies = [
        test["keymngr_ar_r_latency_mean"]
        for name, test in results["tests"].items()
    ]

    # Upload stats
    # upload_mmio_status_latency_mean = 20
    # upload_mmio_debug_enablekey_latency_mean = 30
    # upload_mmio_debug_state_valid_latency_mean = 40

    # 1-flit 0-cav
    # 16-flit 0-cav
    # 16-flit 1-cav
    # 16-flit 2-cav
    # "UVMUploadOverMMIOBenchmark (Stream of 100 1-flit 0-cav txns, DMA 0 Upload 1, delay 0)"
    upload_no_dma_status_latency: List[int] = []
    upload_no_dma_debug_enablekey_latency: List[int] = []
    upload_no_dma_debug_state_valid_latency: List[int] = []

    for (cav, n_flits) in (
        (0, 1),
        (0, 16),
        (1, 16),
        (2, 16),
    ):
        test = results["tests"][f"UVMUploadOverMMIOBenchmark (Stream of 100 {n_flits}-flit {cav}-cav txns, DMA 0 Upload 1, delay 0)"]
        upload_no_dma_status_latency.append(
            test["upload_mmio_status_latency_mean"]
        )
        upload_no_dma_debug_enablekey_latency.append(
            test["upload_mmio_debug_enablekey_latency_mean"]
        )
        upload_no_dma_debug_state_valid_latency.append(
            test["upload_mmio_debug_state_valid_latency_mean"]
        )

    # 1-flit 0-cav
    # 16-flit 0-cav
    # 16-flit 1-cav
    # 16-flit 2-cav
    # "UVMUploadOverMMIOBenchmark (Stream of 100 16-flit 0-cav txns, DMA 0 Upload 0, delay 0)"
    upload_under_dma_status_latency: List[int] = []
    upload_under_dma_debug_enablekey_latency: List[int] = []
    upload_under_dma_debug_state_valid_latency: List[int] = []

    for (cav, n_flits) in (
        (0, 4),
        (1, 4),
        (2, 4),
    ):
        test = results["tests"][f"UVMUploadOverMMIOBenchmark (Stream of 100 {n_flits}-flit {cav}-cav txns, DMA 0 Upload 0, delay 0)"]
        upload_under_dma_status_latency.append(
            test["upload_mmio_status_latency_mean"]
        )
        upload_under_dma_debug_enablekey_latency.append(
            test["upload_mmio_debug_enablekey_latency_mean"]
        )
        upload_under_dma_debug_state_valid_latency.append(
            test["upload_mmio_debug_state_valid_latency_mean"]
        )

    # Revoke stats
    # revoke_mmio_debug_killkey_latency_mean = 20
    # revoke_mmio_debug_state_invalidnotrevoked_latency_mean = 20
    # revoke_mmio_debug_state_invalid_latency_mean = 2820

    # 1-flit 0-cav
    # 16-flit 0-cav
    # 16-flit 1-cav
    # 16-flit 2-cav
    # "UVMRevokeOverMMIOBenchmark (Stream of 100 16-flit 0-cav txns, DMA 0 Revoke 1, delay 0)"
    revoke_no_dma_debug_killkey_latency: List[int] = []
    revoke_no_dma_debug_state_invalidnotrevoked_latency: List[int] = []
    revoke_no_dma_debug_state_invalid_latency: List[int] = []

    for (cav, n_flits) in (
        (0, 1),
        (0, 16),
        (1, 16),
        (2, 16),
    ):
        test = results["tests"][f"UVMRevokeOverMMIOBenchmark (Stream of 100 {n_flits}-flit {cav}-cav txns, DMA 0 Revoke 1, delay 0)"]
        revoke_no_dma_debug_killkey_latency.append(
            test["revoke_mmio_debug_killkey_latency_mean"]
        )
        revoke_no_dma_debug_state_invalidnotrevoked_latency.append(
            test["revoke_mmio_debug_state_invalidnotrevoked_latency_mean"]
        )
        revoke_no_dma_debug_state_invalid_latency.append(
            test["revoke_mmio_debug_state_invalid_latency_mean"]
        )


    test_invalid_latency_with_diff_caps = []
    for (cav, n_flits) in (
        (0, 4),
        (1, 4),
        (2, 4),
    ):
        delay = 0
        test = results["tests"][f"UVMRevokeOverMMIOBenchmark (Stream of 100 {n_flits}-flit {cavs}-cav txns, DMA 0 Revoke 0, delay {delay})"]
        test_invalid_latency_with_diff_caps.append(
            test["revoke_mmio_debug_state_invalid_latency_mean"]
        )

    # check that cavs doesn't change anything
    print(dut)
    all_eq_excl_nan(test_invalid_latency_with_diff_caps)

    # [n_flits][delay/10]
    revoke_under_dma_debug_killkey_latency = []
    revoke_under_dma_state_debug_invalidnotrevoked_latency = []
    revoke_under_dma_state_debug_invalid_latency: Dict[str, List[int]] = defaultdict(list)
    # revoke_under_dma_0cav_max_data_flits = defaultdict(list)

    for n_flits in range(4, 28, 4):
        for delay in range(0, 60, 10):
            cavs = 0
            if True:
                test = results["tests"][f"UVMRevokeOverMMIOBenchmark (Stream of 100 {n_flits}-flit {cavs}-cav txns, DMA 0 Revoke 0, delay {delay})"]
                revoke_under_dma_debug_killkey_latency.append(
                    test["revoke_mmio_debug_killkey_latency_mean"]
                )
                revoke_under_dma_state_debug_invalidnotrevoked_latency.append(
                    test["revoke_mmio_debug_state_invalidnotrevoked_latency_mean"]
                )
                revoke_under_dma_state_debug_invalid_latency[str(n_flits)].append(
                    test["revoke_mmio_debug_state_invalid_latency_mean"]
                )

    latency_stats = lambda xs: [min(xs), median(xs), max(xs)]

    # rolling revoke
    rolling = results["tests"]["UVMRollingUploadRevokeMMIOBenchmark (1000 revokes-and-uploads, revoke after 20, around mask 0xff, 0-flit dma stream on key -1)"]
    n_revokes_per_cycle_rolling = rolling["n_revokes_per_cycle"]
    n_uploads_per_cycle_rolling = rolling["n_uploads_per_cycle"]

    upload_then_revoke = results["tests"]["UVMRollingUploadRevokeMMIOBenchmark (256 revokes-and-uploads, revoke after 256, around mask 0xff, 0-flit dma stream on key -1)"]
    n_revokes_per_cycle_alone = upload_then_revoke["n_revokes_per_cycle"]
    n_uploads_per_cycle_alone = rolling["n_uploads_per_cycle"]

    return LatencyStats(
        dut=dut,
        timestamp=results_timestamp,

        aw_mean_latency_0cav_4flit = aw_mean_latency_4flit[0],
        aw_mean_latency_1cav_4flit = aw_mean_latency_4flit[1],
        aw_mean_latency_2cav_4flit = aw_mean_latency_4flit[2],
        aw_throughput_0cav_1flit = aw_throughput_1flit[0],
        aw_throughput_1cav_1flit = aw_throughput_1flit[1],
        aw_throughput_2cav_1flit = aw_throughput_1flit[2],

        kmngr_aw_b_latency=all_eq_excl_nan(kmngr_aw_b_latencies),
        kmngr_ar_r_latency=all_eq_excl_nan(kmngr_ar_r_latencies),

        upload_no_dma_status_latency=all_eq_excl_nan(upload_no_dma_status_latency),
        upload_no_dma_debug_enablekey_latency=all_eq_excl_nan(upload_no_dma_debug_enablekey_latency),
        upload_no_dma_debug_state_valid_latency=all_eq_excl_nan(upload_no_dma_debug_state_valid_latency),
        upload_under_dma_status_latency=all_eq_excl_nan(upload_under_dma_status_latency),
        upload_under_dma_debug_enablekey_latency=all_eq_excl_nan(upload_under_dma_debug_enablekey_latency),
        upload_under_dma_debug_state_valid_latency=all_eq_excl_nan(upload_under_dma_debug_state_valid_latency),

        revoke_no_dma_killkey_latency=all_eq_excl_nan(revoke_no_dma_debug_killkey_latency),
        revoke_no_dma_state_invalidnotrevoked_latency=all_eq_excl_nan(revoke_no_dma_debug_state_invalidnotrevoked_latency),
        revoke_no_dma_state_invalid_latency=all_eq_excl_nan(revoke_no_dma_debug_state_invalid_latency),

        revoke_under_dma_killkey_latency=all_eq_excl_nan(revoke_under_dma_debug_killkey_latency),
        revoke_under_dma_state_invalidnotrevoked_latency=all_eq_excl_nan(revoke_under_dma_state_debug_invalidnotrevoked_latency),
        revoke_under_dma_4flits_debug_invalid_latency=latency_stats(revoke_under_dma_state_debug_invalid_latency["4"]),
        revoke_under_dma_8flits_debug_invalid_latency=latency_stats(revoke_under_dma_state_debug_invalid_latency["8"]),
        revoke_under_dma_12flits_debug_invalid_latency=latency_stats(revoke_under_dma_state_debug_invalid_latency["12"]),
        revoke_under_dma_16flits_debug_invalid_latency=latency_stats(revoke_under_dma_state_debug_invalid_latency["16"]),
        revoke_under_dma_20flits_debug_invalid_latency=latency_stats(revoke_under_dma_state_debug_invalid_latency["20"]),
        revoke_under_dma_24flits_debug_invalid_latency=latency_stats(revoke_under_dma_state_debug_invalid_latency["24"]),

        n_revokes_per_cycle_rolling=n_revokes_per_cycle_rolling,
        n_uploads_per_cycle_rolling=n_uploads_per_cycle_rolling,

        n_revokes_per_cycle_alone=n_revokes_per_cycle_alone,
        n_uploads_per_cycle_alone=n_uploads_per_cycle_alone,
    )

RELEVANT_PROJECTS = {
    # "single_checker_1per": "mkSingleChecker3_1percycle",
    # "single_checker_2per": "mkSingleChecker3_2percycle",
    "full_exposer_0checkers": "mkCombinedIOCapExposerV6_0pool_KeyManager2V1_64_Tb"
}
RELEVANT_PROJECTS.update({
    f"full_exposer_{n}checkers_{p}per": f"mkCombinedIOCapExposerV6_blockinvalid_{n}pool_{p}percycle_KeyManager2V1_Tb"
    for (n, p) in (
        (1, 1),
        (2, 1),
        (3, 1),
        (4, 1),
        (9, 1),
    )
})
RELEVANT_PROJECTS.update({
    f"full_exposer_{n}checkers_{p}per_64": f"mkCombinedIOCapExposerV6_blockinvalid_{n}pool_{p}percycle_KeyManager2V1_64_Tb"
    for (n, p) in (
        (1, 2),
        (2, 2),
        (3, 2),
        (4, 2),
        (5, 2),
    )
})

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("results_dir", type=str, help="Directory of historical result files, gets a new file added with the current timestamp on the name")
    parser.add_argument("results_file", type=str, help="Most up-to-date results file location outside of {results_dir}, gets overwritten with the same TOML")
    parser.add_argument("--extract-from", type=str, default=None, help="create a dummy file without writing to results_dir using only the test file specified")

    args = parser.parse_args()

    cur_timestamp = datetime.datetime.now(datetime.timezone.utc).isoformat(sep="_", timespec="seconds")

    toml: Dict[str, Any] = {
        "timestamp": cur_timestamp,
        "generated_by": "gen_report.py",
        "git_hash": git_hash(),
    }

    if args.extract_from:
        toml["extract_from"] = asdict(project_stats(args.extract_from, "DUT"))
    else:
        for (project_shortname, dut) in RELEVANT_PROJECTS.items():
            toml[project_shortname] = asdict(project_stats(os.path.join("results", f"{dut}.toml"), dut))
        # Only write the file to the backup directory if it wasn't specific to one test
        with open(os.path.join(args.results_dir, f"hardware_latency_{cur_timestamp}.toml"), "wb") as f:
            tomli_w.dump(toml, f)

    with open(args.results_file, "wb") as f:
        tomli_w.dump(toml, f)