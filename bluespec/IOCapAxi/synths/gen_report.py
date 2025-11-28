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
    f.seek(0, os.SEEK_SET)

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
class SynthStats:
    dut: str
    fit_timestamp: str
    sta_timestamp: str

    fmax: str # XXX.XX MHz
    luts_total: int
    luts_dut: int
    luts_input_harness: int
    luts_output_harness: int
    luts_device_max: int

# group 1 = project name
PROJECT_NAME_PATTERN = re.compile(r'^project_name := "(\w+)"')
# group 1 = DUT name
DUT_PATTERN = re.compile(r'^dut := "(\w+)"')
# group 1 = Fmax
# group 2 = Restricted Fmax
FMAX_PATTERN = re.compile(r'; (\d+\.\d+) MHz\s+; (\d+\.\d+) MHz\s+;\s+CLK_FAST\s+;\s+[^;]+\s+;\s+.+ Model\s+;')
# group 1 = ALMs needed
# group 2 = total ALMs on device
LUTS_PATTERN = re.compile(r'^; Logic utilization \(ALMs needed / total ALMs on device\)\s+; ([\d,]+)\s+/\s+([\d,]+)')
# Fitter resource utilization by entity has 21 columns, first = compilation hierarchy, second = ALMs needed (fractional? quartus appears to round it down)
GENERIC_LUTS_PATTERN = re.compile(r'^;\s+([|\w]+)\s+; (\d+(\.\d+)?)[^;]+;.*' + r' \w+\s+; altera_work\s+;')
# + r'[^;]+;'*17 

def project_stats(project_dir: str) -> SynthStats:
    justfile = os.path.join("projects", project_dir, "Justfile")

    with open(justfile, "r", encoding="utf-8") as f:
        dut = file_line_one_regex_match(f, DUT_PATTERN).group(1)
        project_name = file_line_one_regex_match(f, PROJECT_NAME_PATTERN).group(1)

    timing_file = os.path.join("projects", project_dir, "output_files", f"{project_name}.sta.rpt")
    placing_file = os.path.join("projects", project_dir, "output_files", f"{project_name}.fit.place.rpt")

    timing_timestamp = file_timestamp(timing_file)
    placing_timestamp = file_timestamp(placing_file)

    with open(timing_file, "r", encoding="utf-8") as f:
        m = file_line_one_regex_match(f, FMAX_PATTERN)
        assert float(m.group(1)) >= float(m.group(2))
        fmax = m.group(2)
    with open(placing_file, "r", encoding="utf-8") as f:
        m = file_line_one_regex_match(f, LUTS_PATTERN)
        luts = int(m.group(1).replace(",", ""))
        luts_device_max = int(m.group(2).replace(",", ""))
        assert luts_device_max > luts

        luts_db = {}
        found_luts_db = False
        for line in f:
            if not found_luts_db and not line.startswith("; Fitter Resource Utilization by Entity"):
                continue
            if found_luts_db and line.startswith("Note: For table entries with two numbers listed, "):
                assert luts_db != {}
                found_luts_db = False
            found_luts_db = True
            m = GENERIC_LUTS_PATTERN.match(line)
            if m:
                name = m.group(1)
                if name not in ['|', '|i|', '|o|', '|dut|']:
                    continue
                alms = m.group(2)
                print(name, alms)
                if name in luts_db:
                    raise RuntimeError(f"found two lut_db entries for {name} in {f}")
                # round away from .5
                luts_db[name] = round(float(alms))
    
    print(luts_db)
    print(f)
    assert abs(luts_db["|"] - luts) <= 2, f"Inconsistent LUTs for {f} - toplevel = {luts}, db = {luts_db['|']}"

    return SynthStats(
        dut=dut,
        sta_timestamp=timing_timestamp,
        fit_timestamp=placing_timestamp,
        fmax=fmax,
        luts_total=luts,
        luts_dut=luts_db['|dut|'],
        luts_input_harness=luts_db["|i|"],
        luts_output_harness=luts_db["|o|"],
        luts_device_max=luts_device_max
    )

def all_equal(xs: List) -> bool:
    assert len(xs) > 0
    return all(x == xs[0] for x in xs)

RELEVANT_PROJECTS = {
    "single_checker_1per": "mkSingleChecker3_1percycle_SingleChecker3_design_300MHz",
    "single_checker_2per": "mkSingleChecker3_2percycle_SingleChecker3_design_300MHz",
    "full_exposer_0checkers": "mkCombinedIOCapExposerV6_0pool_KeyManager2V1_64_Tb_UnifiedSingleExposerKeyMngr64Tb_design_185MHz",
}
RELEVANT_PROJECTS.update({
    f"full_exposer_{n}checkers_{p}per_64": f"mkCombinedIOCapExposerV6_blockinvalid_{n}pool_{p}percycle_KeyManager2V1_64_Tb_UnifiedSingleExposerKeyMngr64Tb_design_185MHz"
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

    args = parser.parse_args()

    cur_timestamp = datetime.datetime.now(datetime.timezone.utc).isoformat(sep="_", timespec="seconds")

    toml: Dict[str, Any] = {
        "timestamp": cur_timestamp,
        "generated_by": "gen_report.py",
        "git_hash": git_hash(),
    }
    for (project_shortname, project_name) in RELEVANT_PROJECTS.items():
        toml[project_shortname] = asdict(project_stats(project_name))

    assert all_equal([
        toml[project_shortname]["luts_device_max"]
        for project_shortname in RELEVANT_PROJECTS.keys()
    ])
    
    with open(os.path.join(args.results_dir, f"hardware_synths_{cur_timestamp}.toml"), "wb") as f:
        tomli_w.dump(toml, f)
    with open(args.results_file, "wb") as f:
        tomli_w.dump(toml, f)