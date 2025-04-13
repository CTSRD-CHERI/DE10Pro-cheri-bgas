#! /usr/bin/env python3

import os
import time
import types
import socket
import signal
import logging
import argparse
import datetime
import functools as ft
from pathlib import Path
import contextlib
import subprocess

# default values
################################################################################
sim_utils_dir = Path(__file__).resolve().parent
dflt_sim_bin = (sim_utils_dir / "../build/simdir/sim_CHERI_BGAS").resolve()
dflt_devfs_bin = (sim_utils_dir / "tools/cheri-bgas-fuse-devfs").resolve()
dflt_jtagvpi_to_fmemdmi_bin = (sim_utils_dir / "tools/jtagvpi_to_fmemdmi").resolve()
dflt_openocd_bin = (sim_utils_dir / "tools/openocd").resolve()
dflt_openocd_conf = (sim_utils_dir / "openocd.cfg").resolve()
dflt_splice_cmd = (sim_utils_dir / "tools/forever-splice").resolve()
dflt_connect_cmd = (sim_utils_dir / "tools/forever-splice").resolve()

# Generic helpers
################################################################################

def call_and_retry(cmd, pred, n = 10, sleep_step = 0.03, shell = True):
  for _ in range(0, n):
    print('tick')
    try: subprocess.call(cmd, shell = shell)
    finally:
      if pred():
        print('pred held')
        return True
      time.sleep(sleep_step)
      sleep_step *= 2
  return False

def get_pids_with_name(nm):
  out = subprocess.getoutput(f"ps aux | grep '{nm}' | awk '{{print $2}}'")
  return list(map(int, out.split()))

def pid_exists(pid):
  try: os.kill(pid, 0)
  except OSError: return False
  else: return True

def get_children(ppid):
  def direct_children(pid):
    return list(map(int, subprocess.getoutput(f'pgrep --parent {pid}').split()))
  cpids = direct_children(ppid)
  return cpids + [ pid for cpid in direct_children(ppid)
                       for  pid in get_children(cpid) ]

def kill_children(ppid):
  for pid in get_children(ppid):
    os.kill(pid, signal.SIGKILL)

def popen_thoroughly_kill(p):
  pred = lambda: p.poll() is not None
  p.terminate()
  if pred(): return
  p.kill()
  if pred(): return
  time.sleep(0.2)
  if pred(): return
  kill_children(p.pid)
  p.kill()
  time.sleep(0.2)
  if pred(): return
  os.kill(p.pid, signal.SIGKILL)
  time.sleep(0.2)
  if pred(): return
  if call_and_retry(f'kill -9 {p.pid}', pred): return
  else:
    print(f'>> {p.pid} exists: {pid_exists(p.pid)}, children: {get_children(p.pid)}')
    print('>> stdout:')
    with open(p.stdout_path, 'r') as f: print(f.read())
    print('>> stderr:')
    with open(p.stderr_path, 'r') as f: print(f.read())
    raise ValueError(f"What to do? Why won't {p.pid} die?")

@contextlib.contextmanager
def change_cwd(dir):
  """
  Context manager to locally run with a specified working directory
  """
  old_cwd = os.getcwd()
  os.chdir(dir)
  try: yield
  finally: os.chdir(old_cwd)

def test_and_create_dir(dirpath):
  p = Path(dirpath)
  if not p.exists():
    p.mkdir()
  elif not p.is_dir():
    raise ValueError(f"{p} already exists and is not a directory")
  return p

def test_and_open_file(filepath, mode = "w", buffering = 1): # line buffered
  p = Path(filepath)
  if p.exists() and not p.is_file():
    raise ValueError(f"{p} already exists and is not a file")
  return open(p, mode = mode, buffering = buffering)

def verbosePopen(cmd, **kwargs):
  print(" ".join(map(str, cmd)))
  return subprocess.Popen(cmd, **kwargs)

def get_free_port():
  sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
  sock.bind(('', 0))
  port = sock.getsockname()[1]
  print(f'getting "free" port: {port}')
  sock.close()
  return port

################################################################################

idx2vpi_port = lambda x: get_free_port() # pair_to_vpi_port
idx2dbg_port = lambda x: get_free_port() # pair_to_dbg_port
show_idx = lambda p: f'{p[0]}.{p[1]}'

# spawn specific simulation child processes
################################################################################

def spawn_simulator( workdir
                   , x = None
                   , y = None
                   , args = []
                   , stdout_path = None
                   , stderr_path = None
                   , bin = dflt_sim_bin
                   ):
  """
  """
  if not stdout_path: stdout_path = Path(workdir) / 'sim_stdout'
  if not stderr_path: stderr_path = Path(workdir) / 'sim_stderr'
  newenv = os.environ.copy()
  if x is not None: newenv["ROUTER_X"] = str(x)
  if y is not None: newenv["ROUTER_Y"] = str(y)
  print(newenv)
  p = verbosePopen( [bin] + args
                  , cwd = workdir
                  , stdout = test_and_open_file(stdout_path)
                  , stderr = test_and_open_file(stderr_path)
                  , text = True
                  , env = newenv )
  p.stdout_path = stdout_path
  p.stderr_path = stderr_path
  return p

def spawn_devfs( workdir
               , sim_ports_dir = "./simports"
               , sim_dev_dir = "./simdev"
               , stdout_path = None
               , stderr_path = None
               , bin = dflt_devfs_bin
               ):
  """
  """
  if not stdout_path: stdout_path = Path(workdir) / 'devfs_stdout'
  if not stderr_path: stderr_path = Path(workdir) / 'devfs_stderr'
  sim_ports_dir = test_and_create_dir(Path(workdir) / sim_ports_dir)
  sim_dev_dir = test_and_create_dir(Path(workdir) / sim_dev_dir)
  p = verbosePopen( [bin, sim_ports_dir, '-s', sim_dev_dir]
                  , cwd = workdir
                  , stdout = test_and_open_file(stdout_path)
                  , stderr = test_and_open_file(stderr_path)
                  , text = True )
  p.stdout_path = stdout_path
  p.stderr_path = stderr_path
  return p

def spawn_jtagvpi_to_fmemdmi( workdir
                            , vpi_port
                            , fmem_dev = None
                            , stdout_path = None
                            , stderr_path = None
                            , bin = dflt_jtagvpi_to_fmemdmi_bin
                            ):
  """
  """
  if not stdout_path: stdout_path = Path(workdir) / 'jtagvpi_to_fmemdmi_stdout'
  if not stderr_path: stderr_path = Path(workdir) / 'jtagvpi_to_fmemdmi_stderr'
  if not fmem_dev: fmem_dev = Path(workdir) / 'simdev/debug_unit'
  p = verbosePopen( [bin, '-p', str(vpi_port), '-f', fmem_dev]
                  , cwd = workdir
                  , stdout = test_and_open_file(stdout_path)
                  , stderr = test_and_open_file(stderr_path)
                  , text = True )
  p.stdout_path = stdout_path
  p.stderr_path = stderr_path
  return p

def spawn_openocd( workdir
                 , vpi_port
                 , dbg_port
                 , openocd_conf = dflt_openocd_conf
                 , stdout_path = None
                 , stderr_path = None
                 , bin = dflt_openocd_bin
                 ):
  """
  """
  if not stdout_path: stdout_path = Path(workdir) / 'openocd_stdout'
  if not stderr_path: stderr_path = Path(workdir) / 'openocd_stderr'
  p = verbosePopen( [bin, '-c', f'set vpi_port {vpi_port}'
                        , '-c', f'set debug_port {dbg_port}'
                        , '-f', openocd_conf]
                  , cwd = workdir
                  , stdout = test_and_open_file(stdout_path)
                  , stderr = test_and_open_file(stderr_path)
                  , text = True )
  p.stdout_path = stdout_path
  p.stderr_path = stderr_path
  return p

def spawn_connections( ctxt
                     , bin = dflt_connect_cmd
                     ):
  """
  """

  def connect(tx_idx, rx_idx, tx, rx):
    stdout_path = ctxt.sim_dir / f"conn__{show_idx(tx_idx)}_{tx.replace('/', '_')}_{show_idx(rx_idx)}_{rx.replace('/', '_')}_stdout"
    stderr_path = ctxt.sim_dir / f"conn__{show_idx(tx_idx)}_{tx.replace('/', '_')}_{show_idx(rx_idx)}_{rx.replace('/', '_')}_stderr"
    tx_path = ctxt.sim_dir / f"sim_{show_idx(tx_idx)}/simports/bgas-global-ports/{tx}"
    rx_path = ctxt.sim_dir / f"sim_{show_idx(rx_idx)}/simports/bgas-global-ports/{rx}"
    return verbosePopen( [bin, tx_path, rx_path]
                       , cwd = ctxt.sim_dir
                       , stdout = test_and_open_file(stdout_path)
                       , stderr = test_and_open_file(stderr_path)
                       , text = True )

  #  east -> a
  # north -> b
  # south -> c
  #  west -> d

  conns = []
  for (x0, y0), (x1, y1) in ctxt.connections:

    conn01 = lambda tx, rx: connect((x0, y0), (x1, y1), tx, rx)
    conn10 = lambda tx, rx: connect((x1, y1), (x0, y0), tx, rx)

    # South (c) <-> North (b)
    if x0 == x1 and y0 < y1:
      conns.append(conn01('b/tx', 'c/rx'))
      conns.append(conn10('c/tx', 'b/rx'))
    # North (b) <-> South (c)
    elif x0 == x1 and y0 > y1:
      conns.append(conn01('c/tx', 'b/rx'))
      conns.append(conn10('b/tx', 'c/rx'))
    # West (d) <-> East (a)
    elif y0 == y1 and x0 < x1:
      conns.append(conn01('a/tx', 'd/rx'))
      conns.append(conn10('d/tx', 'a/rx'))
    # East (a) <-> West (d)
    elif y0 == y1 and x0 > x1:
      conns.append(conn01('d/tx', 'a/rx'))
      conns.append(conn10('a/tx', 'd/rx'))
    else: raise ValueError(f"Unhandled connection {((x0, y0),(x1, y1))}")

  return conns

################################################################################
# gather and kill processes for simulation termination
def terminate_simulation(ctxt, signum = None, sframe = None):
  print('='*80)
  if signum is not None:
    print(f'signum: {signal.Signals(signum).name}, sframe: {sframe}')
  signal.signal(signal.SIGINT, signal.SIG_IGN)
  signal.signal(signal.SIGABRT, signal.SIG_IGN)
  signal.signal(signal.SIGTERM, signal.SIG_IGN)
  print('terminating cheri-bgas simulation...')
  print('='*80)

  while len(ctxt.conn_procs) > 0:
    print(f'>>> {len(ctxt.conn_procs)} connection processes remaining')
    time.sleep(0.2)
    for i, p in enumerate(ctxt.conn_procs):
      print(f'terminating connection process {p.pid}')
      p.terminate()
      if p.poll() is not None:
        ctxt.conn_procs.pop(i)
        print(f'{p.pid} terminated')
    print('-'*80)

  print('='*80)
  extra_ps = get_pids_with_name(f'{ctxt.sim_dir}/sim_.*/simdev')
  print('Extra devfs related non-children processes:')
  print(extra_ps)
  for p in extra_ps:
    try:
      print(f'kill {p} non-children spawned process')
      os.kill(p, signal.SIGKILL)
    finally: continue

  print('='*80)
  all_ps = [ (idx, nm, p) for idx, d in ctxt.proc_handles.items()
                          for  nm, p in d.items() ]
  while len(all_ps) > 0:
    print(f'>>> {len(all_ps)} children processes remaining')
    time.sleep(0.2)
    for i, (idx, nm, p) in enumerate(all_ps):
      print(f'terminating {nm} @ {idx} (pid: {p.pid})')
      p.terminate()
      if p.poll() is not None:
        all_ps.pop(i)
        print(f'{nm} @ {idx} (pid: {p.pid}) terminated')
    print('-'*80)

  print('='*80)
  print('unmounting fuse devfs')
  for idx in ctxt.proc_handles.keys():
    cmd = [ 'fusermount3'
          , '-u', (ctxt.sim_dir / f'sim_{show_idx(idx)}/simdev').resolve() ]
    print(" ".join(map(str, cmd)))
    subprocess.run(cmd)
    #while subprocess.call(cmd) != 0: print(f'retrying fusermount -u for {idx}')

  print('...cheri-bgas simulation terminated')
  exit(0)

################################################################################

def main(ctxt):

  # create contexts for each node
  ctxts = [ ( idx
            , test_and_create_dir((ctxt.sim_dir / f"sim_{show_idx(idx)}").resolve())
            , idx2vpi_port(idx)
            , idx2dbg_port(idx)) for idx in ctxt.nodes ]
  # spawn every processes
  proc_handles = {}
  print('spawning simulators')
  for idx, node_dir, vpi_port, dbg_port in ctxts:
    proc_handles[idx] = {}
    x, y = idx
    proc_handles[idx]["simulator"] = spawn_simulator(node_dir, x, y)
  time.sleep(0.25)
  print('spawning cheri-bgas-devfs')
  for idx, node_dir, vpi_port, dbg_port in ctxts:
    proc_handles[idx]["devfs"] = spawn_devfs(node_dir)
  time.sleep(0.25)
  print('spawning jtagvpi_to_fmemdmis')
  for idx, node_dir, vpi_port, dbg_port in ctxts:
    proc_handles[idx]["jtagvpi_to_fmemdmi"] = spawn_jtagvpi_to_fmemdmi(node_dir, vpi_port)
  time.sleep(0.25)
  print('spawning openocds')
  for idx, node_dir, vpi_port, dbg_port in ctxts:
    proc_handles[idx]["openocd"] = spawn_openocd(node_dir, vpi_port, dbg_port)
  ctxt.proc_handles = proc_handles
  print("simulation child processes spawned")
  # spawn connection processes
  ctxt.conn_procs = spawn_connections(ctxt)
  print("simulation connection processes spawned")

  with test_and_open_file(ctxt.sim_dir / 'simulation.info', mode='w+') as sim_info:
    sim_info.write(f'cheri_bgas_simulation:\n')
    sim_info.write(f'- creation_date: {datetime.datetime.now()}\n')
    sim_info.write(f'- pid: {os.getpid()}\n')
    sim_info.write(f'- nodes:\n')
    for idx, node_dir, vpi_port, dbg_port in ctxts:
      sim_info.write(f'  - node_{show_idx(idx)}:\n')
      sim_info.write(f'    - sim_path: {node_dir}\n')
      sim_info.write(f'    - vpi_port: {vpi_port}\n')
      sim_info.write(f'    - dbg_port: {dbg_port}\n')
      sim_info.write(f'    - pids:\n')
      sim_info.write(f'      - simulator: {proc_handles[idx]["simulator"].pid}\n')
      sim_info.write(f'      - devfs: {proc_handles[idx]["devfs"].pid}\n')
      sim_info.write(f'      - jtagvpi_to_fmemdmi: {proc_handles[idx]["jtagvpi_to_fmemdmi"].pid}\n')
      sim_info.write(f'      - openocd: {proc_handles[idx]["openocd"].pid}\n')
    sim_info.write(f'- connection_pids:\n')
    for p in ctxt.conn_procs:
      sim_info.write(f'  - {p.pid}\n')

    #sim_info.seek(0)
    #print(f"simulation info logged in {sim_info.name}:\n{sim_info.read()}")
    print(f"simulation info logged in {sim_info.name}")

  for idx, _, _, dbg_port in ctxts:
    print(f'node {idx} dbg port: {dbg_port}')

  print('='*80)

  signal.signal(signal.SIGINT, ft.partial(terminate_simulation, ctxt))
  signal.signal(signal.SIGABRT, ft.partial(terminate_simulation, ctxt))
  signal.signal(signal.SIGTERM, ft.partial(terminate_simulation, ctxt))
  print("signal handlers registered")
  print("simulation running, press ctrl-c to exit")

  # let children run until SIGINT/SIGABRT/SIGTERM/SIGKILL
  signal.pause()

if __name__ == "__main__":

  parser = argparse.ArgumentParser(description='Run a CHERI-BGAS simulation')

  parser.add_argument(
      '-t', '--topology', nargs=2, metavar=("WIDTH", 'HEIGHT'), default=(1,1)
    , help=f"The WIDTH and HEIGHT of the mesh of nodes to simulate")
  parser.add_argument(
      '-r', '--simulation-run-directory', type=Path, metavar='SIM_RUN_DIR'
    , default=Path(f"{os.getcwd()}/sim-cheri-bgas").resolve()
    , help=f"The SIM_RUN_DIR path to the run directory for the simulation")
  parser.add_argument('-v', '--verbose', action='count', default=0
    , help="Increase verbosity level by adding more \"v\".")

  # parse command line arguments
  clargs = parser.parse_args()

  # prepare execution context
  ctxt = types.SimpleNamespace()

  # set verbosity level
  ctxt.logger = logging.getLogger('cheri-bgas-simulation')
  if clargs.verbose > 2:
    ctxt.logger.setLevel(logging.DEBUG)
  elif clargs.verbose > 0:
    ctxt.logger.setLevel(logging.INFO)

  ctxt.sim_dir = test_and_create_dir(clargs.simulation_run_directory)

  ctxt.nodes = [ (x, y) for x in range(0, int(clargs.topology[0]))
                        for y in range(0, int(clargs.topology[1])) ]
  ctxt.connections = [((x, y-1), (x, y)) for x, y in ctxt.nodes if y > 0] +\
                     [((x-1, y), (x, y)) for x, y in ctxt.nodes if x > 0]

  print(ctxt.nodes)
  print(ctxt.connections)

  main(ctxt)
