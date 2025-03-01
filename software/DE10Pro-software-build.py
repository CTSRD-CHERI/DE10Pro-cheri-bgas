#! /usr/bin/env python3

import os
import git
import shutil
import tarfile
import requests
import tempfile
import subprocess
import collections

# helpers
################################################################################

# verbose prints

def vprint(tgtlvl, msg, pfx = f"{'':<5}"):
  try:
    if (tgtlvl <= vprint.lvl):
      print(f"{pfx}{msg}")
  except AttributeError:
    print("verbosity level not set, defaulting to 0")
    vprint.lvl = 0
    vprint(tgtlvl, msg)

# build context
Ctxt = collections.namedtuple("Ctxt", [ "outputdir"
                                      , "cheribuild"
                                      , "workdir"
                                      , "aarch64_cross_pfx" ]
                                    , defaults = [ None
                                                 , None
                                                 , None
                                                 , None ])

class BuildContext(object):

  def __init__( self
              , ctxt = None
              , workdir = None
              , outputdir = None
              , cheribuild = None
              , aarch64_cross_pfx = None
              , cleanup = True):
    if not ctxt:
      self.ctxt = Ctxt()
    else:
      self.ctxt = ctxt
    if workdir:
      self.ctxt = self.ctxt._replace(workdir = workdir)
    if outputdir:
      self.ctxt = self.ctxt._replace(outputdir = outputdir)
    if cheribuild:
      self.ctxt = self.ctxt._replace(cheribuild = cheribuild)
    if aarch64_cross_pfx:
      self.ctxt = self.ctxt._replace(aarch64_cross_pfx = aarch64_cross_pfx)
    self.cleanup = cleanup
    self.olddir = None
    self.tmpdir = None

  def __enter__(self):
    if self.ctxt.workdir:
      os.makedirs(self.ctxt.workdir, exist_ok = True)
    else:
      self.tmpdir = tempfile.TemporaryDirectory()
      self.ctxt = self.ctxt._replace(workdir = self.tmpdir.name)
    self.olddir = os.getcwd()
    if not self.ctxt.outputdir:
      self.ctxt = self.ctxt._replace(outputdir = self.olddir)
    os.chdir(self.ctxt.workdir)
    return self.ctxt

  def __exit__(self, exception_type, exception_value, traceback):
    os.chdir(self.olddir)
    if self.cleanup:
      if self.tmpdir:
        self.tmpdir.cleanup()
      else:
        shutil.rmtree(self.ctxt.workdir)

# file download

def download(url, outputfilepath = None, outputdir = os.getcwd()):
  r = requests.get(url, allow_redirects = True)
  if outputfilepath:
    fname = outputfilepath
  else:
    fname = os.path.join(outputdir, os.path.basename(url))
  os.makedirs(os.path.dirname(fname), exist_ok=True)
  open(fname, 'wb').write(r.content)
  vprint(2, f"download() (cwd: {os.getcwd()})\n url: {url}\n fname: {fname}")

# build steps
###############################################################################

# cheribuild and llvm install
def install_cheribuild( installdir = "/local/scratch/aj443/tools/cheribuild"
                      , source_root = "/local/scratch/aj443/tools/cheribuild-source-root"
                      , giturl = "https://github.com/CTSRD-CHERI/cheribuild.git"
                      ):

  exepath = f"{installdir}/cheribuild.py"

  def cheribuild(cmdargs):
    cheribuild.source_root = source_root
    cmd = [exepath, f"--source-root={cheribuild.source_root}"] + cmdargs
    subprocess.run(cmd)

  if os.access(exepath, os.X_OK):
    vprint(0, "cheribuild already installed")
  else:
    vprint(0, "installing cheribuild")
    shutil.which("apt")

    pkgdeps = [ "autoconf"
              , "automake"
              , "libtool"
              , "pkg-config"
              , "clang"
              , "bison"
              , "cmake"
              , "mercurial"
              , "ninja-build"
              , "samba"
              , "flex"
              , "texinfo"
              , "time"
              , "libglib2.0-dev"
              , "libpixman-1-dev"
              , "libarchive-dev"
              , "libarchive-tools"
              , "libbz2-dev"
              , "libattr1-dev"
              , "libcap-ng-dev"
              , "libexpat1-dev"
              , "libgmp-dev" ]
    cmd = ["sudo", "apt", "install"] + pkgdeps
    subprocess.run(cmd)
    git.Repo.clone_from(giturl, installdir)

  cheribuild(["llvm"])

  return cheribuild

  # aarch64 gcc install
def install_aarch64_gcc( parentinstalldir = "/local/scratch/aj443/tools"
                       , url="https://releases.linaro.org/components/toolchain/binaries/7.2-2017.11/aarch64-linux-gnu/gcc-linaro-7.2.1-2017.11-x86_64_aarch64-linux-gnu.tar.xz" 
                       , cross_pfx = "aarch64-linux-gnu-"
                      ):
  dirname = os.path.basename(url)
  dirname = os.path.splitext(os.path.splitext(dirname)[0])[0] # peel off .xz and .tar

  full_cross_pfx = f"{parentinstalldir}/{dirname}/bin/{cross_pfx}"

  if os.access(f"{full_cross_pfx}gcc", os.X_OK):
    vprint(0, "aarch64-gcc already installed")
  else:
    vprint(0, "installing aarch64-gcc")
    vprint(2, "pre download")
    download(url)
    vprint(2, "pre tar open")
    tar = tarfile.open(f"{dirname}.tar.xz")
    vprint(2, "pre tar extract")
    tar.extractall(parentinstalldir)

  return full_cross_pfx

################################################################################

def build_riscv_device_tree(ctxt):
  github_base_url = "https://github.com"
  github_user = "CTSRD-CHERI"
  github_repo = "DE10Pro-softcore-devicetree"
  url_base = "/".join([github_base_url, github_user, f"{github_repo}.git"])
  git.Repo.clone_from(url_base, ctxt.workdir)
  subprocess.run(["make", "devicetree.dtb"])
  subprocess.run(["make", "devicetree.wrapped.elf"])
  shutil.copy('devicetree.dtb', ctxt.outputdir)
  os.chmod('devicetree.wrapped.elf', 0o664)
  shutil.copy('devicetree.wrapped.elf', ctxt.outputdir)

###############################################################################

def build_riscv_bbl_purecap(ctxt):
  ctxt.cheribuild(["--force", "bbl-gfe-baremetal-riscv64-purecap"])

###############################################################################

def build_riscv_cheribsd_purecap(ctxt):
  ctxt.cheribuild(["--force", "cheribsd-riscv64-purecap"])
  ctxt.cheribuild(["--force", "disk-image-mfs-root-riscv64-purecap"])
  ctxt.cheribuild([ "--cheribsd-mfs-root-kernel-riscv64/build-fpga-kernels"
                  , "--force"
                  , "cheribsd-mfs-root-kernel-riscv64-purecap" ])

# device tree for the hps system
################################################################################

def build_hps_device_tree(ctxt):
  github_base_url = "https://raw.githubusercontent.com"
  github_user = "terasic"
  github_repo = "linux-socfpga"
  commit_ish = "6143ea1943045351dd4a6d10d54a01906e8427cf"
  url_base = "/".join([github_base_url, github_user, github_repo, commit_ish])
  top = "arch/arm64/boot/dts/altera/socfpga_stratix10_de10_pro.dts"
  srcs = [ top
         , "arch/arm64/boot/dts/altera/socfpga_stratix10_de10_pro.dts"
         , "arch/arm64/boot/dts/altera/socfpga_stratix10.dtsi"
         , "include/dt-bindings/reset/altr,rst-mgr-s10.h"
         , "include/dt-bindings/gpio/gpio.h"
         , "include/dt-bindings/clock/stratix10-clock.h"
         ]
  dtb = ".".join([os.path.basename(top), "dtb"])
  for f in srcs:
    download("/".join([url_base, f]), f)
  cmd = [ "cpp", "-B", ctxt.workdir, "-x", "assembler-with-cpp", "-P"
        , top ]
  x = subprocess.Popen(cmd, stdout = subprocess.PIPE)
  cmd = [ "dtc", "-I", "dts", "-O", "dtb", "-p", "0x1000", "-o"
        , os.path.join(ctxt.outputdir, dtb) ]
  y = subprocess.Popen(cmd, stdin = x.stdout, stdout = subprocess.PIPE)
  x.wait()
  y.wait()

################################################################################

def build_hps_uboot(ctxt, patches = []):

  # get the uboot sources
  github_org="terasic"
  github_repo_name="u-boot-socfpga"
  github_branch="de10_pro_revC"
  github_url=f"https://github.com/{github_org}/{github_repo_name}"

  repo = git.Repo.clone_from(github_url, github_repo_name)
  os.chdir(github_repo_name)
  repo.git.checkout(github_branch)
  for p in patches:
    repo.git.apply(p)
  customenv = os.environ.copy()
  customenv['ARCH'] = "arm64"
  customenv['CROSS_COMPILE'] = ctxt.aarch64_cross_pfx
  x = subprocess.Popen(["make", "mrproper"], env = customenv)
  x.wait()
  x = subprocess.Popen(["make", "socfpga_de10_pro_defconfig"], env = customenv)
  x.wait()
  x = subprocess.Popen(["make"], env = customenv)
  x.wait()

###############################################################################

def build_hps_riscv_device_tree_overlay(ctxt, top = None):
  if not top:
    top = os.path.join( ctxt.outputdir
                      , "socfpga_stratix10_de10_pro_cheri_bgas_system.dtso" )
  vprint(2, f"Using {top} as the toplevel .dtso")
  github_base_url = "https://raw.githubusercontent.com"
  github_user = "freebsd"
  github_repo = "freebsd-src"
  commit_ish = "stable/14"
  url_base = "/".join([github_base_url, github_user, github_repo, commit_ish])
  incdir = "sys/contrib/device-tree/include"
  srcs = [ "dt-bindings/interrupt-controller/arm-gic.h"
         , "dt-bindings/interrupt-controller/irq.h" ]
  dtbo = ".".join([os.path.basename(top), "dtbo"])
  for f in map(lambda x: "/".join([incdir, x]), srcs):
    download("/".join([url_base, f]), f)
  cmd = ["cpp", "-I", incdir, "-x", "assembler-with-cpp", "-P", top]
  x = subprocess.Popen(cmd, stdout = subprocess.PIPE)
  cmd = [ "dtc", "-H", "both", "-O", "dtb", "-o"
        , os.path.join(ctxt.outputdir, dtbo)]
  y = subprocess.Popen(cmd, stdin = x.stdout, stdout = subprocess.PIPE)
  x.wait()
  y.wait()

###############################################################################

def build_hps_arm_freebsd(ctxt):
  args = [ "--force"
         , "--freebsd/repository=https://github.com/CTSRD-CHERI/freebsd-morello"
         #, "--freebsd/git-revision=stratix10"
         , "--freebsd/git-revision=stratix10_fixed"
         #, f"--freebsd/source-directory={ctxt.cheribsd.source_root}/freebsd"
         , "--freebsd/toolchain=cheri-llvm"
         , "freebsd-aarch64"
         , "disk-image-freebsd-aarch64" ]
  ctxt.cheribuild(args)

################################################################################

if __name__ == "__main__":

  # set verbosity level
  vprint.lvl = 3
  # setup aarch64-gcc
  aarch64_cross_pfx = install_aarch64_gcc()
  # setup cheribuild
  cheribuild = install_cheribuild()
  # create a working directory
  extras = f"{cheribuild.source_root}/extra-files"
  os.makedirs(extras, exist_ok = True)
  uboot_patches=[f"{os.getcwd()}/uboot-psci.patch"]
  with BuildContext(cheribuild = cheribuild, aarch64_cross_pfx = aarch64_cross_pfx, outputdir = extras) as ctxt:
    #vprint(2, f"Using {ctxt.workdir} as a working directory")
    ## build RISC-V device tree
    #with BuildContext(ctxt, os.path.join(ctxt.workdir, "riscv_dtb")) as ctxt:
    #  build_riscv_device_tree(ctxt)
    ## build RISC-V bbl
    #build_riscv_bbl_purecap(ctxt)
    ## build RISC-V freebsd
    #build_riscv_cheribsd_purecap(ctxt)
    ## build HPS device tree
    #with BuildContext(ctxt, os.path.join(ctxt.workdir, "hps_dtb")) as ctxt:
    #  build_hps_device_tree(ctxt)
    ## build HPS device tree overlay for RISC-V system
    #with BuildContext(ctxt, os.path.join(ctxt.workdir, "hps_riscv_dtbo")) as ctxt:
    #  build_hps_riscv_device_tree_overlay(ctxt)
    ## build uboot
    #build_hps_uboot(ctxt, uboot_patches)
    # build HPS freebsd
    build_hps_arm_freebsd(ctxt)
