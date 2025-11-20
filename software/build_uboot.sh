#!/bin/bash -e
#-
# SPDX-License-Identifier: BSD-2-Clause
#
# Copyright (c) 2018 A. Theodore Markettos
# All rights reserved.
#
# This software was developed by SRI International and the University of
# Cambridge Computer Laboratory (Department of Computer Science and
# Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
# DARPA SSITH research programme.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#

COMPILER_FILE="gcc-linaro-7.2.1-2017.11-x86_64_aarch64-linux-gnu"
COMPILER_URL="https://releases.linaro.org/components/toolchain/binaries/7.2-2017.11/aarch64-linux-gnu/${COMPILER_FILE}.tar.xz" 
SCRIPT_DIR=$(dirname "$(readlink -f "$0")")
CWD=$(pwd)

#GIT="https://github.com/altera-opensource/u-boot-socfpga"
GIT="https://github.com/terasic/u-boot-socfpga"
#BRANCH="socfpga_v2017.09"
BRANCH="de10_pro_revC"
#BRANCH="de10_pro_revD"
#COMMIT="a2cf064e6c87683173aebda9399f6bd9a5ea3a8c"
#PATCHES="uboot-psci.patch uboot-conf-spl-ymodem.patch"
PATCHES="uboot-psci.patch uboot-bootcmd.patch uboot-conf-spl-ram.patch"

echo "Fetching compiler..."
wget -c $COMPILER_URL
if [ ! -d $COMPILER_FILE ] ; then
	echo "Untarring compiler..."
	tar xJf $COMPILER_FILE.tar.xz
fi
export CROSS_COMPILE=$CWD/$COMPILER_FILE/bin/aarch64-linux-gnu-

if [ -d u-boot-socfpga ] ; then
	echo "Cleaning and updating to upstream U-boot source..."
	cd u-boot-socfpga
#	git fetch origin
#	git reset --hard origin/$BRANCH
else
	echo "Fetching U-boot source..."
	git clone -b $BRANCH --depth=1 $GIT
	cd u-boot-socfpga
fi
#git checkout $BRANCH
for p in $PATCHES; do
  echo "Applying patch $SCRIPT_DIR/$p"
  git apply $SCRIPT_DIR/$p;
done
#git checkout $COMMIT

export ARCH=arm64
echo "Configuring U-Boot source..."
make mrproper
# may need to install ncurses-devel or ncurses-dev package for this step
make socfpga_de10_pro_defconfig
#make socfpga_stratix10_de10_pro
#cp de10_pro.config .config
#change any options here

#echo "Edit device tree now"
#read -n1 -rsp $'Press any key to continue or Ctrl+C to exit...\n'

#make menuconfig
make -j32
#make

${CROSS_COMPILE}objcopy -I binary -O ihex --change-addresses 0xffe00000  spl/u-boot-spl-dtb.bin spl/u-boot-spl-dtb.ihex
