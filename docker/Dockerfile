# This Dockerfile configures a Docker environment that 
# contains all the required packages for the tool
FROM ubuntu:22.04
ARG UID
ARG GID
RUN echo "Group ID: $GID"
RUN echo "User ID: $UID"

USER root
RUN apt-get update -y && apt-get install apt-utils -y
RUN DEBIAN_FRONTEND="noninteractive" apt-get -y install tzdata

# Install basic packages 
RUN apt-get upgrade -y 
RUN apt-get update -y \
    && apt-get install -y verilator gcc-riscv64-unknown-elf \
                          libgmp-dev python3 python3-pip g++\
                          clang llvm lld clang-tidy clang-format \
                          gcc-multilib gcc cmake sudo wget vim \
                          curl tmux git bc

# Install CHERI dependencies
RUN apt-get update -y \
    && apt-get install -y autoconf automake libtool pkg-config \
                          clang bison cmake mercurial ninja-build \
                          samba flex texinfo time libglib2.0-dev \
                          libpixman-1-dev libarchive-dev libarchive-tools \
                          libbz2-dev libattr1-dev libcap-ng-dev \
                          libexpat1-dev libgmp-dev

# Install bsc build dependencies
RUN sudo apt-get update && \
    sudo apt-get -y install make g++ libz-dev flex bison autoconf gperf pkg-config tcl-dev tk-dev libfontconfig1-dev libx11-dev libxft-dev itcl3-dev itk3-dev iverilog git build-essential curl libffi-dev libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

# Install SystemVerilog formatter
#RUN mkdir -p /srcPkgs \
#    && cd /srcPkgs \
#    && wget https://github.com/chipsalliance/verible/releases/download/v0.0-2776-gbaf0efe9/verible-v0.0-2776-gbaf0efe9-Ubuntu-22.04-jammy-x86_64.tar.gz \
#    && mkdir -p verible \
#    && tar xzvf verible-*-x86_64.tar.gz -C verible --strip-components 1

# Append any packages you need here
# RUN apt-get update -y \
#     && apt-get install -y ...

CMD ["bash"]

# Add dev-user
RUN groupadd -o -g $GID dev-user
RUN useradd -r -g $GID -u $UID -m -d /home/dev-user -s /sbin/nologin -c "User" dev-user
RUN echo "dev-user     ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
USER dev-user

# Install Python packages 
ENV PATH="/home/dev-user/.ghcup/bin:${PATH}:/home/dev-user/.local/bin"
RUN pip3 install --user --upgrade pip \
    && pip3 install black colorlog toml tabulate isort
# Install GHC 
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
    BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
    BOOTSTRAP_HASKELL_ADJUST_BASHRC=P sh

#    BOOTSTRAP_HASKELL_GHC_VERSION=9.4.5 \
#    BOOTSTRAP_HASKELL_CABAL_VERSION=3.6.2.0 \

RUN mkdir /home/dev-user/build
WORKDIR /home/dev-user/build 

RUN git clone --recursive https://github.com/B-Lang-org/bsc && \
    cd bsc && \
    git submodule update --init --recursive && \
    sudo mkdir /opt/bsc && \
    sudo chown dev-user:dev-user /opt/bsc && \
    ln -s /opt/bsc inst && \
    cd .. && \
    git clone https://github.com/B-Lang-org/bsc-contrib && \
    cd bsc-contrib && \
    git submodule update --init --recursive && \
    cd .. && \
    . /home/dev-user/.ghcup/env && cabal update && cabal v1-install regex-compat syb old-time split && \
    . /home/dev-user/.ghcup/env && cd bsc && make install-src && make check-smoke && \
    . /home/dev-user/.ghcup/env  make PREFIX="/home/dev-user/bsc-inst" install && \
    rm -rf /home/dev-user/build

ENV PATH="${PATH}:/opt/bsc/bin"
ENV BLUESPECDIR=/opt/bsc/lib

# Add environment variables
RUN printf "\
\nexport LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:\$LIBRARY_PATH \
\n# Basic PATH setup \
\nexport PATH=/workspace/scripts:/home/dev-user/.local/bin:\$PATH:/home/dev-user/.ghcup/bin \
\n# Thread setup \
\nexport nproc=\$(grep -c ^processor /proc/cpuinfo) \
\n# Terminal color... \
\nexport PS1=\"[\\\\\\[\$(tput setaf 3)\\\\\\]\\\t\\\\\\[\$(tput setaf 2)\\\\\\] \\\u\\\\\\[\$(tput sgr0)\\\\\\]@\\\\\\[\$(tput setaf 2)\\\\\\]\\\h \\\\\\[\$(tput setaf 7)\\\\\\]\\\w \\\\\\[\$(tput sgr0)\\\\\\]] \\\\\\[\$(tput setaf 6)\\\\\\]$ \\\\\\[\$(tput sgr0)\\\\\\]\" \
\nexport LS_COLORS='rs=0:di=01;96:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01' \
\nalias ls='ls --color' \
\nalias grep='grep --color'\n" >> /home/dev-user/.bashrc

# Entrypoint set up
WORKDIR /workspace
