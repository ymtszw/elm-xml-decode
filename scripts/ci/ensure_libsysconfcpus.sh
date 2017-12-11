#!/usr/bin/env bash
# Using rather unsafe `pwd` since $HOME is unavailable in CircleCI
set -eu
cwd=$(pwd)
if [ ! -d sysconfcpus/bin ]; then
  git clone https://github.com/obmarg/libsysconfcpus.git
  pushd libsysconfcpus
    ./configure --prefix="${cwd}/sysconfcpus"
    make && make install
  popd
fi
