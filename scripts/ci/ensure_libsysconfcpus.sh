#!/usr/bin/env bash
if [ ! -d sysconfcpus/bin ]; then
  git clone https://github.com/obmarg/libsysconfcpus.git
  pushd libsysconfcpus
    ./configure --prefix="${TRAVIS_BUILD_DIR}/sysconfcpus"
    make && make install
  popd
fi
