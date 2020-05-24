#!/usr/bin/env bash

source /home/vagrant/.profile

pushd WasmContinuations/wasmtime
	cargo install --path . --force
popd