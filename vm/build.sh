#!/usr/bin/env bash

source /home/vagrant/.profile

pushd WasmContinuations/wabt-rs/wabt-sys/wabt
	mkdir build
	cd build
	cmake ..
	cmake --build .
popd

pushd WasmContinuations/wasmtime
	cargo install --path . --force
popd
