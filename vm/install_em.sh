#!/usr/bin/env bash

git clone https://github.com/emscripten-core/emsdk.git
pushd emsdk
	./emsdk install latest
	./emsdk activate latest
popd