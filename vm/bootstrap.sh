#!/usr/bin/env bash

guser="$1"
gpass="$2"

apt-get update
apt-get install clang -y
apt-get install r-base -y
su vagrant -c 'curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y'
su vagrant -c 'source /home/vagrant/.cargo/env'

git config --global credential.helper store
git clone "https://$guser:$gpass@github.com/donald-pinckney/WasmContinuations.git"

pushd WasmContinuations
	git submodule update --init --recursive

	pushd wasmtime
		# cargo install --path . --force
	popd
popd


echo 'export PATH="$HOME/WasmContinuations/emcc_control/build-tools:$PATH"' >> /home/vagrant/.profile
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> /home/vagrant/.profile