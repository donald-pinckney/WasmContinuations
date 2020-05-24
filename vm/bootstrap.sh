#!/usr/bin/env bash

guser="$1"
gpass="$2"

apt-get update
apt-get install clang -y
apt-get install cmake -y
apt-get install r-base -y
apt-get install default-jre -y

echo 'export CC=/usr/bin/clang' >> /home/vagrant/.profile
echo 'export CXX=/usr/bin/clang++' >> /home/vagrant/.profile

su vagrant -c 'curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y'

su vagrant -c 'git config --global credential.helper store'
su vagrant -c "git clone \"https://$guser:$gpass@github.com/donald-pinckney/WasmContinuations.git\""

pushd WasmContinuations
	su vagrant -c 'git submodule update --init --recursive'
popd

su vagrant -c 'bash /vagrant/build.sh'

echo 'export PATH="$HOME/WasmContinuations/emcc_control/build-tools:$PATH"' >> /home/vagrant/.profile
echo 'alias emsdk_setup="source /home/vagrant/emsdk/emsdk_env.sh"' >> /home/vagrant/.bash_aliases

su vagrant -c 'bash /vagrant/install_em.sh'

