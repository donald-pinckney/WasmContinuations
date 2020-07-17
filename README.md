# WasmContinuations

## Requirements
[Rust](https://www.rust-lang.org/tools/install) must be installed.

## Cloning

```bash
git clone --recurse-submodules https://github.com/donald-pinckney/WasmContinuations
cd WasmContinuations
```

## Building

```bash
cd wasmtime
cargo build
cargo install --path . --force # To install the wasmtime binary on the system
```
