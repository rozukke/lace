![lace logo](./logo.png)

# lace

[![Build status](https://github.com/rozukke/lace/actions/workflows/rust.yml/badge.svg?branch=main&event=push)](https://github.com/rozukke/lace/actions/workflows/rust.yml)
[![GitHub release](https://img.shields.io/github/release/rozukke/lace.svg)](https://github.com/rozukke/lace/releases/latest)
[![GitHub license](https://img.shields.io/github/license/rozukke/lace.svg)](https://github.com/rozukke/lace/blob/main/LICENSE)
[![Creator rozukke](https://img.shields.io/badge/Creator-rozukke-f497af.svg)](https://github.com/rozukke)
[![Made with Rust](https://img.shields.io/badge/Made%20with-Rust-b7410e.svg)](https://www.rust-lang.org)

`lace` is an all-in-one **LC3** (Little Computer 3) assembly toolchain. `lace` currently supports compiling, checking, running
and placing a watch on LC3 assembly files. It will support fancy convenience features like a formatter and (probably) running
with a copy of the original OS. **Some features are missing!**

## Commands
- `run`: assemble and run a file - all in one command.
- `compile`: creates a binary file with a *.lc3* extension. Note that `lace` cannot currently run these files, so you may need
a different LC3 virtual machine to test them out until that is implemented.
- `check`: verifies that your file is correct without outputting it or running it.
- `watch`: runs `check` for a specified file on save while you develop. Neat!
- `fmt`: **(planned)** formats your *.asm* file to fit my arbitrary style guide.
- `clean`: **(planned)** used to clean debug artifacts that will be implemented in the future.

## Traps
There are a few extra traps that should make debugging a lot nicer! Please note that they will not perform as expected when you run
your binaries with other virtual machines.
- `putn`: print the contents of `r0` to console. That's not usually very easy to do, and you should probably learn why!
- `reg`: print the contents of every register to console. Currently only supports u16 formatting.

## Work in progress
There are several features and fixes under development:
- Putsp trap
- Showing multiple errors per compilation
- Different number formats for console output
- File formatting
- Debug symbols
- A step-through debugger (big one!)

Check the repo for updates as it is under active development.

## Installation
For now, please use `cargo` to get set up, as there are no official releases. Check in often for the latest updates! If you are
unsure what `cargo` is, check out [this help page](https://doc.rust-lang.org/cargo/getting-started/installation.html).
Alternatively, there is a *flake.nix* available with a Rust development shell, if you have Nix set up.

To install, follow the steps below:
```sh
git clone https://github.com/rozukke/lace.git
cd lace
cargo install --path .
```
You should, as a result, have the `lace` binary available in your PATH.

## License
Copyright (c) 2024 Artemis Rosman

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
