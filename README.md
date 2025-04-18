![lace logo](./logo.png)

# lace

[![Build status](https://github.com/rozukke/lace/actions/workflows/rust.yml/badge.svg?branch=main&event=push)](https://github.com/rozukke/lace/actions/workflows/rust.yml)
[![GitHub release](https://img.shields.io/github/release/rozukke/lace.svg)](https://github.com/rozukke/lace/releases/latest)
[![GitHub license](https://img.shields.io/github/license/rozukke/lace.svg)](https://github.com/rozukke/lace/blob/main/LICENSE)
[![Creator rozukke](https://img.shields.io/badge/Creator-rozukke-f497af.svg)](https://github.com/rozukke)
[![Made with Rust](https://img.shields.io/badge/Made%20with-Rust-b7410e.svg)](https://www.rust-lang.org)

`lace` is an all-in-one **LC3** (Little Computer 3) assembly toolchain. `lace` currently supports compiling, checking, running, debugging,
and placing a watch on LC3 assembly files. It supports fancy errors and a superset of **LC3** with many convenience and functionality additions.

## Commands
- `run`: assemble and run a file - all in one command.
- `compile`: creates a binary file with a *.lc3* extension.
- `check`: verifies that your code is correct without running or fully compiling it.
- `watch`: runs `check` for a specified file on save while you develop. Neat!
- `debug`: a full-flegded LC3 step-through debugger with every convenience.
Use `lace debug --print-help` to find out more.
- `fmt`: **(planned)** formats your *.asm* file to fit my arbitrary style guide.
- `clean`: **(planned)** used to clean debug artifacts that will be implemented in the future.

## Instruction set extension
LC3 is unfortunately limited in terms of functionality, with the absence of a stack being the most painful missing feature.
Luckily, LC3 also comes with a spare opcode (`0b1101`/`0xD`), which I have used to implement stack-based instructions on top 
of the existing set. The new instructions are:
- `call` - call a subroutine using a label with 10 bits of precision, and push program counter to stack (usage: `call label`)
- `rets` - pop address off the stack and set program counter to its value (usage: `rets` after calling subroutine)
- `push` - push the contents of a register onto the stack (usage: `push r0`)
- `pop` - pop the top value of the stack off into a register (usage: `pop r0` after pushing)

Please note that these instructions will only function when using the `lace` virtual machine and `run` command.

## Traps
There are a few extra traps that should make debugging a lot nicer! Please note that they will not perform as expected when you run
your binaries with other virtual machines.
- `putn`: print the contents of `r0` to console. That's not usually very easy to do, and you should probably learn why!
- `reg`: print the contents of every register to console.

## Work in progress
There are several features and fixes under development:
- Showing multiple errors per compilation
- File formatting
- Debug symbols

Check the repo for updates as it is under active development.

## Installation

> [!NOTE]
> If you would like to install the Minecraft-enabled version, please switch to
> the `minecraft` branch.

Tagged releases are available to the right for all major platforms.

You can use `cargo` to get set up easily and consistently. If you are
unsure what `cargo` is, check out [this help page](https://doc.rust-lang.org/cargo/getting-started/installation.html).
Alternatively, there is a *flake.nix* available with a Rust development shell, if you have Nix set up.

To install, follow the steps below:
```sh
git clone https://github.com/rozukke/lace.git
cd lace
cargo install --path .
```
You should, as a result, have the `lace` binary available in your PATH.

## Examples
Some examples are available under the *tests/files* directory for testing purposes. You can run them with LC3, e.g.
`lace run tests/files/hw.asm`.

## Contributors
A huge thank you to [@dxrcy](https://github.com/dxrcy) for various additions and bugfixes, including continued work
on the debugger implementation!

<a href="https://github.com/rozukke/lace/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=rozukke/lace" />
</a>

## License
Copyright (c) 2025 Artemis Rosman

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
