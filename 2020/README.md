# Advent of Code 2020

## Progress
![Python Progress](http://weemadarthur.metapath.org/aoc/2020/Python.svg)
![Haskell Progress](http://weemadarthur.metapath.org/aoc/2020/Haskell.svg)
![Rust Progress](http://weemadarthur.metapath.org/aoc/2020/Rust.svg)

## Python

Run `python3 adventofcode.py` to run the solvers for any given day's
puzzle. Pass a day number to run the solvers for that day.

In addition to the day, you can also pass a sample input as argument -
the solvers will run on the given input instead of the puzzle input
for the given day.

The script will automatically download (and cache) the puzzle input
for the appropriate day. For this to work, you will need to fetch your
own session ID (this is set as a cookie when you log in to the AoC
site), and save it in `~/.config/adventofcode/session`.

### Dependencies

* pytz
* requests

## Haskell

The Haskell solution uses [Stack](https://www.haskellstack.org/) as
the build tool.

To run the Haskell solvers, install Stack, then run `stack build` in
the haskell directory to build the project. This will download and
install the GHC compiler, fetch and build all dependencies, then build
the project.

Run `stack run N` to run the solvers for any given day N. The solver
looks for the input data in the `inputs` directory in the parent
directory. Use the Python script to automatically download the input
data.

Run `stack test` to run the included tests for all solvers.

## Rust

The Rust solution is a simple Rust project, using the standard Rust
build tools.

First, [install Rust](https://www.rust-lang.org/learn/get-started) and
the cargo build tool. Once installed, run `cargo build` in the rust
directory to build the project.

Run `cargo run N`  to run the solvers for any given day N. The solver
looks for the input data in the `inputs` directory in the parent
directory. Use the Python script to automatically download the input
data.

Run `cargo test` to run the included tests for all solvers.
