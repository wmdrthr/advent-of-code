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