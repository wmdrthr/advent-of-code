# Advent of Code

[Advent of Code](https://adventofcode.com) is an Advent calendar of
small programming puzzles for a variety of skill sets and skill levels
that can be solved in any programming language you like. People use
them as a speed contest, interview prep, company training, university
coursework, practice problems, or to challenge each other.

You don't need a computer science background to participate - just a
little programming knowledge and some problem solving skills will get
you pretty far. Nor do you need a fancy computer; every problem has a
solution that completes in at most 15 seconds on ten-year-old
hardware.

This repository contains my solutions to Advent of Code. The primary
language is Python. I am also using the AoC to practice Haskell and
Rust, so there will be some solutions in those languages as well.


## Python

Run `python3 adventofdcode.py` to run the solvers for today's problem
(only in December, of course).

Pass a day number to run the solvers for that day.

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
* networkx

## Haskell

Run `stack exec aoc N` to run the solvers for any given day N.

Run `stack test` to run the built-in tests for all solvers.

Run `stack test adventofcode:dayN` to run only the tests for the given day N.

#### Dependencies

* [Stack](https://www.haskellstack.org/)
