#! /usr/bin/env python3

import os, sys
import json
import errno
import atexit
from pprint import pprint
from datetime import datetime
import itertools
import timeit

import pytz
import requests

import itertools
import functools
import operator
import collections
import array
import queue


SESSION    = '53616c7465645f5f9838857d1efde02a4ef614ed68c0d049e85848042c7adb008a78168f2d2d1eb29f92db86ff9e351d'
USER_AGENT = 'aocd.py/v0.3.6'
URI = 'http://adventofcode.com/{year}/day/{day}/input'

def guess_day():
    """
    Most recent day, if it's during the Advent of Code.  Happy Holidays!
    Raises exception otherwise.
    """
    aoc_now = datetime.now(tz=pytz.timezone('Asia/Kolkata'))
    if aoc_now.month != 12:
        raise Exception('guess_day is only available in December')
    day = min(aoc_now.day, 25)
    return day

def get_data(day):
    """
    Get data for day (1-25) and year (> 2015)
    """
    year = 2018
    inputfile = 'inputs/input{:02}.txt'.format(day)

    if os.path.exists(inputfile):
        data = open(inputfile).read()
        data = data.strip()
    else:
        uri = URI.format(year=year, day=day)
        response = requests.get(uri,
                                cookies={'session': SESSION},
                                headers={'User-Agent': USER_AGENT})
        if response.status_code != 200:
            print(response.status_code)
            print(response.content)
            raise Exception('Unexpected response')
        data = response.text.strip()

        print('Fetched data for Dec {}'.format(day))
        with open(inputfile, 'w') as output:
            output.write(data)

    return data

################################################################################
# Solvers

def solve1(data):
    # Chronal Calibration

    changes = [int(x) for x in data.split('\n')]
    frequency = functools.reduce(operator.add, changes, 0)
    print(frequency)

    mem = {}
    frequency = 0
    for change in itertools.cycle(changes):
        frequency += change
        if frequency in mem:
            print(frequency)
            break
        else:
            mem[frequency] = True

################################################################################

if __name__ == '__main__':

    if len(sys.argv) > 1:
        day = int(sys.argv[1])
    else:
        day = guess_day()

    if len(sys.argv) > 2:
        if sys.argv[2] == '-':
            data = sys.stdin.read()
        else:
            data = sys.argv[2]
    else:
        data = get_data(day)

    solvers = {}
    solvers = dict([(fn, f) for fn, f in globals().items()
                    if callable(f) and fn.startswith('solve')])

    solver = solvers.get('solve{}'.format(day), None)
    if solver is not None:
        elapsed = timeit.timeit('solver(data)', number=1, globals=globals())
        print('{:.3f} seconds'.format(elapsed))
    else:
        print('No solver for day {}'.format(day))
