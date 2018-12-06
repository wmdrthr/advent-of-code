#! /usr/bin/env python3

import os, sys, re
import json
import errno
import atexit
from pprint import pprint
from datetime import datetime
import itertools
import timeit
import string

import itertools
import functools
import operator
import collections
import array
import queue

import pytz
import requests

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

    changes = [int(x) for x in data]
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


def solve2(data):
    # Inventory Management System

    def countletters(boxid):
        counts = [0 for _ in range(26)]
        start = ord('a')
        for letter in boxid:
            counts[ord(letter) - start] += 1
        return counts

    def hamming_one(bx, by):
        difference = 0
        for i in range(len(bx)):
            if bx[i] != by[i]:
                difference += 1
            if difference > 1:
                return False
        if difference == 1:
            return True
        else:
            return False

    twocount = threecount = 0
    for boxid in data:
        counts = countletters(boxid)
        twice = thrice = False
        for count in counts:
            if count == 2:
                twice = True
            elif count == 3:
                thrice = True
            if twice and thrice:
                break
        if twice:
            twocount += 1
        if thrice:
            threecount += 1
    result = twocount * threecount
    print(result)

    for (bx, by) in itertools.product(data, repeat=2):
        if hamming_one(bx, by):
            result = []
            for i in range(len(bx)):
                if bx[i] == by[i]:
                    result.append(bx[i])
            print(''.join(result))
            break

def solve3(data):
    # No Matter How You Slice It

    claims = map(lambda l: map(int, re.findall(r'\d+', l)), data)
    m = collections.defaultdict(list)
    overlaps = {}
    for (claimid, x, y, width, height) in claims:
        overlaps[claimid] = set()
        for i in range(x, x+width):
            for j in range(y, y + height):
                if m[(i,j)]:
                    for otherclaimid in m[(i,j)]:
                        overlaps[claimid].add(otherclaimid)
                        overlaps[otherclaimid].add(claimid)
                m[(i,j)].append(claimid)

    print(len([k for k in m if len(m[k]) > 1]))
    print([k for k in overlaps if len(overlaps[k]) == 0])

def solve4(data):
    # Repose Record

    lines = sorted(data)

    detail = {}
    current_guard = None
    start = 0
    for line in lines:
        line = line .strip()
        if line == '':
            continue
        numbers = [int(x) for x in re.findall(r'\d+', line)]
        if line[-1] == 't':
            current_guard = numbers[-1]
            if current_guard not in detail:
                detail[current_guard] = collections.defaultdict(int)
        elif line[-2:] == 'ep':
            start =  numbers[4]
        elif line[-2:] == 'up':
            for hour in range(start, numbers[4]):
                detail[current_guard][hour] += 1
                start = 0


    # part 1
    sleepiest_guard  = max(detail.keys(), key = lambda k : sum(detail[k].values()))
    max_hours_slept  = max(detail[sleepiest_guard].keys(), key = lambda k : detail[sleepiest_guard][k])
    print('{} * {} = {}'.format(sleepiest_guard, max_hours_slept, sleepiest_guard * max_hours_slept))

    # part 2
    sleepiest_guard  = max(detail.keys(), key = lambda k : max(detail[k].values() or [0,]))
    sleepiest_minute = max(detail[sleepiest_guard].keys(), key = lambda k : detail[sleepiest_guard][k])

    print('{} * {} = {}'.format(sleepiest_minute, sleepiest_guard, sleepiest_guard * sleepiest_minute))

def solve5(data):
    # Alchemical Reduction

    polymer = [ord(c) for c in data[0]]

    def reaction(unita, unitb):
        return (abs(unita - unitb) == 32)

    def react(polymer):
        while True:
            nochange = True
            if len(polymer) == 2:
                if reaction(polymer[0], polymer[1]):
                    polymer = []
                break
            for idx, unit in enumerate(polymer):
                if idx < len(polymer) - 1:
                    if (reaction(polymer[idx], polymer[idx+1])):
                        polymer = polymer[:idx] + polymer[idx+2:]
                        nochange = False
            if nochange:
                break
        return polymer

    print(len(react(polymer)))

    def process(units):

        mpolymer = [ord(c) for c in data[0] if c not in units]
        return len(react(mpolymer))

    min = len(data)
    for unit in [c[0] + c[1] for c in zip(string.ascii_lowercase, string.ascii_uppercase)]:
        l = process(unit)
        print('{}: {}'.format(unit, l))
        if l < min:
            min = l
    print(min)

def solve6(data):
    # Chronal Coordinates

    points = [(int(p[0]), int(p[1])) for p in map(lambda l : re.findall(r'\d+', l), data)]
    maxint = 4294967296

    minx = miny = maxint
    maxx = maxy = 0
    for p in points:
        if p[0] < minx: minx = p[0]
        if p[0] > maxx: maxx = p[0]
        if p[1] < miny: miny = p[1]
        if p[1] > maxy: maxy = p[1]

    def edge_point(point):
        x, y = point
        return (x == minx or y == miny or x == maxx or y == maxy)

    def distance(a, b):
        return abs(a[0] - b[0]) + abs(a[1] - b[1])

    grid = collections.defaultdict(int)
    count = 0
    for x in range(minx, maxx + 1):
        for y in range(miny, maxy + 1):
            distances = [distance((x, y), p) for p in points]
            totaldistance = sum(distances)
            if totaldistance < 10000:
                count += 1
            mindistance = min(distances)
            mindistancepoint = points[distances.index(mindistance)]
            mindistances = [d for d in filter(lambda d: d == mindistance, distances)]
            if len(mindistances) > 1:
                continue
            if edge_point((x, y)):
                grid[mindistancepoint] = maxint
            grid[mindistancepoint] += 1

    k = max([k for k in grid.keys() if grid[k] < maxint], key = lambda k : grid[k])
    print(k, grid[k])
    print(count)


################################################################################

if __name__ == '__main__':

    if len(sys.argv) > 1:
        day = int(sys.argv[1])
    else:
        day = guess_day()
    print('Day {}'.format(day))

    if len(sys.argv) > 2:
        if sys.argv[2] == '-':
            data = sys.stdin.read()
            sys.stdin = open('/dev/tty')
        else:
            data = sys.argv[2]
    else:
        data = get_data(day)

    data = [line.strip() for line in data.split('\n') if line.strip() != '']

    solvers = {}
    solvers = dict([(fn, f) for fn, f in globals().items()
                    if callable(f) and fn.startswith('solve')])

    solver = solvers.get('solve{}'.format(day), None)
    if solver is not None:
        elapsed = timeit.timeit('solver(data)', number=1, globals=globals())
        print('{:.3f} seconds'.format(elapsed))
    else:
        print('No solver for day {}'.format(day))
