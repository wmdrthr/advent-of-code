#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re
import time
from pprint import pprint
from datetime import datetime

import collections
import itertools
import hashlib

import requests

YEAR  = 2015

SESSIONID_FILE = '~/.config/adventofcode/session'
USER_AGENT     = 'wmdrthr/advent-of-code'

def get_session_id():
    try:
        sessionid = open(os.path.expanduser(SESSIONID_FILE)).read()
        return sessionid.strip()
    except (OSError, IOError) as err:
        print('Could not load session-id - ', str(err))
        sys.exit(3)

def get_data(day):
    "Get input data for day (1-25) and year (> 2015)"

    inputfile = 'inputs/input{:02}.txt'.format(day)

    if os.path.exists(inputfile):
        data = open(inputfile).read()
    else:
        uri = 'http://adventofcode.com/{year}/day/{day}/input'.format(year=YEAR, day=day)
        response = requests.get(uri,
                                cookies={'session': get_session_id()},
                                headers={'User-Agent': USER_AGENT})
        if response.status_code != 200:
            raise Exception('Unexpected response: (status = {})\n{}'.format(response.status_code,
                                                                            response.content))
        data = response.text
        print('Fetched data for day {}'.format(day))

        if not os.path.exists('inputs'):
            os.mkdir('inputs')
        with open(inputfile, 'w') as output:
            output.write(data)

    return data

def with_solutions(*expected):
    def wrapper(f):
        error_msg = 'Incorrect solution for Part {}: Expected "{}", Actual "{}"'
        def wrapped_method(*args, **kwargs):
            fgen = f(*args)
            try:
                for index in range(2):
                    actual = next(fgen)
                    if expected[index] is not None and not kwargs['skip_verification']:
                        if actual != expected[index]:
                            print(error_msg.format(index + 1, expected[index], actual))
                            sys.exit(23)
                    print(actual)
                return
            except StopIteration:
                return
            except TypeError as e:
                if e.args[0] == "'NoneType' object is not an iterator":
                    return
                else:
                    raise e

        return wrapped_method
    return wrapper

################################################################################
# Common Code

ORIGIN = (0, 0)
def manhattan(a, b = ORIGIN):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

DIRECTIONS = { '↑' : [( 0, -1), ['←', '→'], 1],
               '↓' : [( 0,  1), ['→', '←'], 2],
               '←' : [(-1,  0), ['↓', '↑'], 3],
               '→' : [( 1,  0), ['↑', '↓'], 4],
               '^' : [( 0, -1), ['<', '>'], 1],
               'v' : [( 0,  1), ['>', '<'], 2],
               '<' : [(-1,  0), ['v', '^'], 3],
               '>' : [( 1,  0), ['^', 'v'], 4]}

def move(point, direction):
    (dx, dy) = DIRECTIONS[direction][0]
    return (point[0] + dx, point[1] + dy)

def neighbors(point):
    for dir in DIRECTIONS:
        dx, dy = DIRECTIONS[dir][0]
        yield (dir, (point[0] + dx, point[1] + dy))



def display(grid, tiles):
    # Given a dict representing a point grid, print the grid, using
    # the given tileset.

    max_x = max_y = 0
    min_x = min_y = 65536
    for point in grid.keys():
        min_x = min(min_x, point[0])
        max_x = max(max_x, point[0])
        min_y = min(min_y, point[1])
        max_y = max(max_y, point[1])

    for y in range(min_y, max_x + 1):
        row = []
        for x in range(min_x, max_x + 1):
            row.append(tiles[grid[(x, y)]])
        print(''.join(row))



################################################################################
# Solvers

@with_solutions(74, 1795)
def solve1(data):

    # Not Quit Lisp

    floor = 0
    basement_step = 0
    for i, ch in enumerate(data):
        if ch == '(':
            floor += 1
        elif ch == ')':
            floor -= 1
        if basement_step == 0 and floor == -1:
            basement_step = i + 1

    yield floor
    yield basement_step

@with_solutions(1606483, 3842356)
def solve2(data):

    # I Was Told There Would Be No Math

    paper_total = ribbon_total = 0
    for present in data.split('\n'):
        l, w, h = list([int(d) for d in present.split('x')])
        sides = (l * w, w * h, h * l)
        perimeters = (2 * (l + w), 2 * (w + h), 2 * (h + l))
        volume = l * w * h
        paper_total += sum([2*s for s in sides]) + min(sides)
        ribbon_total += min(perimeters) + volume

    yield paper_total
    yield ribbon_total

@with_solutions(2565, 2639)
def solve3(data):

    # Perfectly Spherical Houses in a Vacuum

    current = ORIGIN
    presents_counter = collections.defaultdict(int)
    presents_counter[current] = 1

    for direction in data:
        current = move(current, direction)
        presents_counter[current] += 1

    yield len(presents_counter)

    santa, robot = ORIGIN, ORIGIN
    presents_counter = collections.defaultdict(int)
    presents_counter[santa] = 2

    directions = [iter(data)] * 2
    for direction in itertools.zip_longest(*directions):
        santa = move(santa, direction[0])
        presents_counter[santa] += 1
        robot = move(robot, direction[1])
        presents_counter[robot] += 1

    yield len(presents_counter)

@with_solutions(254575, 1038736)
def solve4(data):

    # The Ideal Stocking Stuffer

    def mine_adventcoin(prefix):
        for number in itertools.count(1):
            message = data + str(number)
            m = hashlib.md5()
            m.update(message.encode('ascii'))
            if m.hexdigest().startswith(prefix):
                return number

    yield mine_adventcoin('00000')
    yield mine_adventcoin('000000')

@with_solutions(238, 69)
def solve5(data):

    # Doesn't He Have Intern-Elves For This?

    def nice(string, rules):
        for rule in rules:
            if not rule(string):
                return False
        return True

    ruleset1 = [
        lambda string: len([v for v in string if v in 'aeiou']) >= 3,
        lambda string: any(x[0] == x[1] for x in zip(string, string[1:])),
        lambda string: all(x not in string for x in ('ab', 'cd', 'pq', 'xy'))
    ]

    ruleset2 = [
        lambda string: re.search(r'(..).*\1', string),
        lambda string: re.search(r'(.).\1', string)
    ]

    strings = data.split('\n')
    yield len([string for string in strings if nice(string, ruleset1)])
    yield len([string for string in strings if nice(string, ruleset2)])

################################################################################

if __name__ == '__main__':

    if len(sys.argv) > 1:
        day = int(sys.argv[1])
    else:
        day = guess_day()

    custom_data = False
    if len(sys.argv) > 2:
        if sys.argv[2] == '-':
            data = sys.stdin.read()
        else:
            if os.path.exists(sys.argv[2]):
                data = open(sys.argv[2]).read()
            else:
                data = sys.argv[2]
        custom_data = True
    else:
        data = get_data(day)

    if not data:
        print('Cannot run solver without data. Bailing')
        sys.exit(0)

    data = data.strip()

    solvers = {}
    solvers = dict([(fn, f) for fn, f in globals().items()
                    if callable(f) and fn.startswith('solve')])

    solver = solvers.get('solve{}'.format(day), None)
    if solver is not None:
        start = time.time()
        solver(data, skip_verification=custom_data)
        end = time.time()
        elapsed = (end - start)
        if elapsed > 0.001:
            print('Elapsed: %4.3f s' % elapsed)
        else:
            elapsed *= 1000.0
            print('Elapsed: %4.3f us' % elapsed)
    else:
        print('No solver for day {}'.format(day))
