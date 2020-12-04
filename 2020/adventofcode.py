#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re
import time
from pprint import pprint
from datetime import datetime, timedelta

import itertools
import collections

import pytz
import requests

YEAR  = 2020

SESSIONID_FILE = '~/.config/adventofcode/session'
USER_AGENT     = 'wmdrthr/advent-of-code'

def get_session_id():
    try:
        sessionid = open(os.path.expanduser(SESSIONID_FILE)).read()
        return sessionid.strip()
    except (OSError, IOError) as err:
        print('Could not load session-id - ', str(err))
        print("""Puzzle inputs differ by user. Log in to the Advent of Code site,
then check your cookies to get the value of session-id. Save this
value in {}""".format(os.path.expanduser(SESSIONID_FILE)))
        sys.exit(3)

def guess_day():
    """
    Today's date, if it's during the Advent of Code. Happy Holidays!
    Raises exception otherwise.
    """
    now = datetime.now(tz=pytz.timezone('Asia/Kolkata'))
    if now.year != YEAR or now.month != 12 or now.day > 25:
        raise Exception('AoC {%d} not currently running, day must be provided.'.format(YEAR))
    unlock = now.replace(hour = 10, minute = 30,
                         second = 0, microsecond = 0) # Midnight EST -> 10:30 AM IST
    if now < unlock:
        now = now - timedelta(days = 1)
    return now.day

def get_data(day):
    "Get input data for day (1-25) and year (> 2015)"

    inputfile = 'inputs/input{:02}.txt'.format(day)

    if os.path.exists(inputfile):
        data = open(inputfile).read()
    else:
        # if trying to fetch the data for the current AoC, check if the
        # day's puzzle has unlocked yet
        now = datetime.now(tz=pytz.timezone('Asia/Kolkata'))
        if now.year == YEAR and now.month == 12 and day < 25:
            unlock = now.replace(hour = 10, minute = 30,
                                 second = 0, microsecond = 0) # Midnight EST -> 10:30 AM IST
            if now < unlock:
                print("Today's puzzle hasn't unlocked yet!")
                return None

        uri = 'http://adventofcode.com/{year}/day/{day}/input'.format(year=YEAR, day=day)
        response = requests.get(uri,
                                cookies={'session': get_session_id()},
                                headers={'User-Agent': USER_AGENT})
        if response.status_code != 200:
            raise Exception('Unexpected response: (status = {})\n{}'.format(response.status_code,
                                                                            response.content))
        data = response.text
        print('Fetched data for day {}'.format(day))
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
               '→' : [( 1,  0), ['↑', '↓'], 4]}

def move(point, direction):
    (dx, dy) = DIRECTIONS[direction][0]
    return (point[0] + dx, point[1] + dy)

def neighbors(point):
    for dir in DIRECTIONS:
        dx, dy = DIRECTIONS[dir][0]
        yield (dir, (point[0] + dx, point[1] + dy))



################################################################################
# Solvers

@with_solutions(703131, 272423970)
def solve1(data):

    # Report Repair

    entries = [int(l) for l in data.splitlines()]

    for a, b in itertools.combinations(entries, 2):
        if a + b == 2020:
            yield a * b
            break

    for a, b, c in itertools.combinations(entries, 3):
        if a + b + c == 2020:
            yield a * b * c
            break

@with_solutions(528, 497)
def solve2(data):

    # Password Philosophy

    valid1 = valid2 = 0
    for line in data.splitlines():
        rule, password = [r.strip() for r in line.split(':')]
        counts, letter = rule.split(' ')
        a, b = [int(d) for d in counts.split('-')]

        if a <= password.count(letter) <= b:
            valid1 += 1

        if (password[a - 1] == letter) ^ (password[b - 1] == letter):
            valid2 += 1

    yield valid1
    yield valid2

@with_solutions(162, 3064612320)
def solve3(data):

    # Toboggan Trajectory

    data = [l.strip() for l in data.split('\n') if len(l) > 1]
    trees = {(x, y):1 for y,l in enumerate(data) for x,c in enumerate(l) if c == '#'}
    treemap = collections.defaultdict(int, trees)

    right = len(data[0])
    bottom = len(data)

    def traverse(dx, dy):
        current = ORIGIN
        count = 0
        while True:
            if treemap[(current[0] % right, current[1])]:
                count += 1
            if current[1] >= bottom:
                break
            current = (current[0] + dx, current[1] + dy)
        return count

    # Part 1
    yield traverse(3, 1)

    # Part 2
    count = 1
    for (dx, dy) in [(1, 1),
                     (3, 1),
                     (5, 1),
                     (7, 1),
                     (1, 2)]:
        count *= traverse(dx, dy)
    yield count

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
