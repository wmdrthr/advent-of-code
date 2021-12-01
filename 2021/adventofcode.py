#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re
import time
from pprint import pprint
from datetime import datetime, timedelta

import itertools

import pytz
import requests

YEAR = 2021

SESSIONID_FILE = '~/.config/adventofcode/session'
USER_AGENT     = 'wmdrthr/advent-of-code'

def get_session_id():
    try:
        sessionid = open(os.path.expanduser(SESSIONID_FILE)).read()
        return sessionid.strip()
    except (OSError, IOError) as err:
        print('Could not load session-id - ', str(err))
        sys.exit(3)

def guess_day():
    """
    Today's date, if it's during the Advent of Code. Happy Holidays!
    Raises exception otherwise.
    """
    now = datetime.now(tz=pytz.timezone('Asia/Kolkata'))
    if now.year != YEAR or now.month != 12 or now.day > 25:
        print(f'AoC {YEAR} not currently running, day must be provided.')
        return 0
    unlock = now.replace(hour = 10, minute = 30,
                         second = 0, microsecond = 0) # Midnight EST -> 10:30 AM IST
    if now < unlock:
        now = now - timedelta(days = 1)
    return now.day

def get_data(day):
    "Get input data for day (1-25) and year (> 2015)"

    inputfile = f'inputs/input{day:02}.txt'

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

        if not os.path.exists('inputs'):
            os.mkdir('inputs')

        uri = f'http://adventofcode.com/{YEAR}/day/{day}/input'
        response = requests.get(uri,
                                cookies={'session': get_session_id()},
                                headers={'User-Agent': USER_AGENT})
        if response.status_code == 200:
            data = response.text
            print(f'Fetched data for day {day}')
            with open(inputfile, 'w') as output:
                output.write(data)
        elif response.status_code == 404:
            print(f'Got 404 when trying to fetch input.\n{response.content}')
            return None
        else:
            raise Exception(f'Unexpected response: (status = {response.status_code})\n{response.content}')

    return data

def format_elapsed_time(elapsed):
    for unit in ['ns', 'us', 'ms', 's']:
        if elapsed > 1000:
            elapsed /= 1000
            continue
        return f'Elapsed: {elapsed:4.3f} {unit}'


custom_data = False


class SolutionMismatch(Exception):
    pass

def with_solutions(*expected):
    def wrapper(f):
        error_msg = 'Incorrect solution for Part {}: Expected "{}", Actual "{}"'
        def wrapped_method(*args):
            fgen = f(*args)
            values = []
            for index in range(2):
                try:
                    actual = next(fgen)
                except (StopIteration, TypeError) as e:
                    if expected[index] is not None:
                        raise SolutionMismatch(f'No solution found for part {index}')
                    if e.args[0] != "'NoneType' object is not an iterator":
                        raise e
                    actual = None

                if actual and not custom_data and expected[index] is not None:
                    if actual != expected[index]:
                        raise SolutionMismatch(error_msg.format(index + 1, expected[index], actual))
                values.append(actual)
            return tuple(values)

        return wrapped_method
    return wrapper

def main():

    global custom_data

    if len(sys.argv) > 1:
        day = int(sys.argv[1])
    else:
        day = guess_day()

    if day == 0:
        print("Couldn't figure out the day. Bailing")
        return 3

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
        return 4

    data = data.strip()

    solvers = {}
    solvers = dict([(fn, f) for fn, f in globals().items()
                    if callable(f) and fn.startswith('solve')])

    solver = solvers.get(f'solve{day}', None)
    if solver is not None:
        start = time.monotonic_ns()
        try:
            solutions = solver(data)
        except SolutionMismatch as s:
            print(s.args[0])
            return 23
        end = time.monotonic_ns()
        elapsed = (end - start)
        if solutions:
            print(*solutions, sep='\n')
        print(format_elapsed_time(elapsed))
    else:
        print(f'No solver for day {day}')
        return 5


################################################################################
# Common Code

ORIGIN = (0, 0)
def manhattan(a, b = ORIGIN):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

DIRECTIONS = { '↑' : (( 0, -1), ('←', '→')),
               '↓' : (( 0,  1), ('→', '←')),
               '←' : ((-1,  0), ('↓', '↑')),
               '→' : (( 1,  0), ('↑', '↓')),
               '↖' : ((-1, -1), ('←', '↑')),
               '↗' : (( 1, -1), ('↑', '→')),
               '↘' : (( 1,  1), ('→', '↓')),
               '↙' : ((-1,  1), ('↓', '←'))}

def move(point, direction, distance = 1):
    (dx, dy) = DIRECTIONS[direction][0]
    return (point[0] + (dx * distance), point[1] + (dy * distance))

def turn(heading, direction, angle):
    for _ in range(angle // 90):
        if direction == 'L':
            heading = DIRECTIONS[heading][1][0]
        elif direction == 'R':
            heading = DIRECTIONS[heading][1][1]
    return heading

def rotate(position, direction, angle):
    for _ in range(angle // 90):
        if direction == 'L':
            position = (position[1], -1 * position[0])
        elif direction == 'R':
            position = (-1 * position[1], position[0])
    return position

def neighbors(_, point, rows, cols):
    for dir in DIRECTIONS:
        dx, dy = DIRECTIONS[dir][0]
        if 0 <= point[0] + dx < cols and 0 <= point[1] + dy < rows:
            yield (dir, (point[0] + dx, point[1] + dy))

def raytraced_neighbors(grid, point, rows, cols):
    for dir in DIRECTIONS:
        dx, dy = DIRECTIONS[dir][0]
        for n in itertools.count(1):
            new_point = (point[0] + (dx * n), point[1] + (dy * n))
            if 0 <= new_point[0] < cols and 0 <= new_point[1] < rows:
                if grid[new_point] != 0:
                    yield (dir, new_point)
                    break
            else:
                break

def display(grid, rows, cols, tiles):
    # Given a dict representing a point grid, print the grid, using
    # the given tileset.

    for y in range(rows):
        row = []
        for x in range(cols):
            row.append(tiles[grid[(x, y)]])
        print(''.join(row))


################################################################################
# Solvers

@with_solutions(1162, 1190)
def solve1(data):

    # Sonar Sweep

    readings = [int(l) for l in data.splitlines()]

    count = 0
    for a, b in zip(readings, readings[1:]):
        if b > a:
            count += 1
    yield count

    count = 0
    sums = [a + b + c for a, b, c in zip(readings, readings[1:], readings[2:])]
    for a, b in zip(sums, sums[1:]):
        if b > a:
            count += 1
    yield count


################################################################################

if __name__ == '__main__':

    sys.exit(main())
