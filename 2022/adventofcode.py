#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re, io
import time
from pprint import pprint
from datetime import datetime, timedelta
import contextlib
import unicodedata as U

import string
from copy import deepcopy

import pytz
import requests

YEAR = 2022

SESSIONID_FILE = '~/.config/adventofcode/session'
USER_AGENT     = 'wmdrthr/advent-of-code'

def get_session_id() -> str:
    try:
        sessionid = open(os.path.expanduser(SESSIONID_FILE)).read()
        return sessionid.strip()
    except (OSError, IOError) as err:
        print('Could not load session-id - ', str(err))
        sys.exit(3)

def guess_day() -> int:
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

def get_data(day) -> str|None:
    "Get input data for day (1-25) and year (> 2015)"

    inputfile = f'inputs/input{day:02}.txt'

    if os.path.exists(inputfile):
        data = open(inputfile).read()
    else:
        # if trying to fetch the data for the current AoC, check if the
        # day's puzzle has unlocked yet
        now = datetime.now(tz=pytz.timezone('Asia/Kolkata'))
        if now.year == YEAR and now.month == 12 and day < 25 and day == now.day:
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

def format_elapsed_time(elapsed: float) -> str:
    for unit in ['ns', 'us', 'ms', 's']:
        if elapsed > 1000:
            elapsed /= 1000
            continue
        return f'{elapsed:4.3f} {unit}'
    else:
        elapsed /= 60
        return f'{elapsed:4.3f} m'

def colorize_elapsed_time(elapsed: float) -> str:
    elapsed_ms = elapsed // (1000 * 1000)
    if elapsed_ms >= 1000:
        color = '\033[1;31m' # red
    elif elapsed_ms > 100:
        color = '\033[1;33m' # yellow
    elif elapsed < (1000 * 1000):
        color = '\033[1;34m' # blue
    else:
        color = '\033[1;32m' # green
    return f'{color}{format_elapsed_time(elapsed)}\033[0m'

custom_data = False

class SolutionMismatch(Exception):
    pass

def with_solutions(*expected):
    def wrapper(f):
        def wrapped_method(*args):
            fgen = f(*args)
            for index in range(2):
                actual = None
                try:
                    actual = next(fgen)
                    print(actual)
                except (StopIteration, TypeError) as e:
                    if len(expected) > index and expected[index] is not None:
                        raise SolutionMismatch(f'No solution found for part {index + 1}')
                    if len(e.args) > 0 and e.args[0] != "'NoneType' object is not an iterator":
                        raise e

                if actual and not custom_data and len(expected) > index:
                    if expected[index] is None:
                        raise SolutionMismatch(f'Could not verify solution for part {index + 1}')
                    if actual != expected[index]:
                        raise SolutionMismatch('Incorrect solution for Part {}: Expected "{}", Actual "{}"'.format(index + 1,
                                                                                                                   expected[index],
                                                                                                                   actual))
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

    if day != 5:
        data = data.strip()

    solvers = {}
    solvers = dict([(fn, f) for fn, f in globals().items()
                    if callable(f) and fn.startswith('solve')])

    solver = solvers.get(f'solve{day}', None)
    if solver is not None:
        start = time.monotonic_ns()
        try:
            solver(data)
        except SolutionMismatch as s:
            print(s.args[0])
            return 23
        end = time.monotonic_ns()
        elapsed = (end - start)
        print(f'Time: {colorize_elapsed_time(elapsed)}')
    else:
        print(f'No solver for day {day}')
        return 5

def test():

    print('Running tests', flush=True)

    solvers = {}
    solvers = dict([(fn, f) for fn, f in globals().items()
                    if callable(f) and fn.startswith('solve')])

    errors = []
    timings = []
    passed = failed = 0

    total_start = time.monotonic_ns()
    for day in range(1, 26):
        solver = solvers.get(f'solve{day}', None)
        if solver is not None:
            try:
                data = get_data(day)
                if not data:
                    raise Exception(f'Missing data for day {day}')

                data = data.strip()
                test_start = time.monotonic_ns()
                with contextlib.redirect_stdout(io.StringIO()) as f:
                    with contextlib.redirect_stderr(f):
                        solver(data)
                test_elapsed = time.monotonic_ns() - test_start

                print('\033[1;32m.\033[0m', end='', flush=True)
                passed += 1
                timings.append(test_elapsed)
            except SolutionMismatch as s:
                print('\033[1;31mE\033[0m', end='', flush=True)
                errors.append((day, s.args[0]))
                failed += 1
                timings.append(None)
        else:
            print('_', end='', flush=True)
            timings.append(None)

    total_elapsed = time.monotonic_ns() - total_start
    print(f'\nTest result: {passed} passed, {failed} failed; finished in {format_elapsed_time(total_elapsed)}.')

    if errors:
        for day, error in errors:
            print(f'Day {day:>2} failed ({error}).')
    else:
        # print stats only if all tests passed

        for idx, timing in enumerate(timings):
            print(f'Day {idx + 1:>2}', end='\t')
            if timing is not None:
                timing_str = colorize_elapsed_time(timing)
                print(f'{timing_str:>22}')
            else:
                print(f'          -')


################################################################################
# Common Code

ORIGIN = (0, 0)
def manhattan(a, b = ORIGIN):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

# Below functions assume origin is at top-left,
# with (X,Y) -> X = row, # Y = column.

DIRECTIONS = { '↑' : ((-1, 0), ('←', '→')),
               '↓' : (( 1, 0), ('→', '←')),
               '←' : (( 0,-1), ('↓', '↑')),
               '→' : (( 0, 1), ('↑', '↓'))}

DIAGONALS = {'↖' : ((-1,-1), ('←', '↑')),
             '↗' : ((-1, 1), ('↑', '→')),
             '↘' : (( 1, 1), ('→', '↓')),
             '↙' : (( 1,-1), ('↓', '←'))}

ALL_DIRECTIONS = DIRECTIONS | DIAGONALS

def move(point, direction, distance = 1):
    (dx, dy) = DIRECTIONS[direction][0]
    return (point[0] + (dx * distance), point[1] + (dy * distance))

def diagonal_move(point, direction, distance = 1):
    (dx, dy) = DIAGONALS[direction][0]
    return (point[0] + (dx * distance), point[1] + (dy * distance))

ROTATIONS = {'45': ALL_DIRECTIONS, '90': DIRECTIONS}

def turn(heading, direction, angle, base_angle = '90'):
    directions = ROTATIONS[base_angle]
    for _ in range(angle // base_angle):
        if direction == 'L':
            heading = directions[heading][1][0]
        elif direction == 'R':
            heading = directions[heading][1][1]
    return heading


def neighbors(point, rows = None, cols = None, directions = DIRECTIONS):
    for dir in directions:
        dx, dy = directions[dir][0]
        nx, ny = point[0] + dx, point[1] + dy
        if nx < 0 or ny < 0:
            continue
        if rows is not None and nx >= rows:
            continue
        if cols is not None and ny >= cols:
            continue
        yield (dir, (nx, ny))


def display(grid, rows, cols, tiles):
    # Given a dict (not a 2d array) representing a point grid, print
    # the grid, using the given tileset. This assumes a non-sparse
    # grid.

    for r in range(rows):
        row = []
        for c in range(cols):
            row.append(tiles[grid[(r, c)]])
        print(''.join(row))


################################################################################
# Solvers

@with_solutions(71023, 206289)
def solve1(data):

    # Calorie Counting

    foodpacks = data.split('\n\n')

    totals = []
    for pack in foodpacks:
        total = 0
        for item in pack.split('\n'):
            total += int(item)
        totals.append(total)

    yield max(totals)
    yield sum(sorted(totals)[-3:])

@with_solutions(8392, 10116)
def solve2(data):

    # Rock Paper Scissors

    SHAPE_SCORE = {'X': 1, 'Y': 2, 'Z': 3}
    OUTCOME_SCORE = {'win': 6, 'draw': 3, 'loss': 0}
    OUTCOMES = {'X': 'loss', 'Y': 'draw', 'Z': 'win'}
    STRATEGY = {'A': {'win': 'Y', 'loss': 'Z', 'draw': 'X'},
                'B': {'win': 'Z', 'loss': 'X', 'draw': 'Y'},
                'C': {'win': 'X', 'loss': 'Y', 'draw': 'Z'}}

    def outcome(moves: list[str]) -> str:
        match moves:
          case ['A', 'X']: return 'draw'
          case ['A', 'Y']: return 'win'
          case ['A', 'Z']: return 'loss'

          case ['B', 'X']: return 'loss'
          case ['B', 'Y']: return 'draw'
          case ['B', 'Z']: return 'win'

          case ['C', 'X']: return 'win'
          case ['C', 'Y']: return 'loss'
          case ['C', 'Z']: return 'draw'

        return ''

    turns = [line.split(' ') for line in data.splitlines()]

    yield sum(SHAPE_SCORE[turn[1]] + OUTCOME_SCORE[outcome(turn)] for turn in turns)

    score = 0
    for move, desired_outcome in turns:
        desired_outcome = OUTCOMES[desired_outcome]
        response = STRATEGY[move][desired_outcome]
        score += SHAPE_SCORE[response] + OUTCOME_SCORE[desired_outcome]
    yield score

@with_solutions(7446, 2646)
def solve3(data):

    # Rucksack Reorganization

    PRIORITIES = dict(zip(string.ascii_lowercase, range(1, 27))) | dict(zip(string.ascii_uppercase, range(27, 53)))

    rucksacks = []
    for line in data.splitlines():
        midpoint = len(line)//2
        rucksacks.append((line[:midpoint], line[midpoint:]))

    # Part 1
    priority_sum = 0
    for rucksack in rucksacks:
        compartments = [set(compartment) for compartment in rucksack]
        common = compartments[0] & compartments[1]
        priority_sum += PRIORITIES[common.pop()]

    yield priority_sum

    # Part 2
    priority_sum = 0
    for idx in range(0, len(rucksacks), 3):
        group = []
        for rucksack in rucksacks[idx:idx+3]:
            group.append(set(rucksack[0] + rucksack[1]))
        common = group[0] & group[1] & group[2]
        priority_sum += PRIORITIES[common.pop()]

    yield priority_sum

@with_solutions(518, 909)
def solve4(data):

    # Camp Cleanup

    def contains(pairs):
        left, right = pairs
        return left <= right or right <= left

    def overlap(pairs):
        left, right = pairs
        return len(left & right) > 0

    assignments = []
    for line in data.splitlines():
        pairs = line.split(',')
        left = tuple(int(v) for v in pairs[0].split('-'))
        left = set(range(left[0], left[1] + 1))
        right = tuple(int(v) for v in pairs[1].split('-'))
        right = set(range(right[0], right[1] + 1))
        assignments.append((left, right))

    yield len([pair for pair in assignments if contains(pair)])
    yield len([pair for pair in assignments if overlap(pair)])

@with_solutions('RLFNRTNFB', 'MHQTLJRLB')
def solve5(data):

    # Supply Stacks
    starting_stacks = [[] for _ in range(10)]
    commands = []

    flag = False
    for line in data.splitlines():
        if flag:
            words = line.split(' ')
            count, src, dst = int(words[1]), int(words[3]), int(words[5])
            commands.append((count, src, dst))
        else:
            if line.startswith(' 1'):
                continue
            if line == '':
                flag = True
                continue

            for idx in range(0, len(line), 4):
                crate = line[idx:idx+4]
                if crate[0] == '[':
                    starting_stacks[idx // 4].append(crate[1])

    original_stacks = [list(reversed(stack)) for stack in starting_stacks if len(stack) > 0]

    def move_9000(stacks, command):
        count, src, dst = command
        for _ in range(count):
            crate = stacks[src - 1].pop()
            stacks[dst - 1].append(crate)
        return stacks

    def move_9001(stacks, command):
        count, src, dst = command
        crates = [stacks[src - 1].pop() for _ in range(count)]
        for crate in reversed(crates):
            stacks[dst - 1].append(crate)
        return stacks

    # Part 1
    stacks = deepcopy(original_stacks)
    for command in commands:
        stacks = move_9000(stacks, command)
    yield ''.join(stack[-1] for stack in stacks)

    # Part 2
    stacks = deepcopy(original_stacks)
    for command in commands:
        stacks = move_9001(stacks, command)
    yield ''.join(stack[-1] for stack in stacks)


################################################################################

if __name__ == '__main__':

    if len(sys.argv) > 1 and sys.argv[1] == 'test':
        test()
        sys.exit(0)
    sys.exit(main())
