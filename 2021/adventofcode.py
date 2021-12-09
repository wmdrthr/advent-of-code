#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re, io
import time
from pprint import pprint
from datetime import datetime, timedelta
import contextlib

import itertools
import collections
import statistics
import math

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
        return f'{elapsed:4.3f} {unit}'


custom_data = False

class SolutionMismatch(Exception):

    def __init__(self, message, values):

        self.values = values
        super().__init__(message)

def with_solutions(*expected):
    def wrapper(f):
        def wrapped_method(*args):
            fgen = f(*args)
            values = []
            for index in range(2):
                try:
                    actual = next(fgen)
                except (StopIteration, TypeError) as e:
                    if len(expected) > index and expected[index] is not None:
                        raise SolutionMismatch(f'No solution found for part {index + 1}', tuple(values))
                    if len(e.args) > 0 and e.args[0] != "'NoneType' object is not an iterator":
                        raise e
                    actual = None

                if actual and not custom_data and len(expected) > index:
                    if expected[index] is None:
                        raise SolutionMismatch(f'Could not verify solution for part {index + 1}', tuple(values))
                    if actual != expected[index]:
                        raise SolutionMismatch('Incorrect solution for Part {}: Expected "{}", Actual "{}"'.format(index + 1,
                                                                                                                   expected[index],
                                                                                                                   actual),
                                               tuple(values))
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
            if s.values:
                print(*s.values, sep='\n')
            print(s.args[0])
            return 23
        end = time.monotonic_ns()
        elapsed = (end - start)
        if solutions:
            print(*solutions, sep='\n')
        print(f'Time: {format_elapsed_time(elapsed)}')
    else:
        print(f'No solver for day {day}')
        return 5

def test():

    passed = failed = 0

    solvers = {}
    solvers = dict([(fn, f) for fn, f in globals().items()
                    if callable(f) and fn.startswith('solve')])

    errors = []

    start = time.monotonic_ns()
    for day in range(1, 26):
        solver = solvers.get(f'solve{day}', None)
        if solver is not None:
            try:
                data = get_data(day).strip()
                with contextlib.redirect_stdout(io.StringIO()) as f:
                    with contextlib.redirect_stderr(f):
                        solutions = solver(data)
                print('.', end='', flush=True)
                passed += 1
            except SolutionMismatch as s:
                print('E', end='', flush=True)
                errors.append((day, s.args[0]))
                failed += 1
        else:
            print('_', end='', flush=True)
    elapsed = (time.monotonic_ns() - start)

    print()

    if errors:
        for day, error in errors:
            print(f'Day {day} failed ({error}).')

    print(f'Test result: {passed} passed, {failed} failed; finished in {format_elapsed_time(elapsed)}.')

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


@with_solutions(1648020, 1759818555)
def solve2(data):

    # Dive!

    data = [line.split() for line in data.splitlines()]
    course = [(move, int(val)) for move, val in data]

    def navigate(using_aim):

        aim, depth, position = 0, 0, 0

        for move, val in course:
            if move == 'forward':
                position += val
                if using_aim:
                    depth += aim * val
            elif move == 'down':
                if using_aim:
                    aim += val
                else:
                    depth += val
            elif move == 'up':
                if using_aim:
                    aim -= val
                else:
                    depth -= val
        return depth * position

    yield navigate(False)
    yield navigate(True)


@with_solutions(2035764, 2817661)
def solve3(data):

    # Binary Diagnostic

    numbers = data.splitlines()

    gamma = []
    epsilon = []

    for col in range(len(numbers[0])):
        c = collections.Counter(number[col] for number in numbers)
        if c['1'] > c['0']:
            gamma.append('1')
            epsilon.append('0')
        else:
            gamma.append('0')
            epsilon.append('1')

    gamma = ''.join(gamma)
    epsilon = ''.join(epsilon)
    yield int(gamma, 2) * int(epsilon, 2)

    def life_support_rating(numbers, rating):
        col = 0
        while len(numbers) > 1 and col < len(numbers[0]):
            c = collections.Counter(number[col] for number in numbers)
            if rating == 'oxygen':
                selected_bit = max(['1', '0'], key=lambda k: c[k])
            else:
                selected_bit = min(['0', '1'], key=lambda k: c[k])
            numbers = [number for number in numbers if number[col] == selected_bit]
            col += 1

        return int(numbers[0], 2)

    oxygen = life_support_rating(numbers[:], 'oxygen')
    carbondioxide = life_support_rating(numbers[:], 'carbondioxide')

    yield oxygen * carbondioxide


@with_solutions(50008, 17408)
def solve4(data):

    # Giant Squid

    lines = data.splitlines()
    called_numbers = [int(n) for n in lines[0].split(',')]

    class BingoBoard:

        def __init__(self, lines):

            self.marked = [[False, False, False, False, False] for _ in range(5)]
            self.numbers = [[int(n) for n in l.split(' ') if n != ''] for l in lines]

            self.lookup = {}
            for row in range(5):
                for col in range(5):
                    self.lookup[self.numbers[row][col]] = (row, col)

        def call(self, number):

            row, col = self.lookup.get(number, (None, None))
            if row is not None:
                self.marked[row][col] = True

        def unmarked_numbers(self):

            for row in range(5):
                for col in range(5):
                    if not self.marked[row][col]:
                        yield self.numbers[row][col]

        def winner(self):

            return any(all(row) for row in self.marked) or \
                any(all(col) for col in zip(*self.marked))

        def print(self):

            for row in range(5):
                for col in range(5):
                    if self.marked[row][col]:
                        print(f'{self.numbers[row][col]:>3}', end='*')
                    else:
                        print(f'{self.numbers[row][col]:>3}', end=' ')
                print()
            print()

    boards = []
    for n in range(2, len(lines), 6):
        board = BingoBoard(lines[n:n+5])
        boards.append(board)

    first_winner = None
    winning_boards = set()
    for number in called_numbers:
        for board in boards:
            board.call(number)
            if board.winner():
                if first_winner is None:
                    yield sum(board.unmarked_numbers()) * number
                    first_winner = board

                winning_boards.add(board)

                if len(winning_boards) == len(boards):
                    yield sum(board.unmarked_numbers()) * number
                    return


@with_solutions(5306, 17787)
def solve5(data):

    # Hydrothermal Venture

    lines = []
    for line in data.splitlines():
        a, b = line.split(' -> ')
        x1, y1 = [int(v) for v in a.split(',')]
        x2, y2 = [int(v) for v in b.split(',')]
        lines.append(((x1, y1), (x2, y2)))

    def generate_points(line):

        ((x1, y1), (x2, y2)) = line
        dx, dy = x2 - x1, y2 - y1
        length = max(abs(dx), abs(dy))
        step_x, step_y = dx // length, dy // length
        for i in range(length + 1):
            yield (x1 + i * step_x, y1 + i * step_y)

    def mark_points(skip_diagonals = True):

        grid = collections.defaultdict(int)

        for line in lines:
            ((x1, y1), (x2, y2)) = line
            if skip_diagonals and not (x1 == x2 or y1 == y2):
                continue

            for point in generate_points(line):
                grid[point] += 1

        return grid

    # Part 1
    grid = mark_points()
    #if custom_data:
    #    display(grid, 10, 10, '.123456789')
    yield sum(1 for v in grid.values() if v > 1)

    # Part 2
    grid = mark_points(False)
    #if custom_data:
    #    display(grid, 10, 10, '.123456789')
    yield sum(1 for v in grid.values() if v > 1)


@with_solutions(361169, 1634946868992)
def solve6(data):

    # Lanternfish

    lanternfish = [int(v) for v in data.split(',')]

    def simulate(days):

        population = collections.defaultdict(int)
        for fish in lanternfish:
            population[fish] += 1

        for _ in range(days):
            new_population = collections.defaultdict(int)
            for age, count in population.items():
                if age > 0:
                    new_population[age - 1] += count
                else:
                    new_population[6] += count
                    new_population[8] += count
            population = new_population

        return sum(population.values())

    yield simulate(80)
    yield simulate(256)


@with_solutions(349812, 99763899)
def solve7(data):

    # The Treachery of Whales

    positions = [int(v) for v in data.split(',')]
    positions.sort()
    median = int(statistics.median(positions))
    mean = int(statistics.mean(positions))

    def fuel_cost(move):
        return move * (move + 1) // 2

    # Part 1
    yield sum(abs(x - median) for x in positions)

    # Part 2
    minfuel = 1e9
    for p in range(mean - 1, mean + 2):
        minfuel = min(minfuel, sum(fuel_cost(abs(x-p)) for x in positions))
    yield minfuel


@with_solutions(344, 1048410)
def solve8(data):

    # Seven Segment Search

    data = [l.split('|') for l in data.splitlines()]

    # Part 1
    count = 0
    for _, outputs in data:
        for output in outputs.split(' '):
            if len(output) in (2, 3, 4, 7):
                count += 1

    yield count

    # Part 2
    canonical_pattern = "abcefg cf acdeg acdfg bdcf abdfg abdefg acf abcdefg abcdfg"
    canonical_scores = collections.Counter([c for c in canonical_pattern if c != ' '])

    lookup_table = {}
    for digit, signals in enumerate(canonical_pattern.split(' ')):
        score = sum(canonical_scores[s] for s in signals)
        lookup_table[score] = digit

    total = 0
    for signals, outputs in data:
        number = 0
        score = collections.Counter([s for s in signals if s != ' '])
        for output in outputs.strip().split(' '):
            digit_score = sum(score[c] for c in output)
            digit = lookup_table[digit_score]
            number = (number * 10) + digit
        total += number

    yield total


################################################################################

if __name__ == '__main__':

    if len(sys.argv) > 1 and sys.argv[1] == 'test':
        test()
        sys.exit(0)
    sys.exit(main())
