#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re, io
import time
from pprint import pprint
from datetime import datetime, timedelta
import contextlib
import unicodedata as U

import itertools
import collections
import statistics
import math
import functools
import json

import pytz
import requests
import networkx as nx

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

def colorize_elapsed_time(elapsed):
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
                data = get_data(day).strip()

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

def turn(heading, direction, angle, base_angle = 90):
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
        lines.append(((y1, x1), (y2, x2)))

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
    if custom_data:
        display(grid, 10, 10, '.123456789')
    yield sum(1 for v in grid.values() if v > 1)

    # Part 2
    grid = mark_points(False)
    if custom_data:
        display(grid, 10, 10, '.123456789')
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


@with_solutions(417, 1148965)
def solve9(data):

    # Smoke Basin

    data = [l for l in data.splitlines()]
    heightmap = {(x, y):int(c) for x,l in enumerate(data) for y,c in enumerate(l)}
    rows, cols = len(data), len(data[0])

    # Part 1
    lowpoints = []
    for point,height in heightmap.items():
        for _, (nx, ny) in neighbors(point, rows, cols):
            if heightmap[(nx,ny)] <= height:
                break
        else:
            lowpoints.append((point, height))

    yield sum([v[1] + 1 for v in lowpoints])

    # Part 2
    def bfs(starting_point):
        history = set([starting_point])
        queue = collections.deque([starting_point])
        while queue:
            point = queue.popleft()
            height = heightmap[point]
            for _, (nx, ny) in neighbors(point, rows, cols):
                if (nx, ny) not in history and \
                   heightmap[(nx, ny)] != 9 and \
                       heightmap[(nx, ny)] > height:
                    history.add((nx, ny))
                    queue.append((nx, ny))

        return history

    basins = {}
    for lowpoint, _ in lowpoints:
        basins[lowpoint] = bfs(lowpoint)
    basin_sizes = sorted([len(v) for v in basins.values()])
    yield math.prod(basin_sizes[-3:])


@with_solutions(369105, 3999363569)
def solve10(data):

    # Syntax Scoring

    matching = {'{':'}', '[':']', '(':')', '<':'>'}
    scores = {')': 3, ']': 57, '}': 1197, '>': 25137}

    def check(line):

        stack = []

        for ch in line:
            if ch in matching:
                stack.append(matching[ch])
            elif ch != stack.pop():
                return -scores[ch]

        score = 0
        for ch in reversed(stack):
            score = score * 5 + ' )]}>'.index(ch)
        return score

    scores = [check(line) for line in data.splitlines()]

    yield sum(-score for score in scores if score < 0)
    yield statistics.median(score for score in scores if score > 0)


@with_solutions(1603, 222)
def solve11(data):

    # Dumbo Octopus

    data = [l for l in data.splitlines()]
    octopuses = {(x, y):int(c) for x,l in enumerate(data) for y,c in enumerate(l)}
    rows, cols = len(data), len(data[0])

    flashes = 0
    n = 0

    while True:
        total_energy_level = sum(octopuses.values())
        if total_energy_level == 0:
            yield n

        if n == 100:
            yield flashes

        for octopus in octopuses.keys():
            octopuses[octopus] += 1

        flashed = set()
        while True:

            ready_to_flash = [octopus for octopus,energy_level in octopuses.items() if energy_level > 9]
            if len(ready_to_flash) == 0:
                break

            for octopus in ready_to_flash:
                if octopus in flashed:
                    continue
                flashed.add(octopus)
                for _, neighbor in neighbors(octopus, rows, cols, ALL_DIRECTIONS):
                    octopuses[neighbor] += 1

            for octopus in flashed:
                octopuses[octopus] = 0

        flashes += len(flashed)
        n += 1

@with_solutions(3410, 98796)
def solve12(data):

    # Passage Pathing

    graph = collections.defaultdict(set)
    for line in data.splitlines():
        a, b = line.split('-')
        graph[a].add(b)
        graph[b].add(a)

    def dfs(current, history, path, flag):

        if current == 'end':
            if custom_data:
                print(path)
            return 1

        n = 0
        for node in graph[current]:
            if node == 'start':
                continue

            if node.islower() and node in history:
                if not flag:
                    n += dfs(node, history, path + [node], True)
            else:
                n += dfs(node, history | {node}, path + [node], flag)

        return n

    yield dfs('start', set(), ['start'], True)
    yield dfs('start', set(), ['start'], False)


@with_solutions(621, 'HKUJGAJZ')
def solve13(data):

    # Transparent Origami

    point_lines, folds = data.split('\n\n')

    points = {}
    for line in point_lines.split('\n'):
        (x, y) = [int(v) for v in line.split(',')]
        points[(x, y)] = True

    def fold(points, fold):
        fold = fold.split(' ')[2].split('=')
        if fold[0] == 'x':
            x = int(fold[1])
            newpoints = {point:True
                         for point in points
                         if point[0] < x}
            newpoints.update({(2 * x - point[0], point[1]):True
                              for point in points
                              if point[0] > x})
        elif fold[0] == 'y':
            y = int(fold[1])
            newpoints = {point:True
                         for point in points
                         if point[1] < y}
            newpoints.update({(point[0], 2 * y - point[1]):True
                              for point in points
                              if point[1] > y})
        return newpoints

    folds = folds.split('\n')

    # Part 1
    points = fold(points, folds[0])
    yield len(points)

    # Part 2
    for line in folds[1:]:
        points = fold(points, line)

    maxx = maxy = 0
    for (x, y) in points:
        maxx = max(maxx, x)
        maxy = max(maxy, y)

    for y in range(maxy + 1):
        for x in range(maxx + 1):
            if (x,y) in points:
                print(U.lookup('FULL BLOCK'), end='')
            else:
                print(U.lookup('LIGHT SHADE'), end='')
        print()

    yield 'HKUJGAJZ'


@with_solutions(5656, 12271437788530)
def solve14(data):

    # Extended Polymerization

    template, rest = data.split('\n\n')
    rules = {}
    for rule in rest.splitlines():
        a, b = rule.split(' -> ')
        rules[a] = b

    counter = collections.Counter([pair[0] + pair[1] for pair in itertools.pairwise(template)])
    for step in range(40):
        new_counter = collections.Counter()
        for pair in counter:
            mid = rules[pair]
            new_counter[pair[0] + mid] += counter[pair]
            new_counter[mid + pair[1]] += counter[pair]
        counter = new_counter

        if step in (9, 39):
            chars = collections.Counter()
            for pair in counter:
                chars[pair[0]] += counter[pair]
            chars[template[-1]] += 1

            char_counts = list(chars.values())
            yield max(char_counts) - min(char_counts)


@with_solutions(741, 2976)
def solve15(data):

    # Chiton

    data = [l for l in data.splitlines()]
    chiton_map = {(x, y):int(c) for x,l in enumerate(data) for y,c in enumerate(l)}
    rows, cols = len(data), len(data[0])

    def calculate(chiton_map):
        graph = nx.DiGraph()
        for node in chiton_map:
            for _, neighbor in neighbors(node, rows, cols):
                graph.add_edge(node, neighbor, risk_level=chiton_map[neighbor])

        path = nx.shortest_path(graph, source=(0, 0),
                                target=(rows - 1, cols-1),
                                weight='risk_level')
        return sum(chiton_map[node] for node in path[1:])

    yield calculate(chiton_map)

    chiton_map = {}
    for x in range(rows * 5):
        for y in range(cols * 5):
            dist = x // rows + y // cols
            newval = int(data[x % rows][y % cols])
            for n in range(dist):
                newval += 1
                if newval == 10:
                    newval = 1
            chiton_map[(x, y)] = newval
    rows *= 5
    cols *= 5

    yield calculate(chiton_map)


@with_solutions(974, 180616437720)
def solve16(data):

    # Packet Decoder

    class Packet:

        def __init__(self, version, typeid):

            self.version = version
            self.typeid = typeid
            self.subpackets = []

        @classmethod
        def parse(cls, bitstream):

            packet = cls(int(bitstream[0:3], 2), int(bitstream[3:6], 2))
            bitstream = bitstream[6:]

            if packet.typeid == 4:
                value = ''
                cursor = 0
                for chunk in (bitstream[n:n+5] for n in range(0, len(bitstream), 5)):
                    value += chunk[1:]
                    cursor += 5
                    if chunk[0] == '0':
                        break
                packet.value = int(value, 2)
                return packet, bitstream[cursor:]
            else:
                length_type_id, bitstream = bitstream[0], bitstream[1:]
                if length_type_id == '0':
                    subpacket_length, bitstream = int(bitstream[:15], 2), bitstream[15:]
                    length = len(bitstream)
                    while (length - len(bitstream)) < subpacket_length:
                        subpacket, bitstream = Packet.parse(bitstream)
                        packet.subpackets.append(subpacket)

                    return packet, bitstream

                number_of_subpackets, bitstream = int(bitstream[:11], 2), bitstream[11:]
                for _ in range(number_of_subpackets):
                    subpacket, bitstream = Packet.parse(bitstream)
                    packet.subpackets.append(subpacket)

                return packet, bitstream

        def version_sum(self):
            return self.version + sum(sp.version_sum() for sp in self.subpackets)

        def evaluate(self):

            match self.typeid:
                case 0:
                    return sum(sp.evaluate() for sp in self.subpackets)
                case 1:
                    return math.prod(sp.evaluate() for sp in self.subpackets)
                case 2:
                    return min(sp.evaluate() for sp in self.subpackets)
                case 3:
                    return max(sp.evaluate() for sp in self.subpackets)
                case 4:
                    return self.value
                case 5:
                    return int(self.subpackets[0].evaluate() > self.subpackets[1].evaluate())
                case 6:
                    return int(self.subpackets[0].evaluate() < self.subpackets[1].evaluate())
                case 7:
                    return int(self.subpackets[0].evaluate() == self.subpackets[1].evaluate())

        def __str__(self):

            match self.typeid:
                case 4:
                    return f'LiteralValuePacket<v{self.version} {self.value}>'
                case _:
                    return f'OperatorPacket<v{self.version} [{", ".join(str(p) for p in self.subpackets)}]>'

    if custom_data:
        for line in data.splitlines():
            packet_data = ''.join(bin(int(char, 16))[2:].zfill(4) for char in line)
            packet, packet_data = Packet.parse(packet_data)
            print(f'{line} => {packet.version_sum()}, {packet.evaluate()}')

    else:
        packet_data = ''.join(bin(int(char, 16))[2:].zfill(4) for char in data)
        packet, _ = Packet.parse(packet_data)
        yield packet.version_sum()
        yield packet.evaluate()


@with_solutions(4851, 1739)
def solve17(data):

    # Trick Shot

    tx, ty = [[int(v) for v in w[2:].split('..')] for w in data[13:].split(', ')]
    y = abs(ty[0])
    yield y * (y - 1) // 2

    def simulate(vx, vy):
        x = y = 0
        while y >= ty[0]:
            x, y = x + vx, y + vy
            vx, vy = max(0, vx - 1), vy - 1
            if tx[0] <= x <= tx[1] and ty[0] <= y <= ty[1]:
                return 1
        return 0

    yield sum(simulate(vx, vy) for vx in range(tx[1] + 1) for vy in range(ty[0], -ty[0]))



@with_solutions(4120, 4725)
def solve18(data):

    # Snailfish

    def add_left(x, n):
        if n is None:
            return x
        if isinstance(x, int):
            return x + n
        return [add_left(x[0], n), x[1]]


    def add_right(x, n):
        if n is None:
            return x
        if isinstance(x, int):
            return x + n
        return [x[0], add_right(x[1], n)]


    def explode(x, n = 4):
        if isinstance(x, int):
            return False, None, x, None
        if n == 0:
            return True, x[0], 0, x[1]
        a, b = x
        exp, left, a, right = explode(a, n - 1)
        if exp:
            return True, left, [a, add_left(b, right)], None
        exp, left, b, right = explode(b, n - 1)
        if exp:
            return True, None, [add_right(a, left), b], right
        return False, None, x, None

    def split(x):
        if isinstance(x, int):
            if x >= 10:
                return True, [x // 2, math.ceil(x / 2)]
            return False, x
        a, b = x
        change, a = split(a)
        if change:
            return True, [a, b]
        change, b = split(b)
        return change, [a, b]

    def add(a, b):
        x = [a, b]
        while True:
            change, _, x, _ = explode(x)
            if change:
                continue
            change, x = split(x)
            if not change:
                break
        return x

    def magnitude(x):
        if isinstance(x, int):
            return x
        return 3 * magnitude(x[0]) + 2 * magnitude(x[1])


    numbers = list(map(json.loads, data.splitlines()))
    yield magnitude(functools.reduce(add, numbers))

    yield max(magnitude(add(a, b)) for a, b in itertools.permutations(numbers, 2))


@with_solutions(5231, 14279)
def solve20(data):

    # Trench Map

    data = data.splitlines()
    algorithm = data[0]
    image = collections.defaultdict(int, {(x, y):1 for x,l in enumerate(data[2:]) for y,c in enumerate(l) if c == '#'})

    assert(len(algorithm) == 512)

    pixel_mapping = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 0), (0, 1), (1, -1), (1, 0), (1, 1)]

    def show(image, n):
        for x in range(-n, n):
            for y in range(-n, n):
                print('#' if image[(x, y)] == 1 else '.', end='')
            print()
        print()

    def enhance(image, step):

        maxx = maxy = 0
        minx = miny = float('inf')
        for (x,y) in image.keys():
            if image[(x, y)] == 1:
                maxx, maxy = max(maxx, x), max(maxy, y)
                minx, miny = min(minx, x), min(miny, y)

        boundary = {(x, y) for x in range(minx, maxx+1) for y in range(miny, maxy+1)}

        output_image = collections.defaultdict(int)

        for x in range(minx - 1, maxx + 2):
            for y in range(miny - 1, maxy + 2):
                digits = ''
                for (dx, dy) in pixel_mapping:
                    nx, ny = x + dx, y +  dy
                    if step % 2 == 1:
                        digits += '1' if image[(nx, ny)] == 1 or (nx, ny) not in boundary else '0'
                    else:
                        digits += '1' if image[(nx, ny)] == 1 else '0'
                output_image[(x, y)] = int(algorithm[int(digits, 2)] == '#')

        return output_image

    for n in range(50):
        image = enhance(image, n)
        if n == 1:
            yield sum(image.values())
    yield sum(image.values())


@with_solutions(1073709, 148747830493442)
def solve21(data):

    # Dirac Dice

    data = data.splitlines()
    starting_positions = [None, int(data[0][-1]), int(data[1][-1])]

    # Part 1
    positions = starting_positions[:]
    scores = [None, 0, 0]
    current_player = 1
    die = rolls = 0

    while True:
        for _ in range(3):
            die, rolls = die % 100 + 1, rolls + 1
            positions[current_player] += die
        positions[current_player] = (positions[current_player] - 1) % 10 + 1
        scores[current_player] += positions[current_player]

        if scores[current_player] >= 1000:
            losing_player = 2 if current_player == 1 else 1
            yield scores[losing_player] * rolls
            break

        current_player = 2 if current_player == 1 else 1

    # Part 2
    roll_counts = [0, 0, 0, 1, 3, 6, 7, 6, 3, 1]
    # tuple contains - (p1 position, p2 position, p1 score, p2 score)
    states = {(starting_positions[1], starting_positions[2], 0, 0): 1}
    current_player = 1
    wins = [0, 0, 0]

    while len(states) > 0:

        new_states = collections.defaultdict(int)
        for state in states.keys():
            for roll in range(3, 10):
                position, score = state[current_player - 1], state[current_player + 1]
                position = (position + roll - 1) % 10 + 1
                score += position
                if score >= 21:
                    wins[current_player] += states[state] * roll_counts[roll]
                else:
                    if current_player == 1:
                        new_state = (position, state[1], score, state[3])
                    else:
                        new_state = (state[0], position, state[2], score)
                    new_states[new_state] += states[state] * roll_counts[roll]

        current_player = 2 if current_player == 1 else 1
        states = new_states

    yield max(wins)


@with_solutions(577205, 1197308251666843)
def solve22(data):

    # Reactor Reboot

    steps = []
    for line in data.splitlines():
        words = line.split(' ', 1)
        toggle = words[0] == 'on'
        words = words[1].split(',')
        x1, x2 = [int(v) for v in words[0][2:].split('..')]
        y1, y2 = [int(v) for v in words[1][2:].split('..')]
        z1, z2 = [int(v) for v in words[2][2:].split('..')]

        steps.append((toggle, range(x1, x2 + 1), range(y1, y2 + 1), range(z1, z2 + 1)))

    def subrange(r, low = -50, high = 50):
        first, last = r[0], r[-1]
        if last < low:
            return []
        elif first > high:
            return []
        first = min(max(first, low), high)
        last = min(max(last, low), high)
        return range(first, last + 1)

    # Part 1
    cubes = {}
    for toggle, rx, ry, rz in steps:
        for x in subrange(rx):
            for y in subrange(ry):
                for z in subrange(rz):
                    cubes[(x, y, z)] = toggle

    yield sum(1 for v in cubes.values() if v)

    # Part 2

    def process_cube(rx, ry, rz, rest):

        volume = len(rx) * len(ry) * len(rz)

        intersections = []
        for (_, orx, ory, orz) in rest:
            nrx = subrange(orx, rx[0], rx[-1])
            nry = subrange(ory, ry[0], ry[-1])
            nrz = subrange(orz, rz[0], rz[-1])

            if len(nrx) == 0 or len(nry) == 0 or len(nrz) == 0:
                continue

            intersections.append((None, nrx, nry, nrz))

        for idx, intersection in enumerate(intersections):
            _, rx, ry, rz = intersection
            volume -= process_cube(rx, ry, rz, intersections[idx+1:])

        return volume

    total = 0
    for idx, (toggle, rx, ry, rz) in enumerate(steps):
        if not toggle:
            continue
        total += process_cube(rx, ry, rz, steps[idx+1:])
    yield total


################################################################################

if __name__ == '__main__':

    if len(sys.argv) > 1 and sys.argv[1] == 'test':
        test()
        sys.exit(0)
    sys.exit(main())
