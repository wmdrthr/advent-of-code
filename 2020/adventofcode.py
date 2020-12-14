#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re
import time
from pprint import pprint
from datetime import datetime, timedelta

import itertools
import collections
import functools

import pytz
import requests

YEAR = 2020

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

        if not os.path.exists('inputs'):
            os.mkdir('inputs')

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

def format_elapsed_time(elapsed):
    for unit in ['ns', 'us', 'ms', 's']:
        if elapsed > 1000:
            elapsed /= 1000
            continue
        return f'Elapsed: {elapsed:4.3f} {unit}'


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

def main():

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
        start = time.monotonic_ns()
        solver(data, skip_verification=custom_data)
        end = time.monotonic_ns()
        elapsed = (end - start)
        print(format_elapsed_time(elapsed))
    else:
        print('No solver for day {}'.format(day))


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


@with_solutions(202, 137)
def solve4(data):

    # Passport Processing

    passports = []
    passport = {}
    for line in data.split('\n'):
        if line.strip() == '':
            if len(passport) > 0:
                passports.append(passport)
                passport = {}
            continue
        for field in line.strip().split(' '):
            key, val = field.split(':')
            passport[key] = val
    if len(passport) > 0:
        passports.append(passport)

    hcl_regex = re.compile(r'^#[0-9a-f]{6}$')
    pid_regex = re.compile(r'^\d{9}$')
    def valid_height(v):
        if v[-2:] == 'cm':
            return 150 <= int(v[:-2]) <= 193
        elif v[-2:] == 'in':
            return 59 <= int(v[:-2]) <= 76
        else:
            return False
    validators = {
        'byr': lambda v: 1920 <= int(v) <= 2002,
        'iyr': lambda v: 2010 <= int(v) <= 2020,
        'eyr': lambda v: 2020 <= int(v) <= 2030,
        'hgt': valid_height,
        'hcl': lambda v: hcl_regex.match(v) is not None,
        'ecl': lambda v: v in {'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'},
        'pid': lambda v: pid_regex.match(v) is not None
    }

    def valid_passport(passport, validators=None):

        for key in ['byr', 'iyr', 'eyr', 'hgt',
                    'hcl', 'ecl', 'pid']:
            if key not in passport:
                return False
            if validators is None:
                continue
            if not validators[key](passport[key]):
                return False
        return True

    yield len(list(filter(valid_passport, passports)))

    count = 0
    for passport in passports:
        if valid_passport(passport, validators):
            count += 1
    yield count


@with_solutions(933, 711)
def solve5(data):

    # Binary Boarding

    passes = data.split('\n')

    table = str.maketrans('FBLR', '0101')
    seats = [False for _ in range(1024)]

    def seatid(bpass):
        seatid = int(bpass.translate(table), 2)
        seats[seatid] = True
        return seatid

    yield max([seatid(bpass) for bpass in passes])

    for seat in range(1024):
        if not seats[seat] and seats[seat - 1] and seats[seat + 1]:
            yield seat
            break


@with_solutions(6742, 3447)
def solve6(data):

    # Custom Customs

    anyone = everyone = 0
    for group_data in data.split('\n\n'):
        counter = collections.defaultdict(int)
        responses = group_data.split('\n')
        for response in responses:
            for answer in response:
                counter[answer] += 1

        anyone += len([k for k,v in counter.items() if v > 0])
        everyone += len([k for k,v in counter.items() if v == len(responses)])

    yield anyone
    yield everyone


@with_solutions(248, 57281)
def solve7(data):

    # Handy Haversacks

    rules = collections.defaultdict(dict)
    for line in data.split('\n'):
        parent, children = line.split(' contain ')
        parent = parent[:-5]
        if children == 'no other bags.':
            rules[parent] = {}
        else:
            for child in children.split(','):
                count, colora, colorb, _ = child.strip().split(' ', 4)
                count = int(count)
                rules[parent][f'{colora} {colorb}'] = count

    def bagcheck(color):
        if 'shiny gold' in rules[color]:
            return True
        for child in rules[color]:
            if bagcheck(child):
                return True
        return False

    def bagcount(color):
        count = 0
        for child, childcount in rules[color].items():
            count += childcount + (childcount * bagcount(child))
        return count

    yield len([color for color in rules.keys() if bagcheck(color)])
    yield bagcount('shiny gold')


@with_solutions(1915, 944)
def solve8(data):

    # Handheld Halting

    def execute(program):
        acc = 0
        counter = [0 for _ in range(len(program))]
        idx = 0
        while True:
            if idx == len(program):
                return (True, acc)
            if counter[idx] > 0:
                return (False, acc)

            counter[idx] += 1
            instr, val = program[idx].split(' ')
            val = int(val)
            if instr == 'acc':
                acc += val
                idx += 1
            elif instr == 'jmp':
                idx += val
            elif instr == 'nop':
                idx += 1

    # Part 1
    program = data.split('\n')
    _, res = execute(program)
    yield res

    # Part 2
    for idx, line in enumerate(program):
        if line[:3] == 'acc':
            continue

        fixed_program = program[:]
        instr, val = line.split(' ')
        if instr == 'nop':
            fixed_step = f'jmp {val}'
        else:
            fixed_step = f'nop {val}'
        fixed_program[idx] = fixed_step

        halting, res = execute(fixed_program)
        if halting:
            yield res
            break


@with_solutions(258585477, 36981213)
def solve9(data):

    # Encoding Error

    numbers = [int(l) for l in data.split('\n')]

    target = 0

    window = (0, 25)
    while True:
        sums = set()
        for x,y in itertools.combinations(numbers[window[0]:window[1]], 2):
            sums.add(x+y)
        if numbers[window[1]] not in sums:
            target = numbers[window[1]]
            yield target
            break
        window = (window[0]+1,window[1]+1)

    window = (0,1)
    partial = numbers[0]
    while True:
        if partial == target:
            values = sorted(numbers[window[0]:window[1]])
            yield values[0] + values[-1]
        elif partial < target:
            partial += numbers[window[1]]
            window = (window[0], window[1]+1)
        elif partial > target:
            partial -= numbers[window[0]]
            window = (window[0]+1, window[1])


@with_solutions(2590, 226775649501184)
def solve10(data):

    # Adapter Array

    ratings = set([int(r) for r in data.split('\n')])
    max_rating = max(ratings)
    builtin_rating = max_rating + 3

    differences = []
    current = 0
    while current < max_rating:
        for difference in (1,2,3):
            if (current + difference) in ratings:
                differences.append(difference)
                break
        current += difference
    differences.append(builtin_rating - current)

    yield differences.count(1) * differences.count(3)

    @functools.lru_cache
    def find_next_adapter(current):
        if current == max_rating:
            return 1
        count = 0
        for difference in (1, 2, 3):
            if (current + difference) in ratings:
                count += find_next_adapter(current + difference)
        return count

    yield find_next_adapter(0)

@with_solutions(2481, 2227)
def solve11(data):

    # Seating System
    data = data.split('\n')
    rows, cols = len(data), len(data[0])
    data = {(x, y):1 for y,l in enumerate(data)
            for x,c in enumerate(l) if c == 'L'}
    grid = collections.defaultdict(int, data)

    def step(current_grid, neighbor_function, max_occupancy):
        newgrid = collections.defaultdict(int, current_grid)
        changes = 0
        for y in range(rows):
            for x in range(cols):
                point = (x, y)
                if current_grid[point] == 1:
                    for (_, p) in neighbor_function(current_grid, point, rows, cols):
                        if current_grid[p] == 2:
                            break
                    else:
                        newgrid[point] = 2
                        changes += 1
                elif grid[point] == 2:
                    if sum([1 for (_, p) in neighbor_function(current_grid, point, rows, cols)
                            if current_grid[p] == 2]) >= max_occupancy:
                        newgrid[point] = 1
                        changes += 1
        return newgrid, changes

    # Part 1
    while True:
        grid, changes = step(grid, neighbors, 4)
        if changes == 0:
            yield len([p for p,v in grid.items() if v == 2])
            break

    # Part 2
    grid = collections.defaultdict(int, data)
    while True:
        grid, changes = step(grid, raytraced_neighbors, 5)
        if changes == 0:
            yield len([p for p,v in grid.items() if v == 2])
            break

@with_solutions(439, 12385)
def solve12(data):

    # Rain Risk

    compass_directions = {'N':'↑', 'S':'↓', 'E':'→', 'W':'←'}

    # Part 1
    heading, position = compass_directions['E'], ORIGIN
    for line in data.split('\n'):
        command = line[0]
        value = int(line[1:])

        if command == 'F':
            position = move(position, heading, value)
        elif command in 'NSEW':
            position = move(position, compass_directions[command], value)
        elif command in 'LR':
            heading = turn(heading, command, value)
        else:
            raise Exception("invalid input")

    print(manhattan(position))

    # Part 2
    heading, position = compass_directions['E'], ORIGIN
    waypoint = (10, -1)
    for line in data.split('\n'):
        command = line[0]
        value = int(line[1:])

        if command == 'F':
            position = move(position, compass_directions['E'], waypoint[0] * value)
            position = move(position, compass_directions['N'], waypoint[1] * value)
        elif command in 'NSEW':
            waypoint = move(waypoint, compass_directions[command], value)
        elif command in 'LR':
            waypoint = rotate(waypoint, command, value)
        else:
            raise Exception("invalid input")

    print(manhattan(position))

@with_solutions(3606, 379786358533423)
def solve13(data):

    # Shuttle Search

    data = data.split('\n')
    start_ts = int(data[0])
    buses = [int(x) for x in data[1].split(',') if x != 'x']

    def shortest_delay():
        for ts in itertools.count(start_ts):
            for bus in buses:
                if (ts + 1) % bus == 0:
                    delay = (ts + 1) - start_ts
                    return delay * bus

    yield shortest_delay()

    def chinese_remainder(pairs):
        total = 0
        product = 1
        for _,busid in pairs:
            product *= busid
        for idx, busid in pairs:
            d = product // busid
            total += idx * d * pow(d, busid - 2, busid)
            total %= product
        return total

    buses = [(int(n) - i, int(n)) for (i, n) in enumerate(data[1].split(',')) if n != 'x']

    yield chinese_remainder(buses)

@with_solutions(14925946402938, 3706820676200)
def solve14(data):

    # Docking Data

    memory = collections.defaultdict(int)

    # Part 1
    and_mask = or_mask = None
    for line in data.split('\n'):
        if line.startswith('mask'):
            line = line.strip()
            or_mask = int(line[7:].replace('X', '0'), 2)
            and_mask = int(line[7:].replace('X', '1'), 2)
        else:
            address, value = line.split(' = ')
            address = int(address[4:-1])
            value = int(value)
            value = (value | or_mask) & and_mask
            memory[address] = value

    def floating_addresses(pos, mask):
        if not mask:
            yield 0
        else:
            if mask[-1] == '0':
                for a in floating_addresses(pos // 2, mask[:-1]):
                    yield 2*a + pos % 2
            elif mask[-1] == '1':
                for a in floating_addresses(pos // 2, mask[:-1]):
                    yield 2*a + 1
            elif mask[-1] == 'X':
                for a in floating_addresses(pos // 2, mask[:-1]):
                    yield 2*a + 0
                    yield 2*a + 1

    yield sum(memory.values())

    # Part 2
    memory.clear()
    mask = None
    for line in data.split('\n'):
        if line.startswith('mask'):
            line = line.strip()
            mask = line[7:].strip()
        else:
            address, value = line.split(' = ')
            address = int(address[4:-1])
            value = int(value)
            for addr in floating_addresses(address, mask):
                memory[addr] = value

    print(sum(memory.values()))


################################################################################

if __name__ == '__main__':

    main()
