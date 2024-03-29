#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re
import time
from pprint import pprint
from datetime import datetime, timedelta

import itertools
import collections
import functools
import math

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
        return total % product

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

    yield sum(memory.values())

    # Part 2
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


@with_solutions(706, 19331)
def solve15(data):

    # Rambunctious Recitation

    numbers = [int(v) for v in data.split(',')]

    def memory_game(limit):
        history = {n:i for i,n in enumerate(numbers)}
        last_number = numbers[-1]
        for turn in range(len(numbers), limit):
            try:
                last_turn = history[last_number]
                number = turn - last_turn - 1
            except KeyError:
                number = 0
            history[last_number] = turn - 1
            last_number = number

        return number

    yield memory_game(2020)
    yield memory_game(30000000)


@with_solutions(23954, 453459307723)
def solve16(data):

    # Ticket Translation

    data = data.split('\n')

    rules = collections.defaultdict(set)

    idx = 0
    while True:
        line = data[idx].strip()
        if line == '':
            break
        field, values = line.split(':')
        for rule in values.split(' or '):
            a,b = [int(v) for v in rule.strip().split('-')]
            rules[field].update(x for x in range(a, b+1))
        idx += 1

    idx += 2
    line = data[idx].strip()
    my_ticket = [int(v) for v in line.split(',')]

    tickets = []
    idx += 3
    while idx < len(data):
        line = data[idx].strip()
        tickets.append([int(v) for v in line.split(',')])
        idx += 1

    # Part 1
    all_valid_values = {x for value in rules.values() for x in value}
    invalid_values = [t for ticket in tickets for t in ticket if t not in all_valid_values]
    yield sum(invalid_values)

    # Part 2
    valid_tickets = [ticket for ticket in tickets if set(ticket).issubset(all_valid_values)]

    matched_rules = [None] * len(my_ticket)
    while None in matched_rules:
        for rule_key, rule_values in rules.items():
            if rule_key in matched_rules:
                continue
            possible_rules = set()
            for idx, key in enumerate(matched_rules):
                if key is not None:
                    continue
                for ticket in valid_tickets:
                    if ticket[idx] not in rule_values:
                        break
                else:
                    possible_rules.add(idx)
            if len(possible_rules) == 1:
                matched_rules[possible_rules.pop()] = rule_key
                break

    values = [my_ticket[idx] for idx,name in enumerate(matched_rules) if name.startswith('departure')]
    yield math.prod(values)


@with_solutions(391, 2264)
def solve17(data):

    # Conway Cubes

    def step(active_cubes, dimensions):
        neighbors = collections.defaultdict(int)
        for cube in active_cubes:
            for offset in itertools.product(range(-1,2), repeat=dimensions):
                if offset == (0,) * dimensions:
                    continue
                neighbors[tuple(x + y for x, y in zip(cube, offset))] += 1
        new_active_cubes = set()
        for cube, count in neighbors.items():
            if cube in active_cubes:
                if count == 2 or count == 3:
                    new_active_cubes.add(cube)
            elif count == 3:
                new_active_cubes.add(cube)

        return new_active_cubes

    cubes = {(x, y, 0) for y,l in enumerate(data.split('\n'))
            for x,c in enumerate(l) if c == '#'}
    for _ in range(6):
        cubes = step(cubes, 3)

    yield len(cubes)

    cubes = {(x, y, 0, 0) for y,l in enumerate(data.split('\n'))
            for x,c in enumerate(l) if c == '#'}
    for _ in range(6):
        cubes = step(cubes, 4)

    yield len(cubes)


@with_solutions(11297104473091, 185348874183674)
def solve18(data):

    # Operation Order

    def simple_evaluate_ltr(expr):

        elements = expr.split()
        result = int(elements[0])
        idx = 1
        while idx < len(elements):
            if elements[idx] == '+':
                idx += 1
                result += int(elements[idx])
            elif elements[idx] == '*':
                idx += 1
                result *= int(elements[idx])
            idx += 1

        return result

    def simple_evaluate_am(expr):

        elements = expr.split()
        while '+' in elements:
            idx = elements.index('+')
            result = int(elements[idx-1]) + int(elements[idx+1])
            elements[idx-1:idx+2] = [str(result)]

        while '*' in elements:
            idx = elements.index('*')
            result = int(elements[idx-1]) * int(elements[idx+1])
            elements[idx-1:idx+2] = [str(result)]

        return int(elements[0])


    def paren_evaluate(expr, evaluate):
        if '(' not in expr:
            return evaluate(expr)

        start = expr.index('(')
        count = 0
        end = 0
        for end in range(start+1, len(expr)):
            if expr[end] == ')' and count == 0:
                break
            elif expr[end] == ')' and count > 0:
                count -= 1
            elif expr[end] == '(':
                count += 1
        return paren_evaluate(expr[:start] +
                              str(paren_evaluate(expr[start+1:end], evaluate)) +
                              expr[end+1:],
                              evaluate)


    sum1 = sum2 = 0
    for expr in data.split('\n'):
        sum1 += paren_evaluate(expr, simple_evaluate_ltr)
        sum2 += paren_evaluate(expr, simple_evaluate_am)
    yield sum1
    yield sum2


@with_solutions(203, 304)
def solve19(data):

    # Monster Messages

    rules = {}
    terminals = {}
    messages = []
    cache = {}

    for line in data.split('\n'):
        if ':' in line:
            key, rule = line.split(': ')
            if rule in ('"a"', '"b"'):
                terminals[key] = rule[1]
            else:
                options = rule.split(' | ')
                rules[key] = [r.split(' ') for r in options]
        elif line:
            messages.append(line)

    def match_subrule(message, start, end, subrules):

        if start == end and not subrules:
            return True
        if start == end or not subrules:
            return False

        result = False
        for idx in range(start+1, end+1):
            if match(message, start, idx, subrules[0]) and\
               match_subrule(message, idx, end, subrules[1:]):
                result = True

        return result

    def match(message, start, end, rule):
        key = (start, end, rule)
        if key in cache:
            return cache[key]

        result = False
        if rule in terminals:
            result = (message[start:end] == terminals[rule])
        else:
            for option in rules[rule]:
                if match_subrule(message, start, end, option):
                    result = True

        cache[key] = result
        return result

    def check():
        total = 0
        for message in messages:
            cache.clear()
            if match(message, 0, len(message), '0'):
                total += 1
        return total

    # Part 1
    yield check()

    # Part 2
    rules['8'] = [['42'], ['42', '8']]
    rules['11'] = [['42', '31'], ['42', '11', '31']]

    yield check()


@with_solutions(15006909892229, 2190)
def solve20(data):

    # Jurassic Jigsaw

    ROWS, COLS = 10, 10

    TILES = {}
    for lines in data.split('\n\n'):
        tileid = lines[5:lines.find(':')]
        TILES[tileid] = [list(l) for l in lines.split('\n')[1:]]

    EDGES = {}
    for tileid, tilegrid in TILES.items():
        left, right, top, bottom = [], [], [], []
        for idx in range(ROWS):
            left.append(tilegrid[idx][0])
            right.append(tilegrid[idx][COLS-1])
            top.append(tilegrid[0][idx])
            bottom.append(tilegrid[ROWS-1][idx])
        edges = [tuple(e) for e in [left, right, top, bottom]]
        EDGES[tileid] = set([e for e in edges] + [tuple(reversed(e)) for e in edges])

    # Part 1
    adjacent = collections.defaultdict(set)
    for ((t1,e1),(t2,e2)) in itertools.product(EDGES.items(), repeat=2):
        if t1 == t2:
            continue
        if e1 & e2: # if edge sets of any 2 tiles intersect
            adjacent[t1].add(t2)

    product = 1
    for tileid,_ in EDGES.items():
        if len(adjacent[tileid]) == 2:
            product *= int(tileid)

    yield product


@with_solutions(2389,'fsr,skrxt,lqbcg,mgbv,dvjrrkv,ndnlm,xcljh,zbhp')
def solve21(data):

    # Allergen Assessment

    foods = []
    all_ingredients, all_allergens = set(), set()
    for line in data.split('\n'):
        ingredients, allergens = line.split('(contains ')
        ingredients = set(ingredients.split())
        allergens = set(allergens[:-1].split(', '))
        foods.append((ingredients, allergens))
        all_ingredients |= set(ingredients)
        all_allergens |= set(allergens)

    candidates = {i:set(all_allergens) for i in all_ingredients}
    counter = collections.defaultdict(int)
    for ingredients, allergens in foods:
        for ingredient in ingredients:
            counter[ingredient] += 1

        for allergen in allergens:
            for ingredient in all_ingredients:
                if ingredient not in ingredients:
                    candidates[ingredient].discard(allergen)

    # Part 1
    total = 0
    for ingredient in all_ingredients:
        if len(candidates[ingredient]) == 0:
            total += counter[ingredient]
    yield total

    # Part 2
    allergen_map = {}
    used = set()

    while len(allergen_map) < len(all_allergens):
        for ingredient in all_ingredients:
            possible = [a for a in candidates[ingredient] if a not in used]
            if len(possible) == 1:
                allergen_map[ingredient] = possible[0]
                used.add(possible[0])
    dangerous_ingredients = [k for k,v in sorted(allergen_map.items(), key=lambda kv:kv[1])]
    yield ','.join(dangerous_ingredients)


@with_solutions(31455, 32528)
def solve22(data):

    # Crab Combat

    hands = data.split('\n\n')
    decks = [collections.deque([int(v) for v in hands[0].split('\n')[1:]]),
             collections.deque([int(v) for v in hands[1].split('\n')[1:]])]

    def score(winner):
        return sum([card * (len(winner) - idx) for idx, card in enumerate(winner)])

    def combat(deck1, deck2):
        winner = None
        while True:
            card1, card2 = deck1.popleft(), deck2.popleft()
            if card1 > card2:
                deck1.extend([card1, card2])
            else:
                deck2.extend([card2, card1])

            if len(deck1) == 0:
                return deck2
            elif len(deck2) == 0:
                return deck1

    def copy_deck(deck, n):
        return collections.deque(list(deck)[:n])

    def recursive_combat(deck1, deck2):
        history = set()

        while len(deck1) > 0 and len(deck2) > 0:
            round_hash = (tuple(deck1), tuple(deck2))
            if round_hash in history:
                return 1, deck1, deck2
            else:
                history.add(round_hash)

            card1, card2 = deck1.popleft(), deck2.popleft()
            winner = 0

            if len(deck1) >= card1 and len(deck2) >= card2:
                winner, _, _ = recursive_combat(copy_deck(deck1, card1), copy_deck(deck2, card2))
            elif card1 > card2:
                winner = 1
            else:
                winner = 2

            if winner == 1:
                deck1.extend([card1, card2])
            else:
                deck2.extend([card2, card1])

        if len(deck1) == 0:
            return 2, deck1, deck2
        else:
            return 1, deck1, deck2

    yield score(combat(*decks))

    decks = [collections.deque([int(v) for v in hands[0].split('\n')[1:]]),
             collections.deque([int(v) for v in hands[1].split('\n')[1:]])]
    winner, *finaldecks = recursive_combat(*decks)
    yield score(finaldecks[winner - 1])


@with_solutions('89573246', 2029056128)
def solve23(data):

    # Crab Cups

    cups = [int(v) for v in data]

    def solve(moves):

        number_of_cups = len(cups) if moves == 100 else int(1e6)
        links = [None for _ in range(number_of_cups + 1)]

        for idx, cup in enumerate(cups):
            links[cups[idx]] = cups[(idx + 1) % len(cups)]
        if moves > 1e6:
            links[cups[-1]] = len(cups) + 1
            for idx in range(len(cups) + 1, number_of_cups + 1):
                links[idx] = idx + 1
            links[-1] = cups[0]

        current = cups[0]
        for _ in range(moves):
            pickup = links[current]
            links[current] = links[links[links[pickup]]]

            dest = number_of_cups if current == 1 else current - 1
            while dest in (pickup, links[pickup], links[links[pickup]]):
                dest = number_of_cups if dest == 1 else dest - 1

            links[links[links[pickup]]] = links[dest]
            links[dest] = pickup
            current = links[current]

        return links

    # Part 1
    links = solve(100)
    answer, x = [], 1
    while (x := links[x]) != 1:
        answer.append(x)
    yield ''.join([str(x) for x in answer])

    # Part 2
    links = solve(int(1e7))
    yield links[1] * links[links[1]]


@with_solutions(512, 4120)
def solve24(data):

    # Lobby Layout

    origin = (0, 0, 0)
    directions = {'e' :(1, -1, 0), 'w' :(-1, 1, 0),
                  'sw':(-1, 0, 1), 'se':(0, -1, 1),
                  'nw':(0, 1, -1), 'ne':(1, 0, -1)}

    def hexmove(pos, direction):
        x, y, z = pos
        dx, dy, dz = directions[direction]
        return (x + dx, y + dy, z + dz)

    # Part 1
    def traverse(position, instructions):
        if len(instructions) == 0:
            return position

        if instructions[0] in 'ew':
            new_position = hexmove(position, instructions[0])
            instructions = instructions[1:]
        else:
            new_position = hexmove(position, instructions[:2])
            instructions = instructions[2:]

        return traverse(new_position, instructions)

    black_tiles = set()

    for line in data.split('\n'):
        final_position = traverse(origin, line[:])
        if final_position in black_tiles:
            black_tiles.remove(final_position)
        else:
            black_tiles.add(final_position)

    yield len(black_tiles)

    # Part 2
    def step(tileset):
        new_tileset = set()
        check_tiles = set()
        for tile in tileset:
            check_tiles.add(tile)
            check_tiles.update([hexmove(tile, d) for d in directions.keys()])

        for tile in check_tiles:
            count = 0
            for neighbor in [hexmove(tile, d) for d in directions.keys()]:
                if neighbor in tileset:
                    count += 1
            if tile in tileset and count in (1, 2):
                new_tileset.add(tile)
            elif tile not in tileset and count == 2:
                new_tileset.add(tile)
        return new_tileset

    for _ in range(100):
        black_tiles = step(black_tiles)

    yield len(black_tiles)


@with_solutions(17032383, None)
def solve25(data):

    # Combo Breaker

    card_public_key, door_public_key = [int(l) for l in data.split('\n')]

    def transform(subject_number, loop_size = 0, public_key = None):
        value = 1
        for n in itertools.count():
            value = (value * subject_number) % 20201227

            if loop_size > 0 and n == loop_size:
                return value
            if public_key and value == public_key:
                return n + 1

    card_loop_size = transform(7, public_key=card_public_key)
    yield pow(door_public_key, card_loop_size, 20201227)

################################################################################

if __name__ == '__main__':

    main()
