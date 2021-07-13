#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re
import time
from pprint import pprint
from datetime import datetime

import collections
import functools
import hashlib

import requests

YEAR = 2016

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

custom_data = False

def with_solutions(*expected):
    def wrapper(f):
        error_msg = 'Incorrect solution for Part {}: Expected "{}", Actual "{}"'
        def wrapped_method(*args, **kwargs):
            fgen = f(*args)
            try:
                for index in range(2):
                    actual = next(fgen)
                    if not custom_data and expected[index] is not None:
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

DIRECTIONS = { '↑' : (( 0, -1), {'L':'←', 'R':'→'}),
               '↓' : (( 0,  1), {'L':'→', 'R':'←'}),
               '←' : ((-1,  0), {'L':'↓', 'R':'↑'}),
               '→' : (( 1,  0), {'L':'↑', 'R':'↓'}),
               '↖' : ((-1, -1), {'L':'←', 'R':'↑'}),
               '↗' : (( 1, -1), {'L':'↑', 'R':'→'}),
               '↘' : (( 1,  1), {'L':'→', 'R':'↓'}),
               '↙' : ((-1,  1), {'L':'↓', 'R':'←'})}

def move(point, direction):
    (dx, dy) = DIRECTIONS[direction][0]
    return (point[0] + dx, point[1] + dy)

def turn(direction, turn):
    return DIRECTIONS[direction][1][turn]

def turn_and_move(position, turn, steps = 1):
    point, direction = position
    new_direction = DIRECTIONS[direction][1][turn]
    for _ in range(steps):
        point = move(point, new_direction)

    return (point, new_direction)

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
    print()



################################################################################
# Solvers

@with_solutions(242, 150)
def solve1(data):

    # No Time for a Taxicab

    for line in data.split('\n'):
        # Part 1
        position = (0, 0), '↑'
        for step in line.split(', '):
            position = turn_and_move(position, step[0], int(step[1:]))
        yield manhattan(position[0])

        # Part 2
        position = ((0, 0), '↑')
        memory = set()
        for step in line.split(', '):
            point, direction = position
            for _ in range(int(step[1:])):
                direction = turn(position[1], step[0])
                point = move(point, direction)
                if point in memory:
                    yield manhattan(point)
                memory.add(point)
            position = (point, direction)


@with_solutions('97289', '9A7DC')
def solve2(data):

    # Bathroom Security

    basic_grid = [[None, None, None, None, None],
                  [None, '1',  '2',  '3',  None],
                  [None, '4',  '5',  '6',  None],
                  [None, '7',  '8',  '9',  None],
                  [None, None, None, None, None]]

    fancy_grid = [[None, None, None, None, None, None, None],
                  [None, None, None, '1',  None, None, None],
                  [None, None, '2',  '3',  '4',  None, None],
                  [None, '5',  '6',  '7',  '8',  '9',  None],
                  [None, None, 'A',  'B',  'C',  None, None],
                  [None, None, None, 'D',  None, None, None],
                  [None, None, None, None, None, None, None]]

    data = data.split('\n')

    def get_code(grid, start):
        x, y = start
        code = []
        for line in data:
            for instr in line:
                if instr == 'U':
                    nx, ny = x, y - 1
                elif instr == 'D':
                    nx, ny = x, y + 1
                elif instr == 'L':
                    nx, ny = x - 1, y
                elif instr == 'R':
                    nx, ny = x + 1, y

                if grid[nx][ny] is not None:
                    x, y = nx, ny
            code.append(grid[y][x])

        return ''.join(code)

    yield get_code(basic_grid, (2, 2))
    yield get_code(fancy_grid, (1, 3))


@with_solutions(917, 1649)
def solve3(data):

    # Squares With Three Sides

    data = data.split('\n')

    def valid_triangle(a, b, c):
        if a + b <= c:
            return False
        if a + c <= b:
            return False
        if b + c <= a:
            return False
        return True

    # Part 1
    count = 0
    for line in data:
        a, b, c = [int(v) for v in line.split(' ') if v != '']
        if valid_triangle(a, b, c):
            count += 1

    yield count

    # Part 2
    def get_triangles():
        lines = []
        for line in data:
            lines.append([int(v) for v in line.split(' ') if v != ''])
        idx = 0
        while idx < len(lines):
            for c in range(3):
                yield (lines[idx][c], lines[idx+1][c], lines[idx+2][c])
            idx += 3

    count = 0
    for a,b,c in get_triangles():
        if valid_triangle(a, b, c):
            count += 1

    yield count


@with_solutions(278221, 267)
def solve4(data):

    # Security Through Obscurity

    def parse_room(line):

        if m := re.match('([a-z-]+)(\d+)\[([a-z]+)\]', line):
            room_id, sector_id, checksum = m.groups()
            room_id = ''.join(room_id.split('-'))
            return room_id, int(sector_id), checksum

    def calculate_checksum(room_id):

        def cmp2(x, y):
            if x[1] == y[1]:
                return ord(y[0]) - ord(x[0])
            else:
                return x[1] - y[1]

        counter = collections.defaultdict(int)
        for char in room_id:
            counter[char] += 1
        counts = list(counter.items())
        counts.sort(key = functools.cmp_to_key(cmp2), reverse = True)

        return ''.join([k[0] for k in counts[:5]])

    def decrypt(room_id, sector_id):
        plaintext = []
        for char in room_id:
            shifted_char = ((ord(char) - 97) + (sector_id % 26)) % 26
            plaintext.append(chr(shifted_char + 97))

        return ''.join(plaintext)

    valid = 0
    result = None
    for line in data.split('\n'):
        room_id, sector_id, checksum = parse_room(line)
        if checksum == calculate_checksum(room_id):
            valid += sector_id

            room_name = decrypt(room_id, sector_id)
            if 'northpole' in room_name:
                result = sector_id

    yield valid
    yield result


@with_solutions('f77a0e6e', '999828ec')
def solve5(data):

    # How About a Nice Game of Chess?

    passwords = [[], [None for _ in range(8)]]

    def print_passwords():
        print(' ' * 80, end='\r', flush=True)
        p = ''.join(passwords[0])
        print(f'{p:8}', end='\t', flush=True)
        print(''.join([c and c or '-' for c in passwords[1]]), flush=True, end='')

    index = 0
    for _ in range(8):
        while True:
            hashval = hashlib.md5(f'{data}{index}'.encode('ascii')).hexdigest()
            index += 1
            if hashval.startswith('00000'):
                if len(passwords[0]) < 8:
                    passwords[0].append(hashval[5])
                    print_passwords()
                if hashval[5] in '01234567':
                    idx = int(hashval[5])
                    if passwords[1][idx] is None:
                        passwords[1][idx] = hashval[6]
                        print_passwords()

            if len(passwords[0]) == 8 and None not in passwords[1]:
                break

    print()
    yield ''.join(passwords[0])
    yield ''.join(passwords[1])

################################################################################

if __name__ == '__main__':

    if len(sys.argv) > 1:
        day = int(sys.argv[1])
    else:
        print('Usage: %s <day>')
        sys.exit(2)

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
        solver(data)
        end = time.time()
        elapsed = (end - start)
        if elapsed > 0.001:
            print('Elapsed: %4.3f s' % elapsed)
        else:
            elapsed *= 1000.0
            print('Elapsed: %4.3f us' % elapsed)
    else:
        print('No solver for day {}'.format(day))
