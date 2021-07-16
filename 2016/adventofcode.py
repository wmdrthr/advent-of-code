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



def display_grid(grid, tileset='.#', dimensions=None):
    # Given a dict representing a point grid, print the grid, using
    # the given tileset.

    if dimensions is None:
        max_x = max_y = 0
        min_x = min_y = 65536
        for point in grid.keys():
            min_x = min(min_x, point[0])
            max_x = max(max_x, point[0])
            min_y = min(min_y, point[1])
            max_y = max(max_y, point[1])
    else:
        min_x = min_y = 0
        max_x, max_y = dimensions

    for y in range(min_y, max_y):
        row = []
        for x in range(min_x, max_x):
            row.append(tileset[grid[(x, y)]])
        print(''.join(row))


def occurences(predicate, iterator):
    # Return number of items in iterator for which the predicate
    # returns True
    count = 0

    while True:
        try:
            val = next(iterator)
            if predicate(val):
                count += 1
        except StopIterator:
            pass

    return count


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

@with_solutions('ursvoerv', 'vomaypnn')
def solve6(data):

     # Signals and Noise

    counters = []

    data = data.split('\n')
    for _ in range(len(data[0])):
        counters.append(collections.defaultdict(int))

    for line in data:
        for idx, char in enumerate(line):
            counters[idx][char] += 1

    messages = [[], []]
    for counter in counters:
        counts = list(counter.items())
        counts.sort(key = lambda v: v[1], reverse = True)
        messages[0].append(counts[0][0])
        messages[1].append(counts[-1][0])

    yield ''.join(messages[0])
    yield ''.join(messages[1])


@with_solutions(115, 231)
def solve7(data):

    # Internet Protocol Version 7

    def is_abba(chars):
        for idx in range(0, len(chars) - 3):
            if chars[idx] == chars[idx + 3] and chars[idx + 1] == chars[idx + 2] and chars[idx] != chars[idx + 1]:
                return True
        return False

    def get_hypernet_sequences(addr):
        start = end = 0
        while True:
            start = addr.find('[', end)
            if start == -1:
                break
            end = addr.find(']', start)
            yield addr[start+1:end]

    def supports_tls(addr):
        for hypernet_sequence in get_hypernet_sequences(addr):
            if is_abba(hypernet_sequence):
                return False

        for chars in re.split('\[\w+\]', addr):
            if is_abba(chars):
                return True

    def get_area_broadcast_accessors(chars):
        for idx in range(0, len(chars) - 2):
            if chars[idx] == chars[idx + 2] and chars[idx] != chars[idx + 1]:
                yield chars[idx:idx+3]
        return False

    def byte_allocation_block(area_broadcast_accessor):
        a = area_broadcast_accessor[0]
        b = area_broadcast_accessor[1]
        return f'{b}{a}{b}'

    def supports_ssl(addr):
        hypernet_sequences = list(get_hypernet_sequences(addr))
        supernet_sequences = re.split('\[\w+\]', addr)

        for supernet_sequence in supernet_sequences:
            for aba in get_area_broadcast_accessors(supernet_sequence):
                # try to find the corresponding BAB in all hypernet
                # sequences
                bab = byte_allocation_block(aba)
                for hypernet_sequence in hypernet_sequences:
                    if bab in hypernet_sequence:
                        return True

        return False


    tls = ssl = 0
    for addr in data.split('\n'):
        if supports_tls(addr):
            tls += 1
        if supports_ssl(addr):
            ssl += 1

    yield tls
    yield ssl


@with_solutions(119, 'ZFHFSFOGPO')
def solve8(data):

    # Two-Factor Authentication
    if custom_data:
        LENGTH, WIDTH = 7, 3
    else:
        LENGTH, WIDTH = 50, 6

    instructions = []
    for line in data.split('\n'):
        words = line.split(' ')
        if words[0] == 'rect':
            x, y = [int(v) for v in words[1].split('x')]
            instructions.append(('rect', x, y))
        elif words[0] == 'rotate':
            a, b = int(words[2][2:]), int(words[-1])
            instructions.append((words[0], words[1], a, b))

    display = collections.defaultdict(int)
    for instr in instructions:
        if instr[0] == 'rect':
            x, y = instr[1], instr[2]
            for j in range(y):
                for i in range(x):
                    display[(i, j)] = 1
        elif instr[0] == 'rotate' and instr[1] == 'column':
            column, step = instr[2], instr[3]

            for _ in range(step):
                last = display[(column, WIDTH - 1)]
                for y in range(WIDTH - 1, 0, -1):
                    display[(column, y)] = display[(column, y - 1)]
                display[(column, 0)] = last
        elif instr[0] == 'rotate' and instr[1] == 'row':
            row, step = instr[2], instr[3]

            for _ in range(step):
                last = display[(LENGTH - 1, row)]
                for x in range(LENGTH - 1, 0, -1):
                    display[(x, row)] = display[(x - 1, row)]
                display[(0, row)] = last

        if custom_data:
            print(instr)
            display_grid(display, dimensions=(LENGTH, WIDTH))

    # Part 1
    yield sum([v for v in display.values()])

    # Part 2
    display_grid(display, dimensions=(LENGTH, WIDTH))
    yield 'ZFHFSFOGPO'


@with_solutions(110346, 10774309173)
def solve9(data):

    # Explosives in Cyberspace

    def decompress(s, recurse=False):
        if '(' not in s:
            return len(s)

        count = 0
        while '(' in s:
            start = s.find('(')
            count += start
            s = s[start:]
            marker = s[1:s.find(')')]
            x, y = [int(v) for v in marker.split('x')]
            s = s[s.find(')') + 1:]
            if recurse:
                count += decompress(s[:x], True) * y
            else:
                count += len(s[:x]) * y
            s = s[x:]
        return count + len(s)

    yield decompress(data)
    yield decompress(data, True)


@with_solutions(157, 1085)
def solve10(data):

    # Balance Bots
    if custom_data:
        CHECK_VALS = (2, 5)
    else:
        CHECK_VALS = (17, 61)

    bots = collections.defaultdict(list)
    outputs = collections.defaultdict(list)
    instructions = {}
    targets = {'bot': bots, 'output': outputs}

    for line in data.split('\n'):
        if line.startswith('value'):
            num, bot = [int(v) for v in re.findall(r'\d+', line)]
            bots[bot].append(num)
        elif line.startswith('bot'):
            bot, low, high = [int(v) for v in re.findall(r'\d+', line)]
            trgts = re.findall(r' (bot|output)', line)
            instructions[bot] = (trgts, low, high)

    while len(bots) > 0:
        for bot, vals in dict(bots).items():
            if len(vals) == 2:
                vals = sorted(bots.pop(bot))
                if tuple(vals) == CHECK_VALS:
                    yield bot
                (trgts, low, high) = instructions[bot]
                targets[trgts[0]][low].append(vals[0])
                targets[trgts[1]][high].append(vals[1])

    a, b, c = [outputs[idx][0] for idx in range(3)]
    yield a * b * c


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
