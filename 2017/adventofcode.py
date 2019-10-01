#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re
import json
import errno
import atexit
import string
import time
from pprint import pprint
from datetime import datetime

import itertools
import functools
import operator
import collections
import array
import queue

import networkx

import pytz
import requests

SESSIONID_FILE = '~/.config/adventofcode/session'
SESSIONID      = None
CACHE_FILE     = '~/.config/adventofcode/cache'
USER_AGENT     = 'aocd.py/v0.3.6'
URI = 'http://adventofcode.com/{year}/day/{day}/input'

def get_session_id():
    global SESSIONID

    try:
        sessionid = open(os.path.expanduser(SESSIONID_FILE)).read()
        SESSIONID = sessionid.strip()
    except (OSError, IOError) as err:
        print('Could not load session-id - ', str(err))
        print("""Puzzle inputs differ by user. Log in to the Advent of Code site,
then check your cookies to get the value of session-id. Save this
value in {}""".format(os.path.expanduser(SESSIONID_FILE)))
        sys.exit(3)

def guess_day():
    """
    Most recent day, if it's during the Advent of Code.  Happy Holidays!
    Raises exception otherwise.
    """
    aoc_now = datetime.now(tz=pytz.timezone('Asia/Kolkata'))
    if aoc_now.month != 12:
        raise Exception('guess_day is only available in December')
    day = min(aoc_now.day, 25)
    return day

CACHE = {}
try:
    with open(os.path.expanduser(CACHE_FILE), 'r') as f:
        CACHE = json.load(f)
except (OSError, IOError) as err:
    # if cache file no tfound, do nothing; for all other errors, raise
    # an exception
    if err.errno != errno.ENOENT:
        raise Exception('Problem loading cache', err)

def save_cache():
    with open(os.path.expanduser(CACHE_FILE), 'w') as f:
        json.dump(CACHE, f, sort_keys=True, indent=2)

def get_data(day):
    """
    Get data for day (1-25)
    """
    year = 2017

    uri = URI.format(year=year, day=day)
    key = '{}?session={}'.format(uri, SESSIONID)
    if key not in CACHE:
        response = requests.get(uri,
                                cookies={'session': SESSIONID},
                                headers={'User-Agent': USER_AGENT})
        if response.status_code != 200:
            raise Exception('Unexpected response: [{}] {}'.format(response.status_code, response.content))
        CACHE[key] = response.text
        if not getattr(save_cache, 'registered', False):
            atexit.register(save_cache)
            save_cache.registered = True
    return CACHE[key]

def with_solutions(soln1, soln2):
    def wrapper(f):

        def check(index, expected, actual_g):
            value = next(actual_g)
            if expected is not None:
                if value != expected:
                    print('Incorrect solution for Part {} (expected "{}", actual "{}")'.format(index, expected, value))
                    sys.exit(23)
                print(value)

        def wrapped_method(*args):
            fgen = f(*args)
            check(1, soln1, fgen)
            check(2, soln2, fgen)
        return wrapped_method
    return wrapper

################################################################################
# Solvers

@with_solutions(1136, 1092)
def solve1(data):
    # Inverse Captcha
    n = len(data)
    yield sum(int(a) for a, b in zip(data, data[1:] + data[0]) if a==b)
    n = n // 2
    yield sum(int(a) for a, b in zip(data, data[n:] + data[:n]) if a==b)


@with_solutions(41919, 303)
def solve2(data):
    # Corruption Checksum
    rows = data.split('\n')
    rows = [row.split('\t') for row in rows]
    rows = [sorted(map(int, row), reverse=True) for row in rows]

    yield sum([(row[0] - row[-1]) for row in rows])

    res = 0
    for row in rows:
        for a, b in itertools.combinations(row, 2):
            if a % b == 0:
                res += a // b
    yield res


@with_solutions(480, 349975)
def solve3(data):
    # Spiral Memory
    input = int(data)

    ctr = 3
    while True:
        square = ctr * ctr
        if square >= input:
            break
        ctr += 2

    lesser = abs(input - ((ctr - 1) * (ctr - 1)))
    greater = abs(input - square)

    if lesser < greater:
        yield abs(ctr - 1 - lesser) - 1
    else:
        yield abs(ctr - greater) - 1

    # Part 2 using https://oeis.org/A141481
    oeis_uri = 'https://oeis.org/A141481/b141481.txt'
    response = requests.get(oeis_uri).content.decode('ascii')
    lines = response.splitlines()[2:]

    for line in lines:
        a, b = line.split()
        b = int(b)
        if b > input:
            yield b
            break


@with_solutions(451, 223)
def solve4(data):
    # High-Entropy Passphrases
    passphrases = data.split('\n')

    count = 0
    for phrase in passphrases:
        words = phrase.split()
        wordset = set(words)
        if len(words) == len(wordset):
            count += 1

    yield count

    count = 0
    for phrase in passphrases:
        words = {}
        flag = True
        for word in phrase.split():
            sword = ''.join(sorted(word))
            if sword in words:
                flag = False
                break
            else:
                words[sword] = 1
        if flag:
            count += 1

    yield count


@with_solutions(360603, 25347697)
def solve5(data):
    # A Maze of Twisty Trampolines, All Alike
    offsets = [int(s.strip()) for s in data.split()]

    steps = index = 0
    while True:
        newindex = index + offsets[index]
        offsets[index] += 1
        steps += 1
        if newindex < 0 or newindex >= len(offsets):
            break
        index = newindex

    yield steps

    offsets = [int(s.strip()) for s in data.split()]

    steps = index = 0
    while True:
        newindex = index + offsets[index]
        if offsets[index] >= 3:
            offsets[index] -= 1
        else:
            offsets[index] += 1
        steps += 1
        if newindex < 0 or newindex >= len(offsets):
            break
        index = newindex

    yield steps

    if len(offsets) < 10:
        pprint(offsets)


@with_solutions(14029, 2765)
def solve6(data):
    # Memory Reallocation
    banks = [int(x) for x in data.split()]

    configurations = {}
    history = []

    count = 0
    while True:
        # add current configuration
        config = ','.join([str(x) for x in banks])
        configurations[config] = True
        history.append(config)

        # find largest memory bank
        index, maxv = 0, banks[0]
        for idx, val in enumerate(banks):
            if val > maxv:
                maxv = val
                index = idx

        # empty that bank
        banks[index] = 0

        # distribute the blocks
        while maxv > 0:
            index = (index + 1) % len(banks)
            banks[index] += 1
            maxv -= 1

        count += 1

        # check configuration
        config = ','.join([str(x) for x in banks])
        if config in configurations:
            history.append(config)
            break

    yield count

    index = len(history) - 1
    while index > 0:
        index -= 1
        if history[index] == history[-1]:
            break

    yield len(history) - index - 1


@with_solutions('vgzejbd', 1226)
def solve7(data):
    # Recursive Circus
    disc_regex = re.compile('^(?P<name>[a-z]+)\s\((?P<weight>[\d]+)\)')
    child_regex = re.compile('([a-z]+)')

    class Disc(object):

        discs = {}

        def __init__(self, line):
            match = disc_regex.search(line)
            children = child_regex.findall(line[match.end():])

            self.name = match['name']
            self.weight = int(match['weight'])
            self.children = children

            self._weight = None
            self.parent = None
            self.discs[self.name] = self

        def update(self):
            self.children = [self.discs[c] for c in self.children]
            for child in self.children:
                child.parent = self

        def cweight(self):
            if self._weight is not None:
                return self._weight

            self._weight = self.weight

            for child in self.children:
                self._weight += child.cweight()

            return self._weight

        def balanced(self):

            weights = [child.cweight() for child in self.children]

            return len(weights) == 0 or  weights.count(weights[0]) == len(weights)

        def __repr__(self):
            return self.name

    # create the Disc objects
    for line in data.splitlines():
        disc = Disc(line)

    # set up the parent-child hierarchy
    for disc in Disc.discs.values():
        disc.update()

    # find the root
    while disc.parent is not None:
        disc = disc.parent

    yield disc.name

    # we start from the root, which is obviously unbalanced, and
    # traverse the tree visiting each unbalanced child until we reach
    # a disc with no unbalanced children
    while True:
        unbalanced = [child for child in disc.children if not child.balanced()]
        if len(unbalanced) == 0:
            break
        disc = unbalanced[0]

    # at this point, the current disc is unbalanced, but each of its
    # children is balanced; therefore, one of the children has the
    # wrong weight.
    weights = [child.cweight() for child in disc.children]
    unbalanced, balanced = None, None
    for child in disc.children:
        if balanced is None and weights.count(child.cweight()) > 0:
            # found one of the balanced nodes (any will do)
            balanced = child
            continue
        if weights.count(child.cweight()) == 1:
            # found the child with the wrong weight
            unbalanced = child
            continue

    weight_difference = unbalanced.cweight() - balanced.cweight()
    yield unbalanced.weight - weight_difference


@with_solutions(7787, 8997)
def solve8(data):
    # I Heard You Like Registers
    registers = collections.defaultdict(int)

    def evaluate(condition):
        register, operator, value = condition[3:].split()
        if operator == '>':
            return registers[register] > int(value)
        elif operator == '<':
            return registers[register] < int(value)
        elif operator == '>=':
            return registers[register] >= int(value)
        elif operator == '<=':
            return registers[register] <= int(value)
        elif operator == '==':
            return registers[register] == int(value)
        elif operator == '!=':
            return registers[register] != int(value)
        else:
            raise Exception('Unknown operator %s' % operator)

    maximum = 0
    for line in data.splitlines():
        register, operation, value, condition = line.split(maxsplit=3)
        if evaluate(condition):
            if operation == 'inc':
                registers[register] += int(value)
                maximum = max(maximum, registers[register])
            elif operation == 'dec':
                registers[register] -= int(value)
                maximum = max(maximum, registers[register])

    yield max(registers.values())
    yield maximum


@with_solutions(10050, 4482)
def solve9(data):
    # Stream Processing
    sum = 0
    for line in data.splitlines():
        index = 0
        count = total = score = 0
        garbage = 0
        while index < len(line):
            char = line[index]
            if char == '!':
                index += 2
                continue
            elif char == '{':
                if garbage != 1:
                    score += 1
                else:
                    count += 1
            elif char == '}':
                if garbage != 1:
                    total += score
                    score -= 1
                else:
                    count += 1
            elif char == '<':
                if garbage:
                    count += 1
                garbage = 1
            elif char == '>':
                garbage = 0
            elif garbage:
                count += 1

            index += 1
        sum += total

        if len(line) < 25:
            print('%3d\t%3d\t%s' % (total, count, line))

    yield sum
    yield count

HEX = ['00', '01', '02', '03', '04', '05', '06', '07', '08', '09', '0a', '0b', '0c', '0d', '0e', '0f',
       '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '1a', '1b', '1c', '1d', '1e', '1f',
       '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '2a', '2b', '2c', '2d', '2e', '2f',
       '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '3a', '3b', '3c', '3d', '3e', '3f',
       '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', '4a', '4b', '4c', '4d', '4e', '4f',
       '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '5a', '5b', '5c', '5d', '5e', '5f',
       '60', '61', '62', '63', '64', '65', '66', '67', '68', '69', '6a', '6b', '6c', '6d', '6e', '6f',
       '70', '71', '72', '73', '74', '75', '76', '77', '78', '79', '7a', '7b', '7c', '7d', '7e', '7f',
       '80', '81', '82', '83', '84', '85', '86', '87', '88', '89', '8a', '8b', '8c', '8d', '8e', '8f',
       '90', '91', '92', '93', '94', '95', '96', '97', '98', '99', '9a', '9b', '9c', '9d', '9e', '9f',
       'a0', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'aa', 'ab', 'ac', 'ad', 'ae', 'af',
       'b0', 'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9', 'ba', 'bb', 'bc', 'bd', 'be', 'bf',
       'c0', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'ca', 'cb', 'cc', 'cd', 'ce', 'cf',
       'd0', 'd1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8', 'd9', 'da', 'db', 'dc', 'dd', 'de', 'df',
       'e0', 'e1', 'e2', 'e3', 'e4', 'e5', 'e6', 'e7', 'e8', 'e9', 'ea', 'eb', 'ec', 'ed', 'ee', 'ef',
       'f0', 'f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8', 'f9', 'fa', 'fb', 'fc', 'fd', 'fe', 'ff']

def knothash_reverse(string, start, length):
    """
    reverse([0, 1, 2, 3, 4], 0, 3) = [2, 1, 0, 3, 4]
    reverse([2, 1, 0, 3, 4], 3, 4) = [4, 3, 0, 1, 2]
    reverse([4, 3, 0, 1, 2], 3, 1) = [4, 3, 0, 1, 2]
    reverse([4, 3, 0, 1, 2], 1, 5) = [3, 4, 2, 1, 0]
    """
    end = (start + length - 1) % len(string)
    length = length // 2
    while length > 0:
        try:
            string[start], string[end] = string[end], string[start]
            start = (start + 1) % len(string)
            end -= 1
            if end < 0:
                end = len(string) - 1
            length -= 1
        except IndexError:
            print(start, end, length)
            raise
    return string

def knothash_round(string, lengths, current = 0, skip = 0):
    for length in lengths:
        string = knothash_reverse(string, current, length)
        current += (length + skip)
        current %= 256
        skip += 1
    return (string, current, skip)


def knothash(input_data):

    lengths = [ord(x) for x in input_data] + [17, 31, 73, 47, 23]
    current = skip = 0
    string = [x for x in range(0, 256)]

    for r in range(64):
        string, current, skip = knothash_round(string, lengths, current, skip)

    blocks = [iter(string)] * 16
    dense_hash = []
    for block in itertools.zip_longest(*blocks):
        dense_hash.append(functools.reduce(operator.xor, block))

    return ''.join([HEX[x] for x in dense_hash])


@with_solutions(23874, 'e1a65bfb5a5ce396025fab5528c25a87')
def solve10(data):
    # Knot Hash

    # Part 1
    string = [x for x in range(0, 256)]
    lengths = [int(x) for x in data.split(',')]
    current = skip = 0

    string, _, _ = knothash_round(string, lengths)
    yield string[0] * string[1]

    # Part 2
    yield knothash(data)


@with_solutions(650, 1465)
def solve11(data):
    # Hex Ed

    cube_directions = {
        'n'  : (0, 1, -1),
        'ne' : (1, 0, -1),
        'se' : (1, -1, 0),
        's'  : (0, -1, 1),
        'sw' : (-1, 0, 1),
        'nw' : (-1, 1, 0)
        }

    def cube_move(cube, direction):
        i, j, k = cube_directions[direction]
        a, b, c = cube
        return (a + i, b + j, c + k)

    def cube_distance(a, b):
        return max(abs(a[0] - b[0]),
                   abs(a[1] - b[1]),
                   abs(a[2] - b[2]))

    def _solve(data):
        origin = (0, 0, 0)
        current = origin
        distances = []
        for step in data.split(','):
            current = cube_move(current, step)
            distances.append(cube_distance(current, origin))
        return (cube_distance(current, origin), max(distances))

    a, b = _solve(data)
    yield a
    yield b


@with_solutions(141, 171)
def solve12(data):
    # Digital Plumber

    G = networkx.Graph()
    for line in data.splitlines():

        head, nodes = line.split('<->')
        head = int(head)
        nodes = [int(n) for n in nodes.split(',')]
        for node in nodes:
            G.add_edge(head, node)
            G.add_edge(node, head)

    connected = list(networkx.connected_components(G))
    yield len(connected[0])
    yield len(connected)


@with_solutions(632, 3849742)
def solve13(data):
    # Packet Scanners

    lines = [line.split(':') for line in data.splitlines()]
    firewall = {int(pos): int(height) for pos, height in lines}

    def scanner(layer, time):
        offset = time % ((layer - 1) * 2)
        return 2 * (layer - 1) - offset if offset > layer - 1 else offset

    severity = 0
    for position in firewall:
        if scanner(firewall[position], position) == 0:
            severity += position * firewall[position]
    yield severity

    for delay in itertools.count():
        result = (scanner(firewall[pos], delay + pos) == 0 for pos in firewall)
        if not any(result):
            break
    yield delay


@with_solutions(8316, 1074)
def solve14(data):
    # Disk Defragmentation

    BITS = { '0': (0, [False, False, False, False]),
             '1': (1, [False, False, False,  True]),
             '2': (1, [False, False,  True, False]),
             '3': (2, [False, False,  True,  True]),
             '4': (1, [False, True,  False, False]),
             '5': (2, [False,  True, False,  True]),
             '6': (2, [False,  True,  True, False]),
             '7': (3, [False, True,  True,   True]),
             '8': (1, [True,  False, False, False]),
             '9': (2, [ True, False, False,  True]),
             'a': (2, [ True, False,  True, False]),
             'b': (3, [ True, False,  True,  True]),
             'c': (2, [ True,  True, False, False]),
             'd': (3, [ True,  True, False,  True]),
             'e': (3, [ True,  True,  True, False]),
             'f': (4, [ True,  True,  True,  True])}

    bitmap = []
    total = 0
    for n in range(128):
        input_data = '%s-%d' % (data, n)
        kh = knothash(input_data)
        bitmap.append([])
        for digit in kh:
            total += BITS[digit][0]
            for bit in BITS[digit][1]:
                bitmap[-1].append(bit)
    yield total

    seen = set()
    n = 0
    def dfs(i, j):
        if ((i, j)) in seen:
            return
        if not bitmap[i][j]:
            return
        seen.add((i, j))
        if i > 0:
            dfs(i-1, j)
        if j > 0:
            dfs(i, j-1)
        if i < 127:
            dfs(i+1, j)
        if j < 127:
            dfs(i, j+1)

    for x in range(128):
        for y in range(128):
            if (x, y) in seen:
                continue
            if not bitmap[x][y]:
                continue
            n += 1
            dfs(x, y)

    yield n


@with_solutions(609, 253)
def solve15(data):
    # Dueling Generators

    def generator(factor, start, divisor = None):

        current = start
        while True:
            current *= factor
            current = current % 2147483647
            if divisor is None or current % divisor == 0:
                yield current & 0xffff

    A = generator(16807, 883)
    B = generator(48271, 879)

    yield sum(next(A) == next(B) for _ in range(40000000))

    A = generator(16807, 883, 4)
    B = generator(48271, 879, 8)

    yield sum(next(A) == next(B) for _ in range(5000000))


@with_solutions('gkmndaholjbfcepi', 'abihnfkojcmegldp')
def solve16(data):
    # Permutation Promenade

    history = {}
    c = 0

    def oneround(programs):
        for move in data.split(','):
            if move[0] == 's':
                n = int(move[1:])
                programs = programs[-n:] + programs[:-n]
            elif move[0] == 'x':
                a, b = [int(n) for n in move[1:].split('/')]
                programs[a], programs[b] = programs[b], programs[a]
            elif move[0] == 'p':
                a, b = move[1:].split('/')
                i, j = programs.index(a), programs.index(b)
                programs[i], programs[j] = programs[j], programs[i]
        return programs

    programs = oneround(list('abcdefghijklmnop'))
    yield ''.join(programs)

    programs = list('abcdefghijklmnop')
    history = []
    for x in range(1000000000):
        sp =  ''.join(programs)
        if sp in history:
            yield ''.join(history[1000000000 % len(history)])
            break
        history.append(sp)
        programs = oneround(programs)


@with_solutions(1971, 17202899)
def solve17(data):
    # Spinlock

    step = int(data)
    buffer = array.array('H', [0])
    position = 0
    for current in range(1, 2018):
        position = ((position + step) % len(buffer)) + 1
        buffer.insert(position, current)

    yield buffer[position + 1]

    position = val = 0
    bufferlen = 1
    for current in range(1, 50000001):
        position = ((position + step) % bufferlen) + 1
        if position == 1:
            val = current
        bufferlen += 1
    yield val


@with_solutions(3188, 7112)
def solve18(data):
    # Duet

    program = [line.split() for line in data.splitlines()]

    # single instruction execution
    def execute(registers, pid, ip, queues):
        try:
            instr, reg, val = program[ip]
        except ValueError:
            instr, reg = program[ip]
            val = 0

        try:
            val = int(val)
        except ValueError:
            val = registers[val]

        if instr == 'snd':
            if queues is None:
                registers['lastsnd'] = registers[reg]
            else:
                if pid == 1:
                    registers['count'] += 1
                queues[pid].put(registers[reg])
        elif instr == 'set':
            registers[reg] = val
        elif instr == 'add':
            registers[reg] += val
        elif instr == 'mul':
            registers[reg] *= val
        elif instr == 'mod':
            registers[reg] %= val
        elif instr == 'rcv':
            if queues is None:
                if registers[reg] != 0:
                    return (ip, 'done')
            else:
                try:
                    val = queues[1-pid].get(False)
                    registers[reg] = val
                except queue.Empty:
                    return (ip, 'recv')
        elif instr == 'jgz':
            if reg in string.ascii_lowercase:
                if registers[reg] > 0:
                    ip += val
                    return (ip, 'ok')
            else:
                if int(reg) > 0:
                    ip += val
                    return (ip, 'ok')

        ip += 1
        if 0 <= ip < len(program):
            return (ip, 'ok')
        else:
            return (ip, 'done')

    # Part 1
    registers = collections.defaultdict(int)
    ip = 0
    while True:
        ip, state = execute(registers, 0, ip, None)
        if state == 'done':
            break
    yield registers['lastsnd']

    # Part 2
    registers = [collections.defaultdict(int) for x in range(2)]
    registers[1]['p'] = 1
    ip = [0, 0]
    queues = [queue.Queue() for x in range(2)]
    states = ['ok', 'ok']
    pr = 0

    while True:
        _ip, _state = execute(registers[pr], pr, ip[pr], queues)

        # this program is waiting for a value, but the other program
        # has finished; this program will never receive a value so halt
        if _state == 'recv' and states[1 - pr] == 'done':
            break

        # we didn't write anything, but the other program is waiting
        # for a value; deadlock!
        if _state == 'recv' and queues[pr].empty() and states[1 - pr] == 'recv':
            break

        # both programs are done
        if _state == 'done' and states[1 - pr] == 'done':
            break

        # save state for the current program
        ip[pr] = _ip
        states[pr] = _state

        # if the current program is done, or waiting, switch to the other one
        if _state in ('done', 'recv'):
            pr = 1 - pr

    yield registers[1]['count']


@with_solutions('SXPZDFJNRL', 18126)
def solve19(data):
    # A Series of Tubes

    DIRECTIONS = { '↑' : [(-1,  0), '↓'],
                   '↓' : [( 1,  0), '↑'],
                   '←' : [( 0, -1), '→'],
                   '→' : [( 0,  1), '←']}

    def move(x, y, direction):
        dx, dy = DIRECTIONS[direction][0]
        return (x + dx, y + dy)

    def opposite(direction):
        return DIRECTIONS[direction][1]

    diagram = data.splitlines()

    # entry point is always at the top
    x, y = (0, diagram[0].find('|'))
    direction = '↓'

    path = []
    count = 0
    while True:
        x, y = move(x, y, direction)
        count += 1
        if diagram[x][y] == '+':
            for dir in ('←', '→', '↑', '↓'):
                if dir == opposite(direction):
                    continue
                nx, ny = move(x, y, dir)
                if diagram[nx][ny] != ' ':
                    direction = dir
                    break
        elif diagram[x][y] in string.ascii_uppercase:
            path.append(diagram[x][y])
        elif diagram[x][y] == ' ':
            break

    yield ''.join(path)
    yield count


@with_solutions('161', 438)
def solve20(data):
    # Particle Swarm

    def load(input_data):
        particles = []
        VREG = re.compile('^p=\<(?P<position>[^\>]+)\>, v=\<(?P<velocity>[^\>]+)\>, a=\<(?P<acceleration>[^\>]+)\>')
        for index, line in enumerate(input_data.splitlines()):
            match = VREG.match(line)
            position     = [int(p) for p in match.group('position').split(',')]
            velocity     = [int(v) for v in match.group('velocity').split(',')]
            acceleration = [int(a) for a in match.group('acceleration').split(',')]
            particles.append({'p': position,
                              'v':velocity,
                              'a':acceleration,
                              'n':'%03d' % index})

        return particles

    def add(veca, vecb):
        ax, ay, az = veca
        bx, by, bz = vecb
        return (ax + bx, ay + by, az + bz)

    def distance(particle):
        pos = particle['p']
        return abs(pos[0]) + abs(pos[1]) + abs(pos[2])

    # Part 1
    particles = load(data)
    for n in range(1000): # 1000 ticks should be enough
        for particle in particles:
            particle['v'] = add(particle['v'], particle['a'])
            particle['p'] = add(particle['p'], particle['v'])

    _particles = sorted(particles, key = distance)
    yield _particles[0]['n']

    # Part 2
    def collisions(_particles):
        positionmap = collections.defaultdict(list)
        for particle in _particles:
            positionmap[particle['p']].append(particle)

        _particles = []
        for key in positionmap.keys():
            if len(positionmap[key]) == 1:
                _particles.append(positionmap[key][0])
        return _particles

    particles = load(data)
    for n in range(1000):
        for particle in particles:
            particle['v'] = add(particle['v'], particle['a'])
            particle['p'] = add(particle['p'], particle['v'])
        particles = collisions(particles)

    yield len(particles)


@with_solutions(205, 3389823)
def solve21(data):
    # Fractal Art

    def matrix(line):
        return tuple([tuple(r) for r in line.split('/')])

    def to_str(m, prefix='', sep='/'):
        return '%s: [%s]' % (prefix, sep.join([''.join(r) for r in m]))

    def show(m, sep='/'):
        print(to_str(m, sep))
        if sep == '\n':
            print()

    def load(data):
        patterns = {}
        for line in data.splitlines():
            input, output = [matrix(p) for p in line.split(' => ')]
            patterns[input] = output
            patterns[flip(input)] = output
            for r in range(3):
                input = rotate(input)
                patterns[input] = output
                patterns[flip(input)] = output
        return patterns

    def flip(m):
        return tuple([tuple(reversed(r)) for r in m])

    def rotate(m):
        n = len(m)
        o = []
        for r in range(n):
            t = []
            for c in range(n-1, -1, -1):
                t.append(m[c][r])
            o.append(tuple(t))
        return tuple(o)

    def iteration(m):
        n = len(m)
        if n % 2 == 0:
            size = n // 2 * 3
            div, newdiv = 2, 3
        else:
            size = n//3 * 4
            div, newdiv = 3, 4

        out = [[None]*size for _ in range(size)]
        for i in range(0, n // div):
            for j in range(0, n // div):
                si, sj = i * div, j * div
                g = tuple([row[sj:sj+div] for row in m[si:si+div]])
                transf = patterns[g]

                ni, nj = i * newdiv, j * newdiv
                for a in range(newdiv):
                    for b in range(newdiv):
                        out[ni+a][nj+b] = transf[a][b]

        return tuple([tuple(r) for r in out])

    patterns = load(data)
    m = matrix('.#./..#/###')
    for n in range(18):
        m = iteration(m)
        if n == 4 or n == 17:
            yield to_str(m).count('#')


@with_solutions(5240, 2512144)
def solve22(data):
    # Sporifica Virus

    CLEAN, INFECTED, WEAKENED, FLAGGED = 0, 1, 2, 3

    def load(data):
        data = data.splitlines()
        grid = collections.defaultdict(int)
        for row, line in enumerate(data):
            for col, point in enumerate(line):
                if point == '#':
                    grid[(row, col)] = INFECTED
        startx, starty = len(data) // 2, len(line) // 2
        return grid, (startx, starty)

    CARDINALS = ['←', '↑', '→', '↓']
    DIRECTIONS = { '↑' : (-1,  0),
                   '↓' : ( 1,  0),
                   '←' : ( 0, -1),
                   '→' : ( 0,  1),}

    def move(position, direction):
        x, y = position
        direction = CARDINALS[direction]
        dx, dy = DIRECTIONS[direction]
        return ((x + dx, y + dy))

    def step(grid, position, direction):

        flag = False
        if grid[position] == INFECTED:
            direction = (direction + 1) % 4
            grid[position] = CLEAN
        else:
            flag = True
            direction = (direction - 1) % 4
            grid[position] = INFECTED

        position = move(position, direction)
        return position, direction, flag

    def evolved_step(grid, position, direction):

        flag = False
        if grid[position] == CLEAN:
            grid[position] = WEAKENED
            direction = (direction - 1) % 4
        elif grid[position] == WEAKENED:
            grid[position] = INFECTED
            flag = True
        elif grid[position] == INFECTED:
            grid[position] = FLAGGED
            direction = (direction + 1) % 4
        elif grid[position] == FLAGGED:
            grid[position] = CLEAN
            direction = (direction + 2) % 4

        position = move(position, direction)
        return position, direction, flag

    # Part 1
    grid, position = load(data)
    direction = 1

    count = 0
    for n in range(10000):
        position, direction, infected = step(grid, position, direction)
        if infected: count += 1
    yield count

    # Part 2
    grid, position = load(data)
    direction = 1

    count = 0
    for n in range(10000000):
        position, direction, infected = evolved_step(grid, position, direction)
        if infected: count += 1
    yield count


@with_solutions(6724, 903)
def solve23(data):
    # Coprocessor Conflagaration

    program = [line.split() for line in data.splitlines()]

    def execute(registers, ip):
        instr, reg, val = program[ip]

        try:
            val = int(val)
        except ValueError:
            val = registers[val]

        if instr == 'set':
            registers[reg] = val
        elif instr == 'sub':
            registers[reg] -= val
        elif instr == 'mul':
            registers[reg] *= val
            registers['count'] += 1
        elif instr == 'jnz':
            if reg in 'abcdefgh':
                if registers[reg] != 0:
                    ip += val
                    return ip
            else:
                if int(reg) != 0:
                    ip += val
                    return ip

        ip += 1
        return ip

    # Part 1
    registers = collections.defaultdict(int)
    ip = 0
    while True:
        ip = execute(registers, ip)
        if ip < 0 or ip >= len(program):
            break
    yield registers['count']

    # Part 2
    h = 0
    for b in range(108400, 125400 + 1, 17):
        for x in range(2, b):
            if b % x == 0:
                h += 1
                break
    yield h


@with_solutions(1906, 1824)
def solve24(data):
    # Electromagnetic Moat

    def build_bridges(components, bridge=None):
        if not bridge:
            bridge = [(0,0)]
        next = bridge[-1][1]
        for y in components[next]:
            if (next, y) not in bridge and (y, next) not in bridge:
                new = bridge + [(next, y)]
                yield new
                yield from build_bridges(components, new)

    components = collections.defaultdict(set)
    for line in data.splitlines():
        x, y = [int(p) for p in line.split('/')]
        components[x].add(y)
        components[y].add(x)

    # Part 1
    bridges = []
    maxstrength = maxlength = 0
    for bridge in build_bridges(components):
        bridges.append(bridge)
        maxstrength = max(maxstrength, sum(x+y for x,y in bridge))
        maxlength = max(maxlength, len(bridge))
    yield maxstrength

    # Part 2
    maxstrength = 0
    for bridge in filter(lambda b : len(b) == maxlength, bridges):
        maxstrength = max(maxstrength, sum(x+y for x,y in bridge))
    yield maxstrength


@with_solutions(3578, None)
def solve25(data):
    # The Halting Problem

    lines = data.splitlines()
    state = lines[0][-2]
    trigger = int(lines[1].split()[-2])

    states = {}
    for i in range(3, len(lines), 10):
        state_name = lines[i][-2]
        states[state_name] = [ (int(lines[i+2][-2]), lines[i+3][-6:-1].strip(), lines[i+4][-2]),
                               (int(lines[i+6][-2]), lines[i+7][-6:-1].strip(), lines[i+8][-2]) ]

    cursor = 100
    tape = collections.defaultdict(int)
    for n in range(trigger):
        current = tape[cursor]
        val, direction, nextstate = states[state][current]
        tape[cursor] = val
        if direction == 'left':
            cursor -= 1
        else:
            cursor += 1
        state = nextstate
    yield list(tape.values()).count(1)
    yield None

################################################################################

if __name__ == '__main__':

    if len(sys.argv) > 1:
        day = int(sys.argv[1])
    else:
        day = guess_day()

    get_session_id()
    if len(sys.argv) > 2:
        if sys.argv[2] == '-':
            data = sys.stdin.read()
        else:
            data = sys.argv[2]
    else:
        data = get_data(day)

    if day not in (19,):
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
