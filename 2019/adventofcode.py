#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re
import time
from pprint import pprint
from datetime import datetime

import math
import collections
import itertools
import queue
import threading

import pytz
import requests

YEAR  = 2019

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
    today = datetime.now(tz=pytz.timezone('Asia/Kolkata'))
    if today.year != YEAR or today.month != 12 or today.day > 25:
        raise Exception('AoC {%d} not currently running, day must be provided.'.format(YEAR))
    return today.day

def get_data(day):
    "Get input data for day (1-25) and year (> 2015)"

    inputfile = 'inputs/input{:02}.txt'.format(day)

    if os.path.exists(inputfile):
        data = open(inputfile).read()
        data = data.strip()
    else:
        # if trying to fetch the data for the current AoC, check if the
        # day's puzzle has unlocked yet
        now = datetime.now(tz=pytz.timezone('Asia/Kolkata'))
        if now.year == YEAR and now.month == 12 and day < 25:
            unlock = now.replace(hour = 10, minute = 30) # Midnight EST -> 10:30 AM IST
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
        data = response.text.strip()
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
# Solvers

@with_solutions(3266288, 4896582)
def solve1(data):

    # The Tyranny of the Rocket Equation

    total_basic_fuel = 0
    total_fuel = 0
    for module in data.split():
        mass = int(module)
        fuel = math.floor(mass / 3) - 2
        total_basic_fuel += fuel
        total_fuel += fuel
        while True:
            extra_fuel = math.floor(fuel / 3) - 2
            if extra_fuel <= 0:
                break
            total_fuel += extra_fuel
            fuel = extra_fuel

    yield total_basic_fuel
    yield total_fuel


class IQueue(queue.Queue):

    def __init__(self, iterable = None):
        queue.Queue.__init__(self)
        if iterable:
            for it in iterable:
                self.put(it)

def intcode_run(memory, inputQ = None, outputQ = None):

    tname = threading.current_thread().name

    def decode(opcode):

        opcode, modes = opcode % 100, opcode // 100
        input_a_mode  = modes % 10
        input_b_mode  = (modes // 10) % 10
        output_mode   = modes // 100

        return opcode, input_a_mode, input_b_mode, output_mode

    iptr = 0
    while memory[iptr] != 99:

        # fetch and decode
        opcode, input_a_mode, input_b_mode, output_mode = decode(memory[iptr])
        operand_a = memory[iptr+1]
        if not input_a_mode:
            operand_a = memory[operand_a]
        if opcode in (1, 2, 5, 6, 7, 8):
            operand_b = memory[iptr+2]
            if not input_b_mode:
                operand_b = memory[operand_b]
        if opcode in (1, 2, 7, 8):
            output = memory[iptr+3]

        # execute
        if opcode == 1: # add
            memory[output] = operand_a + operand_b
            iptr += 4
        elif opcode == 2: # multiply
            memory[output] = operand_a * operand_b
            iptr += 4
        elif opcode == 3: # input
            if inputQ is None:
                raise Exception('illegal instruction')
            memory[memory[iptr+1]] = inputQ.get(block = True, timeout=5)
            iptr += 2
        elif opcode == 4: # output
            if outputQ is None:
                raise Exception('illegal instruction')
            if output_mode:
                outputQ.put(memory[iptr+1])
            else:
                outputQ.put(operand_a)
            iptr += 2
        elif opcode == 5: # jump-if-true
            if operand_a != 0:
                iptr = operand_b
            else:
                iptr += 3
        elif opcode == 6: # jump-if-false
            if operand_a == 0:
                iptr = operand_b
            else:
                iptr += 3
        elif opcode == 7: # less than
            if operand_a < operand_b:
                memory[output] = 1
            else:
                memory[output] = 0
            iptr += 4
        elif opcode == 8: # equals
            if operand_a == operand_b:
                memory[output] = 1
            else:
                memory[output] = 0
            iptr += 4
        else:
            raise Exception('illegal instruction')

    return memory

@with_solutions(3562672, 8250)
def solve2(data):

    # 1202 Program Alarm

    tape = [int(x) for x in data.split(',')]

    # Part 1
    program = tape[:]
    program[1:3] = [12,2]
    result = intcode_run(program)
    yield result[0]

    # Part 2
    for x in range(100):
        for y in range(100):
            program = tape[:]
            program[1:3] = [x,y]
            result = intcode_run(program)
            if result[0] == 19690720:
                n = 100 * x + y
                yield n

@with_solutions(2129, 134662)
def solve3(data):

    # Crossed Wires

    grid = collections.defaultdict(lambda: (0, [0,0]))

    def points(start, direction, distance):
        x, y = start
        for step in range(distance):
            if direction == 'R':
                point = (x + 1, y)
            elif direction == 'U':
                point = (x, y - 1)
            elif direction == 'L':
                point = (x - 1, y)
            elif direction == 'D':
                point = (x, y + 1)
            else:
                raise Exception('Invalid direction: {}'.format(direction))
            yield point
            x, y = point

    origin = (0, 0)
    def manhattan(a, b):
        return abs(a[0] - b[0]) + abs(a[1] - b[1])

    for wire, route in enumerate([l for l in data.split('\n') if len(l) > 0]):
        current = origin
        steps = 0
        for path in route.split(','):
            direction = path[0]
            distance = int(path[1:])
            for point in points(current, direction, distance):
                (w,s) = grid[point]
                steps += 1
                s[wire] = steps
                grid[point] = (w + wire + 1, s)
            current = point

    # Part 1
    intersections = [manhattan(origin, k) for (k,v) in grid.items() if v[0] == 3]
    intersections.sort()
    yield intersections[0]

    # Part 2
    signal_delay = [sum(v[1]) for (k,v) in grid.items() if v[0] == 3]
    signal_delay.sort()
    yield signal_delay[0]

@with_solutions(1150, 748)
def solve4(data):

    # Secure Container

    def increasing(n):
        ns = list(n)
        if ns == sorted(ns):
            return True
        return False

    def duplicates(n, check):
        counter = collections.defaultdict(int)
        for d in list(n):
            counter[d] += 1
        return len([v for v in counter.values() if check(v)]) > 0

    start, end = data.split('-', 2)

    # Part 1
    valid = lambda p: increasing(p) and duplicates(p, lambda v: v > 1)
    passwords = [p for p in range(int(start), int(end)) if valid(str(p))]
    yield len(passwords)

    # Part 2
    valid = lambda p: duplicates(p, lambda v: v == 2)
    passwords = [p for p in passwords if valid(str(p))]
    yield len(passwords)

@with_solutions(13547311, 236453)
def solve5(data):

    # Sunny with a Chance of Asteroids

    tape = [int(x) for x in data.split(',')]

    program = tape[:]
    outputQ = IQueue()
    intcode_run(program, IQueue([1]), outputQ)
    while not outputQ.empty():
        output = outputQ.get_nowait()
    yield output

    program = tape[:]
    intcode_run(program, IQueue([5]), outputQ)
    yield outputQ.get_nowait()

@with_solutions(162439, 367)
def solve6(data):

    # Universal Orbit Map

    orbital_map = collections.defaultdict(list)
    primaries = {}
    satellites = collections.defaultdict(set)

    entries = [l.strip().split(')', 2) for l in data.split('\n') if len(l) > 1]
    for a, b in entries:
        primaries[b] = a
        orbital_map[a].append(b)

    # Part 1
    orbital_counter = collections.defaultdict(int)
    def update_orbit_counts(primary):
        for satellite in orbital_map[primary]:
            orbital_counter[satellite] = 1 + orbital_counter[primary]
            update_orbit_counts(satellite)

    update_orbit_counts('COM')
    print(sum(orbital_counter.values()))

    # Part 2
    for object in orbital_map.keys():
        primary = primaries.get(object, None)
        while primary is not None:
            satellites[primary].add(object)
            primary = primaries.get(primary, None)

    def count_transfers(current, target):
        transfers = 0
        current = primaries[current]
        while True:
            current = primaries[current]
            transfers += 1
            if target in satellites[current]:
                break

        return transfers, current

    stage1, current = count_transfers('YOU', 'SAN')
    stage2, _ = count_transfers('SAN', 'YOU')
    print(stage1 + stage2)


@with_solutions(844468, 4215746)
def solve7(data):

    tape = [int(x) for x in data.split(',')]

    # Part 1
    max_output = 0
    for phase_setting in itertools.permutations([0, 1, 2, 3, 4]):
        current_input = 0
        for n in range(5):
            outputQ = IQueue()
            intcode_run(tape[:], IQueue([phase_setting[n], current_input]), outputQ)
            #print('Amp 1 Output: ', outputs)
            current_input = outputQ.get_nowait()
        max_output = max(max_output, current_input)
    yield max_output

    # Part 2
    max_output = 0
    for phase_setting in itertools.permutations([5, 6, 7, 8, 9]):
        queueAB = IQueue([phase_setting[1]])
        queueBC = IQueue([phase_setting[2]])
        queueCD = IQueue([phase_setting[3]])
        queueDE = IQueue([phase_setting[4]])
        queueEA = IQueue([phase_setting[0], 0])

        amps = [threading.Thread(target=intcode_run, name='A', args=(tape[:], queueEA, queueAB)),
                threading.Thread(target=intcode_run, name='B', args=(tape[:], queueAB, queueBC)),
                threading.Thread(target=intcode_run, name='C', args=(tape[:], queueBC, queueCD)),
                threading.Thread(target=intcode_run, name='D', args=(tape[:], queueCD, queueDE)),
                threading.Thread(target=intcode_run, name='E', args=(tape[:], queueDE, queueEA))]

        for amp in amps:
            amp.start()
        for amp in amps:
            amp.join()

        while not queueEA.empty():
            output = queueEA.get_nowait()
        max_output = max(max_output, output)

    yield max_output

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
            data = sys.argv[2]
        custom_data = True
    else:
        data = get_data(day)

    if not data:
        print('Cannot run solver without data. Bailing')
        sys.exit(0)

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
