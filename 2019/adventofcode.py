#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re
import time
from pprint import pprint
from datetime import datetime

import math
from string import ascii_uppercase
import collections
from enum import Enum
import itertools
import functools
import queue
import threading
import unicodedata as U
import traceback as T
from dataclasses import dataclass
import copy

import pytz
import requests
import networkx as nx

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
        data = response.text
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
# Common Code

ORIGIN = (0, 0)
def manhattan(a, b = ORIGIN):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

DIRECTIONS = { '↑' : [( 0, -1), ['←', '→'], 1],
               '↓' : [( 0,  1), ['→', '←'], 2],
               '←' : [(-1,  0), ['↓', '↑'], 3],
               '→' : [( 1,  0), ['↑', '↓'], 4]}

def move(point, direction):
    (dx, dy) = DIRECTIONS[direction][0]
    return (point[0] + dx, point[1] + dy)

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


@dataclass
class V3:
    x: int = 0
    y: int = 0
    z: int = 0

    def __repr__(self):
        return '<{},{},{}>'.format(self.x, self.y, self.z)

    def __getitem__(self, dim):
        if not 0 <= dim <= 2:
            raise IndexError(f'invalid dimension: {dim}')
        return (self.x, self.y, self.z)[dim]

    def __setitem__(self, dim, value):
        if not 0 <= dim <= 2:
            raise IndexError(f'invalid dimension: {dim}')
        if dim == 0: self.x = value
        elif dim == 1: self.y = value
        elif dim == 2: self.z = value

    def __add__(self, other):
        return V3(self.x + other.x, self.y + other.y, self.z + other.z)

    def __iadd__(self, other):
        self.x += other.x
        self.y += other.y
        self.z += other.z
        return self

    def __int__(self):
        return abs(self.x) + abs(self.y) + abs(self.z)

################################################################################
# IntCode VM


class IQueue(queue.Queue):

    def __init__(self, iterable = None):
        queue.Queue.__init__(self)
        if iterable:
            for it in iterable:
                self.put(it)

    def put(self, val_or_iterable):
        try:
            for val in val_or_iterable:
                queue.Queue.put(self, val)
        except TypeError:
            queue.Queue.put(self, val_or_iterable)

class VMState(Enum):
    START = 0
    INIT = 1
    RUNNING = 2
    HALTED = 3
    CRASHED = 4
    INTERRUPTED = 5

class IntCodeVM():

    def __init__(self, program = None, inputQ = None, outputQ = None):
        self.state = VMState.START
        self.reset(program, inputQ, outputQ)

    def reset(self, program, inputQ = None, outputQ = None):
        self.ip = 0
        self.base = 0

        if inputQ:
            self.inputQ = inputQ
        if outputQ:
            self.outputQ = outputQ
        if program:
            self.memory = collections.defaultdict(int, dict(enumerate(program)))
            self.state = VMState.INIT
        return self

    def decode(self, opcode):
        opcode, modes = opcode % 100, opcode // 100
        input_a_mode  = modes % 10
        input_b_mode  = (modes // 10) % 10
        output_mode   = modes // 100

        return opcode, (input_a_mode, input_b_mode, output_mode)

    def fetch(self, loc, mode):
        if mode == 0:
            return self.memory[self.memory[loc]]
        elif mode == 1:
            return self.memory[loc]
        elif mode == 2:
            return self.memory[self.memory[loc] + self.base]

    def dest(self, loc, mode):
        if mode == 2:
            return self.memory[loc] + self.base
        else:
            return self.memory[loc]

    def add(self, modes):
        operand_a = self.fetch(self.ip + 1, modes[0])
        operand_b = self.fetch(self.ip + 2, modes[1])
        destination = self.dest(self.ip + 3, modes[2])

        self.memory[destination] = operand_a + operand_b
        self.ip += 4

    def mul(self, modes):
        operand_a = self.fetch(self.ip + 1, modes[0])
        operand_b = self.fetch(self.ip + 2, modes[1])
        destination = self.dest(self.ip + 3, modes[2])

        self.memory[destination] = operand_a * operand_b
        self.ip += 4

    def read(self, modes):
        destination = self.dest(self.ip + 1, modes[0])
        if self.inputQ is None:
            raise Exception('illegal instruction (no input)')
        try:
            self.memory[destination] = self.inputQ.get()
        except queue.Empty:
            if self.state is not VMState.INTERRUPTED:
                raise
        self.ip += 2

    def write(self, modes):
        value = self.fetch(self.ip + 1, modes[0])
        if self.outputQ is None:
            raise Exception('illegal instruction (no output)')
        self.outputQ.put(value)
        self.ip += 2

    def jump_if_true(self, modes):
        value = self.fetch(self.ip + 1, modes[0])
        if value != 0:
            self.ip = self.fetch(self.ip + 2, modes[1])
        else:
            self.ip += 3

    def jump_if_false(self, modes):
        value = self.fetch(self.ip + 1, modes[0])
        if value == 0:
            self.ip = self.fetch(self.ip + 2, modes[1])
        else:
            self.ip += 3

    def less_than(self, modes):
        value_a = self.fetch(self.ip + 1, modes[0])
        value_b = self.fetch(self.ip + 2, modes[1])
        destination = self.dest(self.ip + 3, modes[2])

        if value_a < value_b:
            self.memory[destination] = 1
        else:
            self.memory[destination] = 0
        self.ip += 4

    def equals(self, modes):
        value_a = self.fetch(self.ip + 1, modes[0])
        value_b = self.fetch(self.ip + 2, modes[1])
        destination = self.dest(self.ip + 3, modes[2])

        if value_a == value_b:
            self.memory[destination] = 1
        else:
            self.memory[destination] = 0
        self.ip += 4

    def adjust_base(self, modes):
        value = self.fetch(self.ip + 1, modes[0])
        self.base += value
        self.ip += 2

    def step(self):

        opcode, modes = self.decode(self.memory[self.ip])
        if opcode == 1:
            self.add(modes)
        elif opcode == 2:
            self.mul(modes)
        elif opcode == 3:
            self.read(modes)
        elif opcode == 4:
            self.write(modes)
        elif opcode == 5:
            self.jump_if_true(modes)
        elif opcode == 6:
            self.jump_if_false(modes)
        elif opcode == 7:
            self.less_than(modes)
        elif opcode == 8:
            self.equals(modes)
        elif opcode == 9:
            self.adjust_base(modes)
        else:
            raise Exception('illegal opcode (unknown opcode: {})'.format(opcode))

    def interrupt(self):

        self.state = VMState.INTERRUPTED
        if self.inputQ is not None:
            self.inputQ.put(0)

    def run(self):

        if self.state is not VMState.INIT:
            print('Invalid state: {}, should be INIT'.format(self.state))
            return

        self.state = VMState.RUNNING
        while self.state is VMState.RUNNING and self.memory[self.ip] != 99:
            try:
                self.step()
            except Exception as e:
                print('VM crash! {}'.format(e.args[0]))
                T.print_tb(sys.exc_info()[2])
                self.state = VMState.CRASHED
                return self

        if self.state == VMState.RUNNING:
            self.state = VMState.HALTED
        return self

    def start(self, name):
        self.thread = threading.Thread(target=IntCodeVM.run, name=name, args=(self,), daemon=True)
        self.thread.start()
        return self

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


@with_solutions(3562672, 8250)
def solve2(data):

    # 1202 Program Alarm

    tape = [int(x) for x in data.split(',')]

    # Part 1
    program = tape[:]
    program[1:3] = [12,2]
    vm = IntCodeVM(program).run()
    assert(vm.state is VMState.HALTED)
    yield vm.memory[0]

    # Part 2
    for x in range(100):
        for y in range(100):
             program = tape[:]
             program[1:3] = [x,y]
             vm.reset(program).run()
             assert(vm.state is VMState.HALTED)
             if vm.memory[0] == 19690720:
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

    outputQ = IQueue()

    vm = IntCodeVM(tape, IQueue([1]), outputQ).run()
    assert(vm.state is VMState.HALTED)
    while not outputQ.empty():
        output = outputQ.get_nowait()
    yield output

    vm.reset(tape, IQueue([5])).run()
    assert(vm.state is VMState.HALTED)
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

        return transfers

    stage1 = count_transfers('YOU', 'SAN')
    stage2 = count_transfers('SAN', 'YOU')
    print(stage1 + stage2)


@with_solutions(844468, 4215746)
def solve7(data):

    # Amplification Circuit

    tape = [int(x) for x in data.split(',')]

    # Part 1
    max_output = 0
    vm = IntCodeVM()
    for phase_setting in itertools.permutations([0, 1, 2, 3, 4]):
        current_input = 0
        for n in range(5):
            outputQ = IQueue()
            vm.reset(tape, IQueue([phase_setting[n], current_input]), outputQ).run()
            assert(vm.state is VMState.HALTED)
            current_input = outputQ.get_nowait()
        max_output = max(max_output, current_input)
    yield max_output

    # Part 2
    max_output = 0
    vms = [IntCodeVM() for _ in range(5)]
    for phase_setting in itertools.permutations([5, 6, 7, 8, 9]):
        queueAB = IQueue([phase_setting[1]])
        queueBC = IQueue([phase_setting[2]])
        queueCD = IQueue([phase_setting[3]])
        queueDE = IQueue([phase_setting[4]])
        queueEA = IQueue([phase_setting[0], 0])

        def runvm(vm, inputQ, outputQ):
            vm.reset(tape, inputQ, outputQ).run()

        amps = [threading.Thread(target=runvm, name='A', args=(vms[0], queueEA, queueAB)),
                threading.Thread(target=runvm, name='B', args=(vms[1], queueAB, queueBC)),
                threading.Thread(target=runvm, name='C', args=(vms[2], queueBC, queueCD)),
                threading.Thread(target=runvm, name='D', args=(vms[3], queueCD, queueDE)),
                threading.Thread(target=runvm, name='E', args=(vms[4], queueDE, queueEA))]

        for amp in amps:
            amp.start()
        amps[-1].join()

        while not queueEA.empty():
            output = queueEA.get_nowait()
        max_output = max(max_output, output)

    yield max_output


@with_solutions(2176, 'CYKBY')
def solve8(data):

    # Space Image Format

    WIDTH, HEIGHT = 25, 6
    AREA = WIDTH * HEIGHT
    layers = []
    for layer in range(len(data) // AREA):
        layers.append(data[layer * AREA : (layer + 1) * AREA])

    # Part 1
    minzeros = (65536, 0)
    for index, layer in enumerate(layers):
        zeros = layer.count('0')
        if zeros < minzeros[0]:
            minzeros = (zeros, index)

    layer = layers[minzeros[1]]
    yield (layer.count('1') * layer.count('2'))

    # Part 2
    chars = {'1':U.lookup('FULL BLOCK'), '0':U.lookup('LIGHT SHADE')}
    image = ['x' for _ in range(AREA)]
    for n in range(AREA):
        pixels = [l[n] for l in layers]
        for i in range(len(data) // AREA):
            if pixels[i] != '2':
                image[n] = chars[pixels[i]]
                break

    for row in range(HEIGHT):
        print(' '.join(image[row*WIDTH:(row+1)*WIDTH]))

    yield 'CYKBY'


@with_solutions(2465411646, 69781)
def solve9(data):

    # Sensor Boost

    tape = [int(x) for x in data.split(',')]
    outputQ = IQueue()

    # Part 1
    vm = IntCodeVM(tape, IQueue([1]), outputQ).run()
    yield outputQ.get_nowait()

    # Part 2
    vm.inputQ.put(2)
    vm.reset(tape).run()
    yield outputQ.get_nowait()


@with_solutions(269, 612)
def solve10(data):

    # Monitoring Station

    data = [l.strip() for l in data.split('\n') if len(l) > 1]
    asteroids = [(x, y) for y,l in enumerate(data) for x,c in enumerate(l) if c == '#']

    def angle(x, y):
        angle = math.atan2(x[0] - y[0], y[1] - x[1])
        if angle < 0:
            angle += 2 * math.pi
        return round(angle, 10)

    # Part 1
    max_visible = 0
    station = None
    for asteroid in asteroids:
        angles = set()
        for other in asteroids:
            if asteroid != other:
                angles.add(angle(other, asteroid))
        if len(angles) > max_visible:
            max_visible = len(angles)
            station = asteroid

    yield max_visible

    # Part 2
    def distance(x, y):
        return ((x[0] - y[0]) ** 2) + ((x[1] - y[1]) ** 2)

    targets = collections.defaultdict(list)
    for asteroid in asteroids:
        if asteroid != station:
            targets[angle(asteroid, station)].append((distance(station, asteroid), asteroid))

    for angle in targets.keys():
        targets[angle].sort(key = lambda t: t[0])

    counter = 0
    while True:
        for angle in sorted(targets.keys()):
            if len(targets[angle]) > 0:
                _, asteroid = targets[angle].pop(0)
                counter += 1
                if counter == 200:
                    yield asteroid[0] * 100 + asteroid[1]


@with_solutions(2211, 'EFCKUEGC')
def solve11(data):

    # Space Police

    tape = [int(x) for x in data.split(',')]

    panel = collections.defaultdict(int)
    robot = ('↑', ORIGIN)

    def move(turn):
        (direction, (x, y)) = robot
        new_direction = DIRECTIONS[direction][1][turn]
        dx, dy = DIRECTIONS[new_direction][0]
        return (new_direction, (x + dx, y + dy))

    inputQ, outputQ = IQueue(), IQueue()

    # Part 1
    vm = IntCodeVM(tape, outputQ, inputQ) # queues named from robot's perspective
    painted = set()
    vm.start('mondrian')
    while vm.state is not VMState.HALTED:
        outputQ.put(panel[robot[1]])
        color = inputQ.get(block = True, timeout = 1)
        turn  = inputQ.get(block = True, timeout = 1)
        panel[robot[1]] = color
        painted.add(robot[1])
        robot = move(turn)

    yield len(painted)

    # Part 2
    vm.reset(tape)
    robot = ('↑', ORIGIN)
    panel.clear(); panel[robot[1]] = 1
    vm.start('mondrian')
    while vm.state is not VMState.HALTED:
        outputQ.put(panel[robot[1]])
        color = inputQ.get(block = True, timeout = 1)
        turn  = inputQ.get(block = True, timeout = 1)
        panel[robot[1]] = color
        painted.add(robot[1])
        robot = move(turn)

    locations = sorted(panel.keys(), key=manhattan)
    chars = [U.lookup('LIGHT SHADE'), U.lookup('FULL BLOCK')]
    for y in range(locations[-1][1] + 1):
        for x in range(locations[-1][0] + 1):
            if (x, y) in panel:
                print(chars[panel[(x, y)]], end='')
            else:
                print(' ', end='')
        print()

    yield 'EFCKUEGC'


@with_solutions(7928, 518311327635164)
def solve12(data):

    # The N-Body Problem

    @dataclass
    class Moon:
        position: V3
        velocity: V3

        def __repr__(self):
            return '(P{}, V{})'.format(self.position, self.velocity)

        def __int__(self):
            return int(self.position) * int(self.velocity)

        def time_step(self):
            self.position += self.velocity

        def energy(self):
            return int(self.position) * int(self.velocity)

    moons = []
    pattern = re.compile('<x=([-\d]+), y=([-\d]+), z=([-\d]+)>')
    for line in data.split('\n'):
        match = pattern.match(line)
        if match:
            x, y, z = [int(v) for v in match.groups()]
            moons.append(Moon(V3(x, y, z), V3(0, 0, 0)))
    original_state = copy.deepcopy(moons)

    def gravity(ma, mb, dim):
        if ma.position[dim] < mb.position[dim]:
            ma.velocity[dim] = ma.velocity[dim] + 1
            mb.velocity[dim] = mb.velocity[dim] - 1
        elif ma.position[dim] > mb.position[dim]:
            ma.velocity[dim] = ma.velocity[dim] - 1
            mb.velocity[dim] = mb.velocity[dim] + 1

    # Part 1
    for _ in range(1000):
        for ma, mb in itertools.combinations(moons, 2):
            for dim in (0, 1, 2):
                gravity(ma, mb, dim)
        for moon in moons:
            moon.time_step()

    yield sum([moon.energy() for moon in moons])

    # Part 2
    def lcm(a, b):
        return a * b // math.gcd(a, b)

    class Sim:
        def __init__(self, moons, dim):
            self.moons = copy.deepcopy(moons)
            self.dim = dim
            self.steps = 0

        def run(self):
            self.thread = threading.current_thread()
            while True:
                for ma, mb in itertools.combinations(self.moons, 2):
                    gravity(ma, mb, self.dim)
                for moon in self.moons:
                    moon.time_step()
                self.steps += 1
                if sum([int(moon.velocity) for moon in self.moons]) == 0:
                    self.steps *= 2
                    break
                if self.moons == original_state:
                    break

    simulations = []
    for dim in (0, 1, 2):
        sim = Sim(original_state, dim)
        simulations.append(sim)
        threading.Thread(target=Sim.run, args=(sim,)).start()

    for sim in simulations:
        sim.thread.join()

    steps = [sim.steps for sim in simulations]
    yield lcm(lcm(steps[0], steps[1]), steps[2])


@with_solutions(205, 10292)
def solve13(data):

    # Care Package

    tape = [int(x) for x in data.split(',')]

    inputQ, outputQ = IQueue(), IQueue()

    # Part 1
    vm = IntCodeVM(tape, inputQ, outputQ)
    vm.start('breakout')
    counter = 0
    while vm.state is VMState.RUNNING:
        x = outputQ.get(block = True, timeout = 1)
        y = outputQ.get(block = True, timeout = 1)
        t = outputQ.get(block = True, timeout = 1)
        if t == 2:
            counter += 1

    yield counter

    # Part 2
    tape[0] = 2
    vm.reset(tape)
    screen = collections.defaultdict(int)
    vm.start('breakout')
    score, ball, paddle = 0, ORIGIN, ORIGIN
    while vm.state is VMState.RUNNING:
        x = outputQ.get()
        y = outputQ.get()
        t = outputQ.get()

        if (x,y) == (-1, 0):
            score = t
            continue

        screen[(x, y)] = t
        if t == 3:
            paddle = (x, y)
        if t == 4:
            ball = (x, y)

            if ball[0] < paddle[0]:
                inputQ.put(-1)
            elif ball[0] > paddle[0]:
                inputQ.put(1)
            else:
                inputQ.put(0)

    while not outputQ.empty():
        x = outputQ.get()
        y = outputQ.get()
        t = outputQ.get()

        if (x,y) == (-1, 0):
            score = t

    yield score


@with_solutions(1590844, 1184209)
def solve14(data):

    # Space Stoichiometry

    pattern = re.compile(r'^((?:\d+ [A-Z]+, )*\d+ [A-Z]+) => (\d+) ([A-Z]+)$')

    reactions = {}
    for line in data.split('\n'):
        match = pattern.search(line)
        if match:
            output = (int(match.group(2)), match.group(3))
            inputs = []
            for input in match.group(1).split(', '):
                amount, input = input.split(' ')
                inputs.append((int(amount), input))
            reactions[output[1]] = (output[0], inputs)

    def calculate(target, target_amount, surplus=None):
        if surplus is None:
            surplus = collections.defaultdict(int)
        if target == 'ORE':
            return target_amount
        elif target_amount <= surplus[target]:
            surplus[target] -= target_amount
            return 0

        target_amount -= surplus[target]
        surplus[target] = 0

        ore = 0
        output_amount, inputs = reactions[target]
        production_ratio = math.ceil(target_amount / output_amount)
        for input_amount, input in inputs:
            input_amount *= production_ratio
            ore += calculate(input, input_amount, surplus)
        surplus[target] += output_amount * production_ratio - target_amount
        return ore

    # Part 1
    fuel_cost = calculate('FUEL', 1)
    yield fuel_cost

    # Part 2
    ore = 1000000000000
    target_fuel = ore // fuel_cost
    fuel = 0
    surplus = collections.defaultdict(int)
    while ore and target_fuel:
        new_surplus = collections.defaultdict(int, surplus)
        ore_used = calculate('FUEL', target_fuel, new_surplus)
        if ore_used > ore:
            target_fuel //= 2
        else:
            fuel += target_fuel
            ore -= ore_used
            surplus = new_surplus

    yield fuel


@with_solutions(236, 368)
def solve15(data):

    # Oxygen System

    tape = [int(x) for x in data.split(',')]

    UNKNOWN = 0
    EMPTY = 1
    WALL = 2
    OXYGEN = 3

    maze = collections.defaultdict(int, {(0,0): EMPTY})
    oxygen_system = None

    def navigate(current, dest, prev = None):
        path = None
        for dir in DIRECTIONS.keys():
            next = move(current, dir)
            if next == prev:
                continue
            if next == dest:
                return [dir]
            if maze[next] == EMPTY or maze[next] == OXYGEN:
                new_path = navigate(next, dest, current)
                if new_path is not None and (path is None or len(new_path) < len(path)):
                    path = [dir] + new_path
        return path

    inputQ, outputQ = IQueue(), IQueue()
    vm = IntCodeVM(tape, inputQ, outputQ)
    vm.start('theseus')

    droid = ORIGIN
    explore = [(0, 1), (1, 0), (-1, 0), (0, -1)]

    while len(explore) > 0:
        next_dest = explore.pop()
        path = navigate(droid, next_dest)
        for current_dir in path:
            inputQ.put(DIRECTIONS[current_dir][-1])
            status = outputQ.get()
            next = move(droid, current_dir)

            explore = [p for p in explore if p != next]

            if status == 0:
                maze[next] = WALL
            else:
                droid = next

                if maze[droid] == UNKNOWN:
                    explore += [p for p in [move(droid, dir) for dir in DIRECTIONS.keys()]
                                if maze[p] == UNKNOWN]
                if status == 2:
                    maze[droid] = OXYGEN
                    oxygen_system = droid
                    oxy_path = navigate(ORIGIN, droid)
                    yield len(oxy_path)
                else:
                    maze[droid] = EMPTY

    vm.interrupt()

    display(maze, {UNKNOWN: U.lookup('LIGHT SHADE'),
                   EMPTY: '.',
                   WALL: U.lookup('FULL BLOCK'),
                   OXYGEN: '*'})

    # Part 2
    time = 0
    while True:
        oxygenated = [p for p,v in maze.items() if v == OXYGEN]
        empty_neighbors = [move(p, dir) for p in oxygenated for dir in DIRECTIONS.keys()
                           if maze[move(p, dir)] == EMPTY]
        if len(empty_neighbors) == 0:
            break
        for p in empty_neighbors:
            maze[p] = OXYGEN
        time += 1

    yield time


@with_solutions('11833188', '55005000')
def solve16(data):

    # Flawed Frequency Transmission

    base_pattern = [0, 1, 0, -1]

    def nth_pattern(n):
        if n == 0:
            return itertools.islice(itertools.cycle(base_pattern), 1, None)
        pattern = []
        for val in base_pattern:
            pattern += [val] * (n + 1)
        return itertools.islice(itertools.cycle(pattern), 1, None)

    # Part 1
    signal = [int(c) for c in data]
    for phase in range(100):
        phase_output = []
        for n in range(len(signal)):
            pattern = nth_pattern(n)
            output_digit = sum([a * b for (a,b) in zip(signal, pattern)])
            phase_output.append(abs(output_digit) % 10)
        signal = phase_output
    yield '{}'.format(''.join([chr(48 + d) for d in phase_output[:8]]))

    # Part 2
    skip = int(data[:7])
    signal = [int(c) for c in data] * 10000

    for phase in range(100):
        checksum = 0
        phase_output = []
        for n in range(1, len(signal) - skip + 1):
            checksum += signal[-n]
            phase_output.append(abs(checksum) % 10)
        signal = [0] * skip + list(reversed(phase_output))

    print(''.join([chr(48 + d) for d in signal[skip:skip+8]]))


@with_solutions(6000, 807320)
def solve17(data):

    # Set and Forget

    tape = [int(x) for x in data.split(',')]

    SCAFFOLD = 35
    MAPPING = {'^':'↑', 'v':'↓', '<':'←', '>':'→', '→':'R','←':'L'}

    inputQ, outputQ = IQueue(), IQueue()
    vm = IntCodeVM(tape, inputQ, outputQ)
    vm.start('camera')

    def move(point, dir):
        dx, dy = DIRECTIONS[dir][0]
        return (point[0] + dx, point[1] + dy)

    grid = collections.defaultdict(int)
    row = []
    x = y = maxx = 0
    robot = None
    robot_dir = None
    while vm.state is VMState.RUNNING or not outputQ.empty():
        output = outputQ.get()
        grid[(x, y)]  = output
        if output not in (35, 46, 10):
            robot = (x, y)
            grid[(x,y)] = SCAFFOLD
            robot_dir = MAPPING[chr(output)]
        x += 1
        if output == 10:
            if len(row) > 0:
                print(f'{y}\t{"".join(row)}')
            row = []
            y += 1; x = 0; maxx = x
        else:
            row.append(chr(output))
    maxy = y

    # Part 1
    intersections = []
    scaffolding = {k:v for (k,v) in grid.items() if v == SCAFFOLD}
    for point in scaffolding.keys():
        intersection = True
        for _,neighbor in neighbors(point):
            if neighbor not in scaffolding or scaffolding[neighbor] != SCAFFOLD:
                intersection = False
                break
        if intersection:
            intersections.append(point)

    yield sum([x * y for (x, y) in intersections])

    # Part 2
    def navigate():
        current = robot
        current_dir = robot_dir
        path = []
        while True:
            next = move(current, current_dir)
            if grid[next] == SCAFFOLD:
                if path[-1] in ('L', 'R'):
                    path.append(1)
                else:
                    path[-1] += 1
                current = next
            else:
                # first, try turning right
                dir = DIRECTIONS[current_dir][1][1]
                next = move(current, dir)
                if grid[next] == SCAFFOLD:
                    path.append('R')
                    current_dir = dir
                    continue

                # try turning left instead
                dir = DIRECTIONS[current_dir][1][0]
                next = move(current, dir)
                if grid[next] == SCAFFOLD:
                    path.append('L')
                    current_dir = dir
                    continue

                # neither? maybe we have reached the end
                break
        return path

    path = ','.join([str(p) for p in navigate()])
    A = 'R,4,L,10,L,10'
    B = 'L,8,R,12,R,10,R,4'
    C = 'L,8,L,8,R,10,R,4'
    main_routine = path.replace(A, 'A').replace(B, 'B').replace(C, 'C')

    for ch in main_routine:
        inputQ.put(ord(ch))
    inputQ.put(10)

    for func in (A, B, C):
        for ch in func:
            inputQ.put(ord(ch))
        inputQ.put(10)

    # continuous video feed?
    inputQ.put(ord('n'))
    inputQ.put(10)

    tape = [int(x) for x in data.split(',')]
    tape[0] = 2
    vm.reset(tape)
    vm.start('robot')
    vm.thread.join()
    while not outputQ.empty():
        output = outputQ.get()

    yield output


@with_solutions(192, 8381082)
def solve19(data):

    # Tractor Beam

    tape = [int(x) for x in data.split(',')]

    grid = collections.defaultdict(int)

    inputQ, outputQ = IQueue(), IQueue()
    vm = IntCodeVM(tape, inputQ, outputQ)

    def check(x, y):
        vm.reset(tape)
        inputQ.put([x, y])
        vm.run()
        return outputQ.get()

    # Part 1
    count = 1
    for x in range(50):
        for y in range(50):
            if (x, y) == (0,0):
                continue
            if check(x, y):
                count += 1

    yield count

    # Part 2
    x, y = 0, 100
    while True:
        if check(x, y):
            if check(x + 99, y - 99):
                yield (10_000 * x) + (y - 99)
            else:
                y += 1
        else:
            x += 1


@with_solutions(686, 8384)
def solve20(data):

    # Donut Maze

    data = [l for l in data.split('\n') if len(l) > 1]
    grid = {(x,y):c for y,l in enumerate(data) for x,c in enumerate(l)}

    maze = nx.Graph()
    start, end = None, None
    portals = {}
    for y,row in enumerate(data):
        for x,v in enumerate(row):
            if v != '.':
                continue
            node = (x, y)
            for dir, point in neighbors(node):
                if grid[point] == '.':
                    maze.add_edge(node, point, level=0)
                elif grid[point] in ascii_uppercase:
                    portal = ''.join(sorted([grid[point], grid[move(point, dir)]]))
                    if portal == 'AA':
                        start = node
                        continue
                    elif portal == 'ZZ':
                        end = node
                        continue

                    if portal in portals:
                        maze.add_edge(node, portals[portal], level=0)
                        portals[portal] = (node, portals[portal])
                    else:
                        portals[portal] = node

    # Part 1
    path = nx.shortest_path(maze, source=start, target=end)
    yield len(path) - 1

    # Part 2
    maze = maze.to_directed()

    for portal, (a, b) in portals.items():
        ax, ay = a
        bx, by = b

        if ax in (2, len(data[0]) - 3) or ay in (2, len(data) - 3):
            maze.edges[a, b]['level'] = -1
            maze.edges[b, a]['level'] =  1
        else:
            maze.edges[a, b]['level'] =  1
            maze.edges[b, a]['level'] = -1

    start = (start, 0)
    target = (end, 0)
    visited = set()
    queue = {start}
    steps = 0
    while len(queue) > 0:
        visited.update(queue)
        _queue = set()
        for point, level in queue:
            for neighbor in maze.successors(point):
                level_change = maze.edges[point, neighbor]['level']
                if level + level_change >= 0:
                    _queue.add((neighbor, level+level_change))
        queue = _queue - visited
        steps += 1
        if target in queue:
            break

    yield steps


@with_solutions(19360288, 1143814750)
def solve21(data):

    # Springdroid Adventure

    tape = [int(x) for x in data.split(',')]

    inputQ, outputQ = IQueue(), IQueue()
    vm = IntCodeVM(tape, inputQ, outputQ)

    def springscript(program):
        inputQ.put([ord(c) for c in program])
        inputQ.put(10)

    # Part 1
    springscript('OR A J')
    springscript('AND B J')
    springscript('AND C J')
    springscript('NOT J J')
    springscript('AND D J')
    springscript('WALK')

    vm.run()
    while True:
        output = outputQ.get_nowait()
        if output > 256:
            yield output
            break

    # Part 2
    springscript('OR A J')
    springscript('AND B J')
    springscript('AND C J')
    springscript('NOT J J')
    springscript('AND D J')
    springscript('OR E T')
    springscript('OR H T')
    springscript('AND T J')
    springscript('RUN')

    vm.reset(tape).run()
    while True:
        output = outputQ.get_nowait()
        if output > 256:
            yield output
            break


@with_solutions(4775, 37889219674304)
def solve22(data):

    # Slam Shuffle

    def shuffle(number_of_cards, p):
        for line in data.split('\n'):
            if line == 'deal into new stack':
                p = (number_of_cards - 1 - p) % number_of_cards
            else:
                n = int(line.split(' ')[-1])
                if line[:3] == 'cut':
                    p = (p - n) % number_of_cards
                elif line[:4] == 'deal':
                    p = (n * p) % number_of_cards
        return p

    def shuffle2(number_of_cards, p, reps):
        increment = 1
        offset = 0

        def inv(n):
            return pow(n, number_of_cards - 2, number_of_cards)

        for line in data.split('\n'):
            if line == 'deal into new stack':
                increment = (increment * -1) % number_of_cards
                offset = (offset + increment) % number_of_cards
            else:
                n = int(line.split(' ')[-1])
                if line[:3] == 'cut':
                    offset = (offset + n * increment) % number_of_cards
                elif line[:4] == 'deal':
                    increment = (increment * inv(n)) % number_of_cards

        final_increment = pow(increment, reps, number_of_cards)
        final_offset = offset * (1 - final_increment) * inv((1 - increment) % number_of_cards)
        final_offset %= number_of_cards

        return (final_offset + p * final_increment) % number_of_cards

    # Part 1
    yield shuffle(10007, 2019)

    # Part 2
    yield shuffle2(119315717514047, 2020, 101741582076661)

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

    if day != 20:
        data = data.strip()

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
