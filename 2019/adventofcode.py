#! /usr/bin/env python3
# encoding: utf-8

import os, sys, re
import time
from pprint import pprint
from datetime import datetime

import math

import pytz
import requests

YEAR  = 2019

SESSIONID_FILE = '~/.config/adventofcode/session'
USER_AGENT     = 'wmdrthr/advent-of-code'
UPDATE_INPUT   = False

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
        uri = 'http://adventofcode.com/{year}/day/{day}/input'.format(year=YEAR, day=day)
        response = requests.get(uri,
                                cookies={'session': get_session_id()},
                                headers={'User-Agent': USER_AGENT})
        if response.status_code != 200:
            raise Exception('Unexpected response: (status = %d)\n%s'.format(response.status_code,
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
            for index in range(2):
                actual = next(fgen)
                if expected[index] is not None and not kwargs['skip_verification']:
                    if actual != expected[index]:
                        print(error_msg.format(index + 1, expected[index], actual))
                        sys.exit(23)
                print(actual)
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
