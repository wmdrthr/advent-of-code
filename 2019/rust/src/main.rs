#![feature(is_sorted)]

extern crate elapsed;
extern crate chrono;

use std::process::exit;
use std::env;
use std::fs;
use std::io::{self, Read};

use chrono::prelude::*;

const YEAR: i32 = 2019;

pub mod adventofcode;
pub mod tsiolkovsky;
pub mod intcode;
pub mod crossingwires;
pub mod passwords;
pub mod orbitalmap;

fn usage() {
    println!("usage: adventofcode <day>\n\t1 <= day <= 31");
}

fn guess_day() -> Result<u8, i32> {
    let today: Date<Local> = Local::today();
    if today.year() != YEAR || today.month() != 12 || today.day() > 25 {
        Err(2)
    } else {
        Ok(today.day() as u8)
    }
}

fn get_data(day: u8) -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        if args[2] == "-" {
            let mut buffer = String::new();
            let stdin = io::stdin();
            let mut handle = stdin.lock();

            handle.read_to_string(&mut buffer)
                .expect("error reading input data");
            buffer
        } else {
            args.remove(2)
        }
    } else {
        let filename = format!("../inputs/input{:02}.txt", day);
        let contents = match fs::read_to_string(filename) {
            Ok(str) => str,
            Err(_) => {
                println!("input not found for day {}", day);
                String::from("")
            }
        };
        contents
    }
}

fn run(day: u8, data: String) -> bool {

    match day {
        1 => { tsiolkovsky::solve(data);   true }
        2 => { intcode::solve2(data);      true }
        3 => { crossingwires::solve(data); true }
        4 => { passwords::solve(data);     true }
        5 => { intcode::solve5(data);      true }
        6 => { orbitalmap::solve(data);    true }
        _ => {
            println!("no solver for day {} yet.", day);
            false
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let day: u8 = if args.len() > 1 {
        match args[1].trim().parse() {
            Ok(num) => num,
            Err(_) => { usage(); exit(2); }
        }
    } else {
        match guess_day() {
            Ok(day) => day,
            Err(code) => { usage(); exit(code); }
        }
    };

    let data = get_data(day);

    let (elapsed, _status) = elapsed::measure_time(|| {
        run(day, data);
    });
    println!("elapsed = {}", elapsed);
}
