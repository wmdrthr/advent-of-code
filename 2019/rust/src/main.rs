#![feature(is_sorted)]

extern crate elapsed;
extern crate chrono;
extern crate itertools;
extern crate regex;

use std::process::exit;
use std::env;
use std::fs;
use std::io::{self, Read};

use chrono::prelude::*;


pub mod tsiolkovsky;
pub mod intcode;
pub mod programalarm;
pub mod crossingwires;
pub mod passwords;
pub mod airconditioning;
pub mod orbitalmap;
pub mod amplifiers;
pub mod spaceimageformat;
pub mod sensorboost;
pub mod asteroids;
pub mod hullpaintingrobot;
pub mod nbodyproblem;
pub mod carepackage;

fn usage() {
    println!("usage: adventofcode <day> [-|input file]");
}

const YEAR: i32 = 2019;

fn guess_day() -> Result<u8, i32> {
    let today: Date<Local> = Local::today();
    if today.year() != YEAR || today.month() != 12 || today.day() > 25 {
        Err(2)
    } else {
        Ok(today.day() as u8)
    }
}

fn get_data(day: u8) -> io::Result<String> {
    let mut args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        if args[2] == "-" {
            let mut buffer = String::new();
            let stdin = io::stdin();
            let mut handle = stdin.lock();

            handle.read_to_string(&mut buffer)
                .expect("error reading input data");
            Ok(buffer)
        } else if fs::metadata(&args[2]).is_ok() {
            fs::read_to_string(&args[2])
        } else {
            Ok(args.remove(2))
        }
    } else {
        let filename = format!("../inputs/input{:02}.txt", day);
        fs::read_to_string(filename)
    }
}

fn run(day: u8, data: String) {

    match day {
        1  => { tsiolkovsky::solve(data);       }
        2  => { programalarm::solve(data);      }
        3  => { crossingwires::solve(data);     }
        4  => { passwords::solve(data);         }
        5  => { airconditioning::solve(data);   }
        6  => { orbitalmap::solve(data);        }
        7  => { amplifiers::solve(data);        }
        8  => { spaceimageformat::solve(data);  }
        9  => { sensorboost::solve(data);       }
        10 => { asteroids::solve(data);         }
        11 => { hullpaintingrobot::solve(data); }
        12 => { nbodyproblem::solve(data);      }
        13 => { carepackage::solve(data);       }
        _ => { println!("no solver for day {} yet.", day); }
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

    let data = match get_data(day) {
        Ok(contents) => contents,
        Err(_) => {
            println!("input not found for day {}", day);
            usage(); exit(5);
        }
    };

    let (elapsed, _) = elapsed::measure_time(|| {
        run(day, data);
    });
    println!("elapsed = {}", elapsed);
}
