#[macro_use] extern crate lazy_static;

extern crate elapsed;
extern crate chrono;
extern crate itertools;
extern crate regex;

use std::process::exit;
use std::env;
use std::fs;
use std::io::{self, Read};

use chrono::prelude::*;

pub mod reportrepair;
pub mod passwordphilosophy;
pub mod toboggantrajectory;
pub mod passportprocessing;
pub mod binaryboarding;
pub mod customcustoms;
pub mod handyhaversacks;
pub mod handheldhalting;
pub mod encodingerror;
pub mod adapterarray;
pub mod seatingsystem;
pub mod rainrisk;
pub mod shuttlesearch;
pub mod dockingdata;
pub mod rambunctiousrecitation;
pub mod tickettranslation;

fn usage() {
    println!("usage: adventofcode <day> [-|input file]");
}

const YEAR: i32 = 2020;

fn guess_day() -> Result<u8, i32> {
    let now: DateTime<Local> = Local::now();
    if now.year() != YEAR || now.month() != 12 || now.day() > 25 {
        Err(2)
    } else {
        let unlock = Local.ymd(now.year(), now.month(), now.day())
            .and_hms_milli(10, 30, 0, 0);

        if now > unlock {
            Ok(now.day() as u8)
        } else {
            Ok((now.day() - 1) as u8)
        }
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
        1  => { reportrepair::solve(data);           }
        2  => { passwordphilosophy::solve(data);     }
        3  => { toboggantrajectory::solve(data);     }
        4  => { passportprocessing::solve(data);     }
        5  => { binaryboarding::solve(data);         }
        6  => { customcustoms::solve(data);          }
        7  => { handyhaversacks::solve(data);        }
        8  => { handheldhalting::solve(data);        }
        9  => { encodingerror::solve(data);          }
        10 => { adapterarray::solve(data);           }
        11 => { seatingsystem::solve(data);          }
        12 => { rainrisk::solve(data);               }
        13 => { shuttlesearch::solve(data);          }
        14 => { dockingdata::solve(data);            }
        15 => { rambunctiousrecitation::solve(data); }
        16 => { tickettranslation::solve(data);      }
        _  => { println!("no solver for day {} yet.", day); }
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
        Ok(contents) => contents.trim().to_string(),
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
