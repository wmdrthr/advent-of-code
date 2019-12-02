use std::env;
use std::fs;
use std::io::{self, Read};

use chrono::prelude::*;

const YEAR: i32 = 2019;

pub fn guess_day() -> Result<u8, i32> {
    let today: Date<Local> = Local::today();
    if today.year() != YEAR || today.month() != 12 || today.day() > 25 {
        Err(2)
    } else {
        Ok(today.day() as u8)
    }
}

pub fn get_data(day: u8) -> String {
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
