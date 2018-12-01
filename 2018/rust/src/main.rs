extern crate elapsed;

use std::fs;
use std::env;
use std::process::exit;

pub mod chronalcalibration;

fn get_data(day: u8) -> String {
    let filename = format!("../inputs/input{:02}.txt", day);
    let contents = match fs::read_to_string(filename) {
        Ok(str) => str,
        Err(_) => {
            println!("error reading input for day {}", day);
            String::from("")
        }
    };

    contents
}

fn usage() {
    println!("usage: adventofcode <day>\n\t1 <= day <= 31");
}

fn run(day: u8) -> bool {
    let data = get_data(day);

    match day {
        1 => { chronalcalibration::solve(data); true}
        _ => {
            println!("no solver for day {} yet.", day);
            false
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        usage();
        exit(1);
    } else {
        let day: u8 = match args[1].trim().parse() {
            Ok(num) => num,
            Err(_) => { usage(); exit(2); }
        };

        let (elapsed, _status) = elapsed::measure_time(|| {
            run(day);
        });
        println!("elapsed = {}", elapsed);
    }
}
