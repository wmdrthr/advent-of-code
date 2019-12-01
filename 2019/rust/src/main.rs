extern crate elapsed;
extern crate chrono;

use std::process::exit;
use std::env;

pub mod adventofcode;

fn usage() {
    println!("usage: adventofcode <day>\n\t1 <= day <= 31");
}

fn run(day: u8, data: String) -> bool {

    match day {
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
        match adventofcode::guess_day() {
            Ok(day) => day,
            Err(code) => { usage(); exit(code); }
        }
    };

    let data = adventofcode::get_data(day);

    let (elapsed, _status) = elapsed::measure_time(|| {
        run(day, data);
    });
    println!("elapsed = {}", elapsed);
}
