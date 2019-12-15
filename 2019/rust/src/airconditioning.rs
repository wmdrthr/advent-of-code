use crate::intcode::*;
use std::time::Duration;

// Day 5: Sunny with a Chance of Asteroids

pub fn solve(data: String) {

    let tape: Vec<i64> = data.split(",") .map(|l| l.parse::<i64>().unwrap()).collect();

    // Part 1
    let (_, rx) = intcode_spawn(tape.clone(), vec![1]);
    loop {
        let v = rx.recv_timeout(Duration::from_millis(500)).unwrap();
        if v != 0 {
            println!("Part 1: {}", v);
            break;
        }
    }

    // Part 2
    let (_, rx) = intcode_spawn(tape.clone(), vec![5]);
    println!("Part 2: {}", rx.recv_timeout(Duration::from_millis(500)).unwrap());
}
