use crate::intcode::*;

// Day 9: Sensor Boost

pub fn solve(data: String) {

    let tape: Vec<i64> = data.split(",").map(|l| l.parse::<i64>().unwrap()).collect();

    let (_, rx) = intcode_spawn(tape.clone(), vec![1]);
    println!("Part 1: {}", rx.recv().unwrap());

    let (_, rx) = intcode_spawn(tape.clone(), vec![2]);
    println!("Part 2: {}", rx.recv().unwrap());
}
