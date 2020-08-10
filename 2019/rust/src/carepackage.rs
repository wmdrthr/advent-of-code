use crate::intcode::*;
use std::sync::mpsc::Receiver;

fn read_three_values(rx: &Receiver<i64>) -> Option<(i64, i64, i64)> {

    let x = match rx.recv() {
        Err(_)  => { return None },
        Ok(val) => val
    };

    // the program always outputs three values together, so if we
    // read one value, we can confidently read two more
    let y = rx.recv().unwrap();
    let v = rx.recv().unwrap();

    Some((x, y, v))
}

// Day 13: Care Package

pub fn solve(data: String) {

    let tape: Vec<i64> = data.split(",") .map(|l| l.parse::<i64>().unwrap()).collect();

    let mut counter: u32 = 0;
    let (_, rx) = intcode_spawn(tape.clone(), vec![]);

    loop {

        if let Some((_, _, v)) = read_three_values(&rx) {
            if v == 2 {
                counter += 1;
            }
        } else {
            break;
        }
    }

    println!("Part 1: {}", counter);

    let mut program = tape.clone();
    program[0] = 2; // play for free

    let (tx, rx) = intcode_spawn(program, vec![]);
    let mut score: u32 = 0;
    let mut ball: (u32, u32);
    let mut paddle: (u32, u32) = (0, 0);

    loop {

        if let Some((x, y, v)) = read_three_values(&rx) {

            if x == -1 && y == 0 {
                score = v as u32;
                continue;
            } else if v == 3 {
                paddle = (x as u32, y as u32);
            } else if v == 4 {
                ball = (x as u32, y as u32);

                if ball.0 < paddle.0 {
                    tx.send(-1).unwrap();
                } else if ball.0 > paddle.0 {
                    tx.send(1).unwrap();
                } else {
                    tx.send(0).unwrap();
                }
            }
        } else {
            break;
        }
    }

    println!("Part 2: {}", score);
}
