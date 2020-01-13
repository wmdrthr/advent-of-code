use crate::intcode::*;

// Day 2: 1202 Program Alarm

pub fn solve(data: String) {
    let tape: Vec<i64> = data.split(",").map(|l| l.parse::<i64>().unwrap()).collect();

    // Part 1
    let mut tape1 = tape.clone();
    tape1[1] = 12; tape1[2] = 2;
    let mut vm = VM::new(tape1, None, None);
    vm.run();
    println!("Part 1: {}", vm.memory[0]);

    // Part 2
    for x in 0..100 {
        for y in 0..100 {
            let mut tape2 = tape.clone();
            tape2[1] = x; tape2[2] = y;
            let mut vm = VM::new(tape2, None, None);
            vm.run();
            if vm.memory[0] == 19690720 {
                println!("Part 2: {}", (100 * x + y));
                return;
            }
        }
    }
}
