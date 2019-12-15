use crate::intcode::*;
use std::sync::mpsc;
use std::thread;
use itertools::Itertools;


fn solve7b(tape: &Vec<i64>, settings: Vec<i64>) -> i64 {

        let (tx_ab, rx_ab) = mpsc::channel(); tx_ab.send(settings[1]).unwrap();
        let (tx_bc, rx_bc) = mpsc::channel(); tx_bc.send(settings[2]).unwrap();
        let (tx_cd, rx_cd) = mpsc::channel(); tx_cd.send(settings[3]).unwrap();
        let (tx_de, rx_de) = mpsc::channel(); tx_de.send(settings[4]).unwrap();
        let (tx_ea, rx_ea) = mpsc::channel();
        let (tx, rx)       = mpsc::channel(); tx.send(settings[0]).unwrap(); tx.send(0).unwrap();

        let mut vms: Vec<VM> = Vec::new();
        vms.push(VM::new(tape.clone(), Some(rx),    Some(tx_ab)));
        vms.push(VM::new(tape.clone(), Some(rx_ab), Some(tx_bc)));
        vms.push(VM::new(tape.clone(), Some(rx_bc), Some(tx_cd)));
        vms.push(VM::new(tape.clone(), Some(rx_cd), Some(tx_de)));
        vms.push(VM::new(tape.clone(), Some(rx_de), Some(tx_ea)));

        for mut vm in vms {
            thread::spawn(move || {
                vm.run();
            });
        }

        let (tx_out, rx_out) = mpsc::channel();

        let handler = thread::spawn(move || {
            loop {
                let v: i64 = rx_ea.recv().unwrap();
                match tx.send(v) {
                    Ok(_)  => {},
                    Err(_) => { tx_out.send(v).unwrap(); break; }
                }
            }
        });
    handler.join().unwrap();

    rx_out.recv().unwrap()
}

// Day 7: Amplification Circuit

pub fn solve(data: String) {

    let tape: Vec<i64> = data.split(",") .map(|l| l.parse::<i64>().unwrap()).collect();

    // Part 1
    let mut max_output: i64 = 0;
    for setting in (0..5).permutations(5) {
        let mut current: i64 = 0;
        for i in 0..5 {
            let (_, rx) = intcode_spawn(tape.clone(), vec![setting[i].clone(), current]);
            current = rx.recv().unwrap();
        }
        max_output = max_output.max(current);
    }
    println!("Part 1: {}", max_output);

    // Part 2
    max_output = 0;
    for settings in (5..10).permutations(5) {
        let output = solve7b(&tape, settings);
        max_output = max_output.max(output);
    }
    println!("Part 2: {}", max_output);
}


#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn intcode_amplifier_programs() {

        let mut program = vec![3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0];
        assert_eq!(solve7b(&program, vec![4,3,2,1,0]), 43210);

        program = vec![3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0];
        assert_eq!(solve7b(&program, vec![0,1,2,3,4]), 54321);

        program = vec![3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                       1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0];
        assert_eq!(solve7b(&program, vec![1,0,4,3,2]), 65210);
    }
}
