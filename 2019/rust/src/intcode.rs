use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;
use itertools::Itertools;

#[derive(Debug)]
enum Mode {
    Position, Immediate
}
type Modes = (Mode, Mode, Mode);

#[derive(PartialEq, Debug)]
enum State {
    INIT, RUNNING, HALTED, CRASHED
}

fn get_mode(mode: i32) -> Mode {
    match mode {
        0 => Mode::Position,
        1 => Mode::Immediate,
        _ => panic!(format!("Invalid mode {}", mode))
    }
}

pub struct VM {
    memory: Vec<i32>,
    ip: usize,
    state: State,
    input: Option<Receiver<i32>>,
    output: Option<Sender<i32>>
}

macro_rules! crash {
    ($self:ident, $msg:expr) => {
        {
            $self.state = State::CRASHED;
            println!("VM CRASH: {}", $msg);
            return;
        }
    }
}


impl VM {
    fn new(program: Vec<i32>, input: Option<Receiver<i32>>, output: Option<Sender<i32>>) -> VM {
        VM {
            memory: program,
            ip: 0,
            state: State::INIT,
            input,
            output
        }
    }

    fn fetch(&self, m: usize, mode: Mode) -> i32 {
        let v = self.memory[m];
        match mode {
            Mode::Position => self.memory[v as usize],
            Mode::Immediate => v
        }
    }

    fn add(&mut self, modes: Modes) {
        let operand_a = self.fetch(self.ip + 1, modes.0);
        let operand_b = self.fetch(self.ip + 2, modes.1);
        let output = self.memory[self.ip + 3];

        self.memory[output as usize] = operand_a + operand_b;
        self.ip += 4;
    }

    fn mul(&mut self, modes: Modes) {
        let operand_a = self.fetch(self.ip + 1, modes.0);
        let operand_b = self.fetch(self.ip + 2, modes.1);
        let output = self.memory[self.ip + 3];

        self.memory[output as usize] = operand_a * operand_b;
        self.ip += 4;
    }

    fn read(&mut self) {
        let output = self.memory[self.ip + 1];
        let value: i32 = match &self.input {
            Some(rx) => rx.recv().unwrap(),
            None    => crash!(self, "Illegal Instruction")
        };

        self.memory[output as usize] = value;
        self.ip += 2;
    }

    fn write(&mut self, modes: Modes) {
        let output = self.fetch(self.ip + 1, modes.0);
        match &self.output {
            Some(tx) => tx.send(output).unwrap(),
            None    => crash!(self, "Illegal Instruction")
        };
        self.ip += 2;
    }

    fn jump_if_true(&mut self, modes: Modes) {
        let value = self.fetch(self.ip + 1, modes.0);
        if value != 0 {
            self.ip = self.fetch(self.ip + 2, modes.1) as usize;
        } else {
            self.ip += 3;
        }
    }

    fn jump_if_false(&mut self, modes: Modes) {
        let value = self.fetch(self.ip + 1, modes.0);
        if value == 0 {
            self.ip = self.fetch(self.ip + 2, modes.1) as usize;
        } else {
            self.ip += 3;
        }
    }

    fn less_than(&mut self, modes: Modes) {
        let value_a = self.fetch(self.ip + 1, modes.0);
        let value_b = self.fetch(self.ip + 2, modes.1);
        let output = self.memory[self.ip + 3];

        if value_a < value_b {
            self.memory[output as usize] = 1;
        } else {
            self.memory[output as usize] = 0;
        }
        self.ip += 4;
    }

    fn equals(&mut self, modes: Modes) {
        let value_a = self.fetch(self.ip + 1, modes.0);
        let value_b = self.fetch(self.ip + 2, modes.1);
        let output = self.memory[self.ip + 3];

        if value_a == value_b {
            self.memory[output as usize] = 1;
        } else {
            self.memory[output as usize] = 0;
        }
        self.ip += 4;
    }

    fn step(&mut self) {
        let m: i32 = self.memory[self.ip];
        let flags: i32 = m / 100;
        let modes = (get_mode(flags % 10),
                     get_mode((flags / 10) % 10),
                     get_mode(flags / 100));
        match m % 100 {
            1 => self.add(modes),
            2 => self.mul(modes),
            3 => self.read(),
            4 => self.write(modes),
            5 => self.jump_if_true(modes),
            6 => self.jump_if_false(modes),
            7 => self.less_than(modes),
            8 => self.equals(modes),
            _ => crash!(self, format!("Invalid opcode: {} at location {}", m % 100, self.ip)),
        };
    }

    fn run(&mut self) {

        self.state = State::RUNNING;
        loop {
            if self.memory[self.ip] == 99 {
                self.state = State::HALTED;
                break;
            }

            self.step();
            if self.state != State::RUNNING {
                break;
            }
        }
    }
}

pub fn intcode_spawn(program: Vec<i32>, inputs: Vec<i32>) -> (Sender<i32>, Receiver<i32>) {

    let (input_tx, input_rx)   = mpsc::channel();
    let (output_tx, output_rx) = mpsc::channel();

    let mut vm = VM::new(program, Some(input_rx), Some(output_tx));

    thread::spawn(move || {
        vm.run();
    });

    for it in inputs {
        input_tx.send(it).unwrap();
    }

    (input_tx, output_rx)
}


// Solver for Day 2: 1202 Program Alarm
pub fn solve2(data: String) {
    let tape: Vec<i32> = data.split(",").map(|l| l.parse::<i32>().unwrap()).collect();

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


// Solver for Day 5: Sunny with a Chance of Asteroids
pub fn solve5(data: String) {

    let tape: Vec<i32> = data.split(",") .map(|l| l.parse::<i32>().unwrap()).collect();

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


// Solver for Day 7: Amplification Circuit
fn solve7b(tape: &Vec<i32>, settings: Vec<i32>) -> i32 {

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
                let v: i32 = rx_ea.recv().unwrap();
                match tx.send(v) {
                    Ok(_)  => {},
                    Err(_) => { tx_out.send(v).unwrap(); break; }
                }
            }
        });
    handler.join().unwrap();

    rx_out.recv().unwrap()
}


pub fn solve7(data: String) {

    let tape: Vec<i32> = data.split(",") .map(|l| l.parse::<i32>().unwrap()).collect();

    // Part 1
    let mut max_output: i32 = 0;
    for setting in (0..5).permutations(5) {
        let mut current: i32 = 0;
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
    fn intcode_basic_programs() {
        let inputs: Vec<Vec<i32>> = vec![
            vec![1, 0, 0, 0, 99],
            vec![2, 3, 0, 3, 99],
            vec![2, 4, 4, 5, 99, 0],
            vec![1, 1, 1, 4, 99, 5, 6, 0, 99]];

        let outputs: Vec<Vec<i32>> = vec![
            vec![2, 0, 0, 0, 99],
            vec![2, 3, 0, 6, 99],
            vec![2, 4, 4, 5, 99, 9801],
            vec![30, 1, 1, 4, 2, 5, 6, 0, 99]];


        for n in 0..4 {
            let mut vm = VM::new(inputs[n].clone(), None, None);
            vm.run();
            assert_eq!(vm.memory, outputs[n]);
        }
    }

    #[test]
    fn intcode_advanced_programs_1() {
        let (_, rx) = intcode_spawn(vec![3,9,8,9,10,9,4,9,99,-1,8], vec![8]);
        assert_eq!(rx.recv().unwrap(), 1);

        let (_, rx) = intcode_spawn(vec![3,9,8,9,10,9,4,9,99,-1,8], vec![5]);
        assert_eq!(rx.recv().unwrap(), 0);

        let (_, rx) = intcode_spawn(vec![3,9,8,9,10,9,4,9,99,-1,8], vec![12]);
        assert_eq!(rx.recv().unwrap(), 0);
    }

    #[test]
    fn intcode_advanced_programs_2() {
        let (_, rx) = intcode_spawn(vec![3,9,7,9,10,9,4,9,99,-1,8], vec![8]);
        assert_eq!(rx.recv().unwrap(), 0);

        let (_, rx) = intcode_spawn(vec![3,9,7,9,10,9,4,9,99,-1,8], vec![5]);
        assert_eq!(rx.recv().unwrap(), 1);

        let (_, rx) = intcode_spawn(vec![3,9,7,9,10,9,4,9,99,-1,8], vec![12]);
        assert_eq!(rx.recv().unwrap(), 0);
    }

    #[test]
    fn intcode_advanced_programs_3() {
        let (_, rx) = intcode_spawn(vec![3,3,1108,-1,8,3,4,3,99], vec![8]);
        assert_eq!(rx.recv().unwrap(), 1);

        let (_, rx) = intcode_spawn(vec![3,3,1108,-1,8,3,4,3,99], vec![5]);
        assert_eq!(rx.recv().unwrap(), 0);

        let (_, rx) = intcode_spawn(vec![3,3,1108,-1,8,3,4,3,99], vec![12]);
        assert_eq!(rx.recv().unwrap(), 0);
    }

    #[test]
    fn intcode_advanced_programs_4() {
        let (_, rx) = intcode_spawn(vec![3,3,1107,-1,8,3,4,3,99], vec![8]);
        assert_eq!(rx.recv().unwrap(), 0);

        let (_, rx) = intcode_spawn(vec![3,3,1107,-1,8,3,4,3,99], vec![5]);
        assert_eq!(rx.recv().unwrap(), 1);

        let (_, rx) = intcode_spawn(vec![3,3,1107,-1,8,3,4,3,99], vec![12]);
        assert_eq!(rx.recv().unwrap(), 0);
    }

    #[test]
    fn intcode_advanced_programs_5() {
        let (_, rx) = intcode_spawn(vec![3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], vec![8]);
        assert_eq!(rx.recv().unwrap(), 1);

        let (_, rx) = intcode_spawn(vec![3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], vec![0]);
        assert_eq!(rx.recv().unwrap(), 0);

        let (_, rx) = intcode_spawn(vec![3,3,1105,-1,9,1101,0,0,12,4,12,99,1], vec![8]);
        assert_eq!(rx.recv().unwrap(), 1);

        let (_, rx) = intcode_spawn(vec![3,3,1105,-1,9,1101,0,0,12,4,12,99,1], vec![0]);
        assert_eq!(rx.recv().unwrap(), 0);
    }

    #[test]
    fn intcode_advanced_programs_6() {
        let (_, rx) = intcode_spawn(vec![3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99],
                                    vec![8]);
        assert_eq!(rx.recv().unwrap(), 1000);

        let (_, rx) = intcode_spawn(vec![3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99],
                                    vec![5]);
        assert_eq!(rx.recv().unwrap(), 999);

        let (_, rx) = intcode_spawn(vec![3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99],
                                    vec![12]);
        assert_eq!(rx.recv().unwrap(), 1001);
    }

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
