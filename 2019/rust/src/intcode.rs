use std::collections::VecDeque;

#[derive(Debug)]
enum Mode {
    Position, Immediate
}
type Modes = (Mode, Mode, Mode);

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
    input: Option<VecDeque<i32>>,
    output: Option<VecDeque<i32>>
}

impl VM {
    fn new(program: Vec<i32>, input: Option<VecDeque<i32>>, output: Option<VecDeque<i32>>) -> VM {
        VM {
            memory: program,
            ip: 0,
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
        let value: i32 = match &mut self.input {
            Some(q) => match q.pop_front() {
                Some(v) => v,
                None    => panic!("Input Required")
            }
            None    => panic!("Illegal Instruction")
        };

        self.memory[output as usize] = value;
        self.ip += 2;
    }

    fn write(&mut self, modes: Modes) {
        let output = self.fetch(self.ip + 1, modes.0);
        match &mut self.output {
            Some(q) => q.push_back(output),
            None    => panic!("Illegal Instruction")
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
            _ => panic!(format!("Invalid opcode: {} at location {}", m % 100, self.ip)),
        };
    }

    fn run(&mut self) {

        loop {
            if self.memory[self.ip] == 99 {
                break;
            }

            self.step();
        }
    }
}

pub fn intcode_basic_run(program: Vec<i32>) -> VM {
    let mut vm = VM::new(program, None, None);
    vm.run();
    vm
}

pub fn intcode_advanced_run(program: Vec<i32>, inputs: Vec<i32>) -> VM {

    let mut input_queue: VecDeque<i32> = VecDeque::new();
    for it in inputs {
        input_queue.push_back(it);
    }
    let mut vm = VM::new(program, Some(input_queue), Some(VecDeque::new()));
    vm.run();
    vm
}

// Solver for Day 2
pub fn solve2(data: String) {
    let tape: Vec<i32> = data.split(",").map(|l| l.parse::<i32>().unwrap()).collect();

    // Part 1
    let mut tape1 = tape.clone();
    tape1[1] = 12; tape1[2] = 2;
    let vm = intcode_basic_run(tape1);
    println!("Part 1: {}", vm.memory[0]);

    // Part 2
    for x in 0..100 {
        for y in 0..100 {
            let mut tape2 = tape.clone();
            tape2[1] = x; tape2[2] = y;
            let vm = intcode_basic_run(tape2);
            if vm.memory[0] == 19690720 {
                println!("Part 2: {}", (100 * x + y));
                return;
            }
        }
    }
}

// Solver for Day 5
pub fn solve5(data: String) {

    let tape: Vec<i32> = data.split(",") .map(|l| l.parse::<i32>().unwrap()).collect();

    // Part 1
    let mut vm = intcode_advanced_run(tape.clone(), vec![1]);
    let drained = vm.output.unwrap().drain(0..).filter(|v| *v != 0).collect::<VecDeque<_>>();
    println!("Part 1: {}", drained[0]);

    // Part 2
    vm = intcode_advanced_run(tape.clone(), vec![5]);
    println!("Part 2: {}", vm.output.unwrap().pop_front().unwrap());
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
            let vm = intcode_basic_run(inputs[n].clone());
            assert_eq!(vm.memory, outputs[n]);
        }
    }

    #[test]
    fn intcode_advanced_programs_1() {
        let mut vm = intcode_advanced_run(vec![3,9,8,9,10,9,4,9,99,-1,8], vec![8]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 1);
        vm = intcode_advanced_run(vec![3,9,8,9,10,9,4,9,99,-1,8], vec![5]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 0);
        vm = intcode_advanced_run(vec![3,9,8,9,10,9,4,9,99,-1,8], vec![12]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 0);
    }

    #[test]
    fn intcode_advanced_programs_2() {
        let mut vm = intcode_advanced_run(vec![3,9,7,9,10,9,4,9,99,-1,8], vec![8]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 0);
        vm = intcode_advanced_run(vec![3,9,7,9,10,9,4,9,99,-1,8], vec![5]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 1);
        vm = intcode_advanced_run(vec![3,9,7,9,10,9,4,9,99,-1,8], vec![12]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 0);
    }

    #[test]
    fn intcode_advanced_programs_3() {
        let mut vm = intcode_advanced_run(vec![3,3,1108,-1,8,3,4,3,99], vec![8]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 1);
        vm = intcode_advanced_run(vec![3,3,1108,-1,8,3,4,3,99], vec![5]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 0);
        vm = intcode_advanced_run(vec![3,3,1108,-1,8,3,4,3,99], vec![12]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 0);
    }

    #[test]
    fn intcode_advanced_programs_4() {
        let mut vm = intcode_advanced_run(vec![3,3,1107,-1,8,3,4,3,99], vec![8]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 0);
        vm = intcode_advanced_run(vec![3,3,1107,-1,8,3,4,3,99], vec![5]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 1);
        vm = intcode_advanced_run(vec![3,3,1107,-1,8,3,4,3,99], vec![12]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 0);
    }

    #[test]
    fn intcode_advanced_programs_5() {
        let mut vm = intcode_advanced_run(vec![3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], vec![8]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 1);
        vm = intcode_advanced_run(vec![3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], vec![0]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 0);

        vm = intcode_advanced_run(vec![3,3,1105,-1,9,1101,0,0,12,4,12,99,1], vec![8]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 1);
        vm = intcode_advanced_run(vec![3,3,1105,-1,9,1101,0,0,12,4,12,99,1], vec![0]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 0);
    }

    #[test]
    fn intcode_advanced_programs_6() {
        let mut vm = intcode_advanced_run(vec![3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99],
                                          vec![8]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 1000);

        vm = intcode_advanced_run(vec![3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99],
                                  vec![5]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 999);

        vm = intcode_advanced_run(vec![3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99],
                                  vec![12]);
        assert_eq!(vm.output.unwrap().pop_front().unwrap(), 1001);
    }
}
