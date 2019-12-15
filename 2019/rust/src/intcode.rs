use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;
use std::thread;

#[derive(Debug)]
enum Mode {
    Position, Immediate, Relative
}
type Modes = (Mode, Mode, Mode);

#[derive(PartialEq, Debug)]
enum State {
    INIT, RUNNING, HALTED, CRASHED
}

fn get_mode(mode: i64) -> Mode {
    match mode {
        0 => Mode::Position,
        1 => Mode::Immediate,
        2 => Mode::Relative,
        _ => panic!(format!("Invalid mode {}", mode))
    }
}

pub struct VM {
    pub memory: Vec<i64>,
    ip: usize,
    base: i64,
    state: State,
    input: Option<Receiver<i64>>,
    output: Option<Sender<i64>>
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
    pub fn new(program: Vec<i64>, input: Option<Receiver<i64>>, output: Option<Sender<i64>>) -> VM {
        VM {
            memory: program,
            ip: 0,
            base: 0,
            state: State::INIT,
            input,
            output
        }
    }

    fn resize(&mut self, m: usize) {
        if m >= self.memory.len() {
            self.memory.resize(2 * m, 0);
        }
    }

    fn get(&mut self, m: usize) -> i64 {
        self.resize(m);
        self.memory[m]
    }

    fn set(&mut self, m: usize, v: i64) {
        self.resize(m);
        self.memory[m] = v;
    }

    fn fetch(&mut self, m: usize, mode: Mode) -> i64 {
        let v = self.get(m);
        match mode {
            Mode::Position  => self.get(v as usize),
            Mode::Immediate => v,
            Mode::Relative  => self.get((v + self.base) as usize)
        }
    }

    fn dest(&mut self, m: usize, mode: Mode) -> usize {
        let output = match mode {
            Mode::Relative => (self.get(m) + self.base) as usize,
            _ => self.get(m) as usize
        };
        self.resize(output);
        output
    }

    fn add(&mut self, modes: Modes) {
        let operand_a = self.fetch(self.ip + 1, modes.0);
        let operand_b = self.fetch(self.ip + 2, modes.1);
        let output = self.dest(self.ip + 3, modes.2);

        self.set(output as usize, operand_a + operand_b);
        self.ip += 4;
    }

    fn mul(&mut self, modes: Modes) {
        let operand_a = self.fetch(self.ip + 1, modes.0);
        let operand_b = self.fetch(self.ip + 2, modes.1);
        let output = self.dest(self.ip + 3, modes.2);

        self.set(output as usize, operand_a * operand_b);
        self.ip += 4;
    }

    fn read(&mut self, modes: Modes) {
        let output = self.dest(self.ip + 1, modes.0);
        let value: i64 = match &self.input {
            Some(rx) => rx.recv().unwrap(),
            None    => crash!(self, "Illegal Instruction")
        };

        self.set(output as usize, value);
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
        let output = self.dest(self.ip + 3, modes.2);

        if value_a < value_b {
            self.set(output as usize, 1);
        } else {
            self.set(output as usize, 0);
        }
        self.ip += 4;
    }

    fn equals(&mut self, modes: Modes) {
        let value_a = self.fetch(self.ip + 1, modes.0);
        let value_b = self.fetch(self.ip + 2, modes.1);
        let output = self.dest(self.ip + 3, modes.2);

        if value_a == value_b {
            self.set(output as usize, 1);
        } else {
            self.set(output as usize, 0);
        }
        self.ip += 4;
    }

    fn adj_base(&mut self, modes: Modes) {
        let value = self.fetch(self.ip + 1, modes.0);
        self.base += value;
        self.ip += 2
    }

    fn step(&mut self) {
        let m: i64 = self.get(self.ip);
        let flags: i64 = m / 100;
        let modes = (get_mode(flags % 10),
                     get_mode((flags / 10) % 10),
                     get_mode(flags / 100));
        match m % 100 {
            1 => self.add(modes),
            2 => self.mul(modes),
            3 => self.read(modes),
            4 => self.write(modes),
            5 => self.jump_if_true(modes),
            6 => self.jump_if_false(modes),
            7 => self.less_than(modes),
            8 => self.equals(modes),
            9 => self.adj_base(modes),
            _ => crash!(self, format!("Invalid opcode: {} at location {}", m % 100, self.ip)),
        };
    }

    pub fn run(&mut self) {

        self.state = State::RUNNING;
        loop {
            if self.get(self.ip) == 99 {
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

pub fn intcode_spawn(program: Vec<i64>, inputs: Vec<i64>) -> (Sender<i64>, Receiver<i64>) {

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


#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn intcode_basic_programs() {
        let inputs: Vec<Vec<i64>> = vec![
            vec![1, 0, 0, 0, 99],
            vec![2, 3, 0, 3, 99],
            vec![2, 4, 4, 5, 99, 0],
            vec![1, 1, 1, 4, 99, 5, 6, 0, 99]];

        let outputs: Vec<Vec<i64>> = vec![
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
    fn intcode_advanced_programs_7() {

        let tape = vec![109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99];
        let (_, rx) = intcode_spawn(tape.clone(), vec![0]);
        let mut output = Vec::new();
        loop {
            match rx.recv() {
                Ok(v) => output.push(v),
                Err(_) => break
            }
        }
        assert_eq!(output, tape);

        let (_, rx) = intcode_spawn(vec![1102,34915192,34915192,7,4,7,99,0], vec![0]);
        assert_eq!(rx.recv().unwrap(), 1219070632396864);

        let (_, rx) = intcode_spawn(vec![104,1125899906842624,99], vec![0]);
        assert_eq!(rx.recv().unwrap(), 1125899906842624);
    }
}
