
fn execute(program: Vec<&str>) -> (bool, i32) {

    let mut acc: i32 = 0;
    let mut history: Vec<bool> = vec![false; program.len() as usize];
    let mut index: usize = 0;

    return loop {
        if index == program.len() {
            break (true, acc);
        } else if history[index] {
            break (false, acc);
        }

        history[index] = true;

        let parts: Vec<&str> = program[index].split(' ').collect();
        let instr = parts[0];
        let val = parts[1].parse::<i32>().unwrap();

        if instr == "acc" {
            acc += val;
            index += 1;
        } else if instr == "jmp" {
            index = (index as i32 + val) as usize;
        } else if instr == "nop" {
            index += 1;
        }
    }
}


fn calculate(program: Vec<&str>) -> (i32, i32) {

    let (_, part1) = execute(program.clone());
    let mut part2: i32 = 0;

    for (index, line) in program.iter().enumerate() {

        if line.starts_with("acc") {
            continue;
        }

        let mut fixed_program = program.clone();
        let parts: Vec<&str> = fixed_program[index].split(' ').collect();
        let instr = parts[0];
        let val = parts[1];
        let mut fixed_step = String::new();

        if instr == "nop" {
            fixed_step.push_str("jmp");
            fixed_step.push(' ');
            fixed_step.push_str(val);
        } else {
            fixed_step.push_str("nop");
            fixed_step.push(' ');
            fixed_step.push_str(val);
        }
        fixed_program[index] = &fixed_step;

        let (halting, final_value) = execute(fixed_program);
        if halting {
            part2 = final_value;
            break;
        }
    }

    (part1, part2)
}

// Day 8: Handheld Halting
pub fn solve(data: String) {


    let program: Vec<&str> = data.split("\n").collect();

    let (part1, part2) = calculate(program);
    println!("Part 1: {}\nPart 2: {}", part1, part2);
}


#[cfg(test)]
mod tests {

    use super::*;
    use std::fs;

    fn load_input_file(filename: &str) -> String {
        fs::read_to_string(filename).unwrap().trim().to_string()
    }

    #[test]
    fn testinputs() {
        let input = load_input_file("../inputs/testinput08.txt");
        let program: Vec<&str> = input.split("\n").collect();
        assert_eq!(calculate(program), (5, 8));
    }

    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input08.txt");
        let program: Vec<&str> = input.split("\n").collect();
        assert_eq!(calculate(program), (1915, 944));
    }

}
