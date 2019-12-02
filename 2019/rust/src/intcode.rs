pub fn intcode_run(program: &Vec<u32>, noun: u32, verb: u32) -> Vec<u32> {

    let mut memory = program.clone();
    memory[1] = noun;
    memory[2] = verb;
    let mut iptr: usize = 0;

    loop {

        if memory[iptr] == 99 { break }

        let input_a = memory[iptr+1] as usize;
        let input_b = memory[iptr+2] as usize;
        let output  = memory[iptr+3] as usize;

        match memory[iptr] {
            1 => { memory[output] = memory[input_a] + memory[input_b] }
            2 => { memory[output] = memory[input_a] * memory[input_b] }
            _ => { println!("invalid opcode: {}", memory[iptr]); }
        }
        iptr += 4;
    }

    memory
}


pub fn solve(data: String) {

    let program: Vec<u32> = data.split(",")
        .map(|l| l.parse::<u32>().unwrap())
        .collect();

    // Part 1
    let memory = intcode_run(&program, 12, 2);
    println!("Part 1: {}", memory[0]);

    // Part 2
    for x in 0..100 {
        for y in 0..100 {
            let memory = intcode_run(&program, x, y);
            if memory[0] == 19690720 {
                println!("Part 2: {}", (100 * x + y));
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn intcode_program_1() {
        let input = vec![1,0,0,0,99];
        assert_eq!(intcode_run(&input, 0, 0), vec![2,0,0,0,99])
    }

    #[test]
    fn intcode_program_2() {
        let input = vec![2,3,0,3,99];
        assert_eq!(intcode_run(&input, 3, 0), vec![2,3,0,6,99])
    }

    #[test]
    fn intcode_program_3() {
        let input = vec![2,4,4,5,99,0];
        assert_eq!(intcode_run(&input, 4, 4), vec![2,4,4,5,99,9801])
    }

        #[test]
    fn intcode_program_4() {
        let input = vec![1,1,1,4,99,5,6,0,99];
        assert_eq!(intcode_run(&input, 1, 1), vec![30,1,1,4,2,5,6,0,99])
    }

}
