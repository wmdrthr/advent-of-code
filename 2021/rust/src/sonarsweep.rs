use itertools::izip;

fn solve1(values: &Vec<u32>) -> u32 {

    let mut count = 0;

    for pair in values.iter().zip(values.iter().skip(1)) {
        if pair.1 > pair.0 {
            count += 1;
        }
    }

    count
}

fn solve2(values: &Vec<u32>) -> u32 {

    let sums: Vec<u32> = izip!(values.iter(), values.iter().skip(1), values.iter().skip(2))
        .map(|(a, b, c)| a + b + c)
        .collect();

    solve1(&sums)
}


// Day 1: Sonar Sweep
pub fn solve(data: String) {

    let values: Vec<u32> = data.split("\n").
        map(|e| e.parse::<u32>().unwrap())
        .collect();

    println!("Part 1: {}\nPart 2: {}", solve1(&values), solve2(&values));
}


#[cfg(test)]
mod tests {

    use super::*;
    use std::fs;

    fn load_input_file(filename: &str) -> String {
        fs::read_to_string(filename).unwrap().trim().to_string()
    }

    #[test]
    fn testinput() {
        assert_eq!(solve1(&vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263]), 7);
        assert_eq!(solve2(&vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263]), 5);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input01.txt");
        let values: Vec<u32> = input.split("\n").
            map(|e| e.parse::<u32>().unwrap())
            .collect();

        assert_eq!(solve1(&values), 1162);
        assert_eq!(solve2(&values), 1190);
    }
}
