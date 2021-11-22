use std::collections::HashSet;
use itertools::Itertools;

fn calculate(numbers: Vec<u64>, window_size: usize) -> (u64, u64) {

    let mut window: (usize, usize) = (0, window_size);

    let target: u64;
    let weakness: u64;

    loop {
        let mut sums: HashSet<u64> = HashSet::new();

        for pair in numbers[window.0..window.1].iter().combinations(2) {
            sums.insert(pair[0] + pair[1]);
        }

        if ! sums.contains(&numbers[window.1]) {
            target = numbers[window.1];
            break;
        }

        window = (window.0 + 1, window.1 + 1);
    }

    window = (0, 1);
    let mut partial_sum = numbers[0];
    loop {
        if partial_sum == target {
            let mut range: Vec<u64> = numbers[window.0..window.1].to_vec();
            range.sort();
            weakness = range[0] + range[range.len() - 1];
            break;
        } else if partial_sum < target {
            partial_sum += numbers[window.1];
            window = (window.0, window.1 + 1);
        } else if partial_sum > target {
            partial_sum -= numbers[window.0];
            window = (window.0 + 1, window.1);
        }
    }

    (target, weakness)
}

// Day 9: Encoding Error
pub fn solve(data: String) {

    let numbers: Vec<u64> = data.split('\n')
        .map(|l| l.parse().unwrap())
        .collect();

    let (part1, part2) = calculate(numbers, 25);
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
        let input = load_input_file("../inputs/testinput09.txt");
        let numbers: Vec<u64> = input.split('\n').map(|l| l.parse().unwrap()).collect();
        assert_eq!(calculate(numbers, 5), (127, 62));
    }

    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input09.txt");
        let numbers: Vec<u64> = input.split('\n').map(|l| l.parse().unwrap()).collect();
        assert_eq!(calculate(numbers, 25), (258585477, 36981213));
    }

}
