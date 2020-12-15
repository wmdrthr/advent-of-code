use std::collections::HashMap;
use std::io::{stdout, Write};

fn memory_game(numbers: Vec<u64>, limit: u64) -> u64 {

    let mut history: HashMap<u64, u64> = HashMap::new();

    for (idx, number) in numbers.iter().enumerate() {
        history.insert(*number, idx as u64);
    }

    let mut last_number: u64 = *numbers.last().unwrap();
    let mut number: u64 = 0;
    let mut last_turn: u64;

    for turn in numbers.len() as u64..limit {

        match history.get(&last_number) {
            Some(n) => {
                last_turn = *n;
                number = turn - last_turn - 1;
            },
            None => { number = 0; }
        };

        history.insert(last_number, turn - 1);
        last_number = number;
    }

    number
}

// Day 15: Rambunctious Recitation
pub fn solve(data: String) {

    let numbers: Vec<u64> = data.split(',')
        .map(|n| n.parse::<u64>().unwrap())
        .collect();

    println!("Part 1: {}", memory_game(numbers.clone(), 2020));
    if cfg!(debug_assertions) {
        println!("Part 2 is going to take some time.");
        stdout().flush().unwrap();
    }
    println!("Part 2: {}", memory_game(numbers, 30000000));
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::fs;

    fn load_input_file(filename: &str) -> String {
        fs::read_to_string(filename).unwrap().trim().to_string()
    }

    #[test]
    fn limit_2020() {
        assert_eq!(memory_game(vec![0,3,6], 2020), 436);
        assert_eq!(memory_game(vec![1,3,2], 2020), 1);
        assert_eq!(memory_game(vec![2,1,3], 2020), 10);
        assert_eq!(memory_game(vec![1,2,3], 2020), 27);
        assert_eq!(memory_game(vec![2,3,1], 2020), 78);
        assert_eq!(memory_game(vec![3,2,1], 2020), 438);
        assert_eq!(memory_game(vec![3,1,2], 2020), 1836);
    }

    #[cfg(feature="slow_tests")]
    #[test]
    fn limit_30000000() {
        assert_eq!(memory_game(vec![0,3,6], 30000000), 175594);
        assert_eq!(memory_game(vec![1,3,2], 30000000), 2578);
        assert_eq!(memory_game(vec![2,1,3], 30000000), 3544142);
        assert_eq!(memory_game(vec![1,2,3], 30000000), 261214);
        assert_eq!(memory_game(vec![2,3,1], 30000000), 6895259);
        assert_eq!(memory_game(vec![3,2,1], 30000000), 18);
        assert_eq!(memory_game(vec![3,1,2], 30000000), 362);
    }

    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input15.txt");
        let numbers: Vec<u64> = input.split(',')
            .map(|n| n.parse::<u64>().unwrap())
            .collect();
        assert_eq!(memory_game(numbers.clone(), 2020), 706);
        if cfg!(feature="slow_tests") {
            assert_eq!(memory_game(numbers, 30000000), 19331);
        }
    }
}
