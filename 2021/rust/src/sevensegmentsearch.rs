use std::collections::HashMap;


static CANONICAL_PATTERN: &str = "abcefg cf acdeg acdfg bdcf abdfg abdefg acf abcdefg abcdfg";

fn score(characters: &str) -> HashMap<char, u32> {

    let mut counter: HashMap<char, u32> = HashMap::new();

    for c in characters.chars() {

        if c != ' ' {
            let entry = counter.entry(c).or_insert(0);
            *entry += 1;
        }
    }

    counter
}

fn solve1(data: &Vec<(&str, &str)>) -> u32 {

    let mut count: u32 = 0;

    for (_, outputs) in data {
        for output in outputs.split(" ") {

            if [2, 3, 4, 7].contains(&output.len()) {
                count += 1;
            }
        }
    }

    count
}

fn solve2(data: Vec<(&str, &str)>) -> u32 {

    let canonical_scores = score(CANONICAL_PATTERN);
    let mut lookup_table: HashMap<u32, u32> = HashMap::new();

    for (digit, signals) in CANONICAL_PATTERN.split(' ').enumerate() {

        let score: u32 = signals.chars().map(|c| canonical_scores[&c]).sum();
        lookup_table.insert(score, digit as u32);
    }

    let mut total: u32 = 0;
    for (signals, outputs) in data {
        let mut number: u32 = 0;
        let score: HashMap<char, u32> = score(signals);
        for output in outputs.trim().split(' ') {
            let mut digit_score =0;
            for c in output.chars() {
                digit_score += score[&c];
            }
            let digit = lookup_table[&digit_score];
            number = (number * 10) + digit;
        }
        total += number;
    }

    total
}

fn parse(input: &String) -> Vec<(&str, &str)> {

    input.split('\n')
        .map(|l| {
            let parts = l.split(" | ").collect::<Vec<&str>>();
            (parts[0], parts[1])
        })
        .collect()
}

// Day 8: Seven Segment Search
pub fn solve(input: String) {

    let data = parse(&input);

    println!("{:?}", solve1(&data));
    println!("{:?}", solve2(data));
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

        let input = load_input_file("../inputs/testinput08.txt");
        let data = parse(&input);

        assert_eq!(solve1(&data), 26);
        assert_eq!(solve2(data), 61229);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input08.txt");
        let data = parse(&input);

        assert_eq!(solve1(&data), 344);
        assert_eq!(solve2(data), 1048410);
    }
}
