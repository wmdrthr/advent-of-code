use regex::Regex;

fn valid_part1(password: &str, letter: &char,
               min: u32, max: u32) -> bool {

    let count = password.chars().filter(|c| c == letter).count() as u32;

    min <= count && count <= max

}

fn valid_part2(password: &str, letter: char,
               a: u32, b: u32) -> bool {

    let characters: Vec<char> = password.chars().collect();

    (characters[(a - 1) as usize] == letter) ^ (characters[(b - 1) as usize] == letter)

}

fn parse(line: &str) -> (&str, char, u32, u32) {

    lazy_static! {
        static ref RE: Regex = Regex::new(r"(\d+)-(\d+)\s(.):\s(.+)").unwrap();
    }

    let captures = RE.captures(line).unwrap();
    let a: u32 = captures.get(1).unwrap().as_str().parse::<u32>().unwrap();
    let b: u32 = captures.get(2).unwrap().as_str().parse::<u32>().unwrap();
    let letter: char = captures.get(3).unwrap().as_str().chars().next().unwrap();
    let password = captures.get(4).unwrap().as_str();

    (password, letter, a, b)
}

fn calculate(entries: Vec<&str>) -> (u32, u32) {

    let mut valid1: u32 = 0;
    let mut valid2: u32 = 0;

    for entry in entries.iter() {
        let (password, letter, a, b) = parse(entry);

        if valid_part1(password, &letter, a, b) {
            valid1 += 1;
        }

        if valid_part2(password, letter, a, b) {
            valid2 += 1;
        }
    }

    (valid1, valid2)
}

// Day 2: Password Philosophy
pub fn solve(data: String) {

    let entries: Vec<&str> = data.split("\n").collect();

    let (valid1, valid2) = calculate(entries);
    println!("Part 1: {}\nPart 2: {}\n", valid1, valid2);
}


#[cfg(test)]
mod tests {

    use super::*;
    use std::fs;

    fn load_input_file(filename: &str) -> String {
        fs::read_to_string(filename).unwrap().trim().to_string()
    }

    #[test]
    fn valid_ruleset1() {
        assert_eq!(valid_part1("abcde", &'a', 1, 3), true);
        assert_eq!(valid_part1("cdefg", &'b', 1, 3), false);
        assert_eq!(valid_part1("ccccccccc", &'c', 2, 9), true);
    }

    #[test]
    fn valid_ruleset2() {
        assert_eq!(valid_part2("abcde", 'a', 1, 3), true);
        assert_eq!(valid_part2("cdefg", 'b', 1, 3), false);
        assert_eq!(valid_part2("ccccccccc", 'c', 2, 9), false);
    }

    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input02.txt");

        let entries: Vec<&str> = input.split("\n").collect();
        assert_eq!(calculate(entries), (528, 497));
    }
}
