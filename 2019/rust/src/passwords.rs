use std::iter::FromIterator;
use std::collections::HashMap;

fn increasing(password: &str) -> bool {
    let mut sorted: Vec<char> = password[..].chars().collect();
    sorted.sort();

    password == String::from_iter(sorted)
}

fn cardinality(s: &str) -> HashMap<char,u32> {
    let mut counter: HashMap<char, u32> = HashMap::new();
    for c in s.chars() {
        match counter.insert(c, 1) {
            None    => {},
            Some(n) => { counter.insert(c, n + 1); },
        }
    }

    counter
}

fn duplicates(password: &str) -> bool {
    let mut count: u32 = 0;
    for (_key, val) in cardinality(password).iter() {
        if val > &1 { count += 1; }
    }

    count > 0
}

fn twins(password: &str) -> bool {
    let mut count: u32 = 0;
    for (_key, val) in cardinality(password).iter() {
        if val == &2 { count += 1; }
    }

    count > 0

}

pub fn solve(data: String) {

    let start: u32 = data[0..6].parse::<u32>().expect("invalid input");
    let end: u32 = data[7..].parse::<u32>().expect("invalid input");

    let candidates: Vec<String> = (start..end)
        .map(|p| p.to_string())
        .collect();

    let valid_passwords = candidates
        .iter()
        .filter(|p| increasing(p) && duplicates(p))
        .collect::<Vec<&String>>();

    println!("Part 1: {}", valid_passwords.len());

    let valid_passwords = valid_passwords
        .iter()
        .filter(|p| twins(p))
        .collect::<Vec<&&String>>();
    println!("Part 2: {}", valid_passwords.len());
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn passwords_increasing() {
        assert_eq!(increasing("111111"), true);
        assert_eq!(increasing("223450"), false);
        assert_eq!(increasing("123789"), true);
    }

    #[test]
    fn passwords_duplicates() {
        assert_eq!(duplicates("111111"), true);
        assert_eq!(duplicates("223450"), true);
        assert_eq!(duplicates("123789"), false);
    }

    #[test]
    fn passwords_twins() {
        assert_eq!(twins("112233"), true);
        assert_eq!(twins("123444"), false);
        assert_eq!(twins("111122"), true);
    }
}
