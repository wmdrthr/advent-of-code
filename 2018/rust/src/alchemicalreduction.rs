use std::collections::HashSet;

fn check(polymer: &String, unit: char) -> bool {
    if polymer.len() > 0 {
        let last_char = polymer.chars().last().unwrap();
        return unit != last_char && unit.to_ascii_uppercase() == last_char.to_ascii_uppercase();
    } else {
        return false;
    }
}

fn react(mut polymer: String, unit: char) -> String {
    if polymer.len() > 0 {
        if check(&polymer, unit) {
            polymer.pop();
            polymer
        } else {
            polymer.push(unit.clone());
            polymer
        }
    } else {
        polymer.push(unit.clone());
        polymer
    }
}

pub fn solve(data: String) {
    let polymer = data.chars().fold(String::new(), react);
    let base_len = polymer.len();
    println!("{}", base_len);

    let units: HashSet<char> = polymer.chars().map(|c| c.to_ascii_lowercase()).collect();

    let mut min_len = base_len;
    for unit in units {
        let base_polymer = polymer.clone();
        let mut stripped_polymer: String = String::with_capacity(base_len);
        for c in base_polymer.chars() {
            if c != unit && c.to_ascii_lowercase() != unit {
                stripped_polymer.push(c);
            }
        }

        let simplified_polymer = stripped_polymer.chars().fold(String::new(), react);
        if simplified_polymer.len() < min_len {
            min_len = simplified_polymer.len();
        }
    }

    println!("{}", min_len);
}
