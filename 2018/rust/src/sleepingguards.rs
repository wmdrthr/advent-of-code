use regex::Regex;
use std::collections::HashMap;

fn solve1(sleep_detail: &HashMap<u32, HashMap<u32, u32>>) -> u32 {

    let sleepiest_guard: &u32 = sleep_detail
        .keys()
        .max_by_key(|k| sleep_detail.get(k).unwrap().values().sum::<u32>())
        .unwrap();
    let max_hours_slept: &u32 = sleep_detail
        .get(sleepiest_guard)
        .unwrap()
        .keys()
        .max_by_key(|k| sleep_detail.get(sleepiest_guard).unwrap().get(k).unwrap())
        .unwrap();
    (*sleepiest_guard) * (*max_hours_slept)
}

fn solve2(sleep_detail: &HashMap<u32, HashMap<u32, u32>>) -> u32 {
    let sleepiest_guard: &u32 = sleep_detail
        .keys()
        .max_by_key(|k| sleep_detail.get(k).unwrap().values().max())
        .unwrap();
    let sleepiest_minute: &u32 = sleep_detail
        .get(sleepiest_guard)
        .unwrap()
        .keys()
        .max_by_key(|k| sleep_detail.get(sleepiest_guard).unwrap().get(k).unwrap())
        .unwrap();
    (*sleepiest_guard) * (*sleepiest_minute)
}

pub fn solve(data: String) {
    let pattern: Regex = Regex::new(r"\d+").unwrap();

    let mut sleep_detail: HashMap<u32, HashMap<u32, u32>> = HashMap::new();
    let mut current_guard: u32 = 0;
    let mut start: u32 = 0;

    let mut input: Vec<&str> = data.split("\n")
        .filter(|l| l.len() > 0)
        .collect();
    input.sort();

    for line in input {
        let numbers: Vec<u32> = pattern
            .find_iter(&line)
            .into_iter()
            .map(|m| m.as_str().parse::<u32>().unwrap())
            .collect();

        let suffix = line.get(line.len() - 2..).unwrap();

        if suffix == "ft" {
            current_guard = *numbers.last().unwrap();
        } else if suffix == "ep" {
            start = numbers[4];
        } else if suffix == "up" {
            for hour in start .. numbers[4] {
                let entry = sleep_detail
                    .entry(current_guard)
                    .or_insert(HashMap::new())
                    .entry(hour)
                    .or_insert(0);
                *entry += 1;
            }
        } else {
            panic!("invalid input");
        }
    }

    println!("Part 1: {}", solve1(&sleep_detail));
    println!("Part 2: {}", solve2(&sleep_detail));
}
