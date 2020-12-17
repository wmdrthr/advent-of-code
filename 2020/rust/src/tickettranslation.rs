use std::collections::{HashMap, HashSet};

type Ticket = Vec<u64>;
type Rules = HashMap<String, HashSet<u64>>;

fn parse(data: String) -> (Rules, Ticket, Vec<Ticket>) {

    let lines: Vec<&str> = data.split('\n').collect();
    let mut line: &str;

    let mut rules: Rules = HashMap::new();
    let mut tickets: Vec<Ticket> = Vec::new();
    let mut my_ticket: Ticket = Vec::new();

    let mut idx: usize = 0;
    loop {
        line = lines[idx];
        if line == "" {
            break;
        }

        let parts: Vec<&str> = line.split(':').collect();
        let mut rule_set: HashSet<u64> = HashSet::new();
        for rule in parts[1].split(" or ") {
            let values: Vec<u64> = rule.trim()
                .split('-')
                .map(|v| v.parse::<u64>().unwrap())
                .collect();

            for value in values[0]..values[1] + 1 {
                rule_set.insert(value);
            }
        }
        rules.insert(parts[0].to_string(), rule_set);

        idx += 1;
    }

    idx += 2;
    line = lines[idx];
    for value in line.split(',').map(|v| v.parse::<u64>().unwrap()) {
        my_ticket.push(value);
    }

    idx += 3;
    while idx < lines.len() {
        line = lines[idx];
        let mut ticket: Ticket = Vec::new();
        for value in line.split(',').map(|v| v.parse::<u64>().unwrap()) {
            ticket.push(value);
        }
        tickets.push(ticket);
        idx += 1;
    }

    (rules, my_ticket, tickets)
}

fn solve16a(rules: Rules, tickets: Vec<Ticket>) -> (Vec<Ticket>, u64) {

    let mut all_valid_values: HashSet<u64> = HashSet::new();
    let mut invalid_values: Vec<u64> = Vec::new();
    let mut valid_tickets: Vec<Ticket> = Vec::new();

    for rule_set in rules.values() {
        all_valid_values.extend(rule_set);
    }

    for ticket in tickets {
        let mut flag: bool = true;
        for value in &ticket {
            if ! all_valid_values.contains(&value) {
                invalid_values.push(*value);
                flag = false;
                break;
            }
        }

        if flag {
            valid_tickets.push(ticket);
        }
    }

    (valid_tickets, invalid_values.iter().sum())

}

fn match_rules(rules: Rules, valid_tickets: Vec<Ticket>) -> Vec<String> {

    let mut matched_rules: Vec<String> = vec!["NA".to_string(); valid_tickets[0].len()];

    loop {

        if ! matched_rules.contains(&"NA".to_string()) {
            break;
        }

        for (rule_key, rule_values) in rules.iter() {

            if matched_rules.contains(&rule_key) {
                continue;
            }

            let mut possible_rules: HashSet<usize> = HashSet::new();

            for (idx, key) in matched_rules.iter().enumerate() {
                if key != &"NA".to_string() {
                    continue;
                }
                let mut flag: bool = true;
                for ticket in &valid_tickets {
                    if ! rule_values.contains(&ticket[idx]) {
                        flag = false;
                        break;
                    }
                }

                if flag {
                    possible_rules.insert(idx);
                }
            }

            if possible_rules.len() == 1 {
                let idx = possible_rules.drain().next().unwrap();
                matched_rules[idx] = rule_key.clone();
                break;
            }
        }
    }

    matched_rules
}

fn solve16b(rules: Rules, valid_tickets: Vec<Ticket>, my_ticket: Ticket) -> u64 {

    let matched_rules = match_rules(rules, valid_tickets);

    let mut product = 1;
    for (idx, rule_key) in matched_rules.iter().enumerate() {

        if rule_key.starts_with("departure") {
            product *= my_ticket[idx];
        }
    }

    product
}

// Day 16: Ticket Translation
pub fn solve(data: String) {

    let (rules, my_ticket, tickets) = parse(data);

    let (valid_tickets, mut res) = solve16a(rules.clone(), tickets);
    println!("Part 1: {}", res);

    res = solve16b(rules, valid_tickets, my_ticket);
    println!("Part 2: {}", res);
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::fs;

    fn load_input_file(filename: &str) -> String {
        fs::read_to_string(filename).unwrap().trim().to_string()
    }

    #[test]
    fn invalid_values() {
        let input = load_input_file("../inputs/testinput16a.txt");
        let (rules, _, tickets) = parse(input);

        let (_, res) = solve16a(rules, tickets);
        assert_eq!(res, 71);
    }

    #[test]
    fn matching_rules() {
        let input = load_input_file("../inputs/testinput16b.txt");
        let (rules, _, tickets) = parse(input);

        let (valid_tickets, _) = solve16a(rules.clone(), tickets);
        let matched_rules = match_rules(rules, valid_tickets);
        assert_eq!(matched_rules, vec!["row", "class", "seat"]);
    }


    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input16.txt");
        let (rules, my_ticket, tickets) = parse(input);

        let (valid_tickets, mut res) = solve16a(rules.clone(), tickets);
        assert_eq!(res, 23954);

        res = solve16b(rules, valid_tickets, my_ticket);
        assert_eq!(res, 453459307723);
    }

}
