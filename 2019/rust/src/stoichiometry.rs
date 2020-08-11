use std::fmt;
use std::collections::HashMap;
use regex::Regex;

#[derive(PartialEq, Eq, Hash)]
struct Input {
    name: String,
    quantity: i64
}

impl fmt::Debug for Input {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.quantity, self.name)
    }
}

impl Input {

    fn new(name: String, quantity: i64) -> Input {
        Input { name, quantity }
    }
}

type Formula = (i64, Vec<Input>);

fn calculate(reactions: &HashMap<String, Formula>,
             target: String,
             mut target_amount: i64,
             surplus: &mut HashMap<String, i64>) -> i64 {

    if target == "ORE" {
        return target_amount;
    }

    let surplus_amount = match surplus.get(&target) {
        None => 0,
        Some(q) => *q
    };

    if target_amount <= surplus_amount {
        surplus.entry(target).and_modify(|q| { *q -= target_amount });
        return 0;
    }

    target_amount -= surplus_amount;
    surplus.insert(target.clone(), 0);

    let mut ore: i64 = 0;
    let (output_amount, inputs) = reactions.get(&target).unwrap();
    let production_ratio = (target_amount as f32 / *output_amount as f32).ceil() as i64;
    for input in inputs {
        let amount = input.quantity * production_ratio;
        ore += calculate(reactions, input.name.clone(), amount, surplus);
    }

    *surplus.get_mut(&target).unwrap() += (output_amount * production_ratio) - target_amount;

    ore
}

fn solve14a(reactions: &HashMap<String, Formula>) -> i64 {

    // Part 1
    let mut surplus: HashMap<String, i64> = HashMap::new();
    calculate(&reactions, String::from("FUEL"), 1, &mut surplus)
}

fn solve14b(reactions: &HashMap<String, Formula>, fuel_cost: i64) -> i64 {

    // Part 2
    let mut ore: i64 = 1000000000000;
    let mut target_fuel = ore / fuel_cost;
    let mut fuel = 0;
    let mut surplus: HashMap<String, i64> = HashMap::new();

    while ore > 0 && target_fuel > 0 {
        let mut new_surplus: HashMap<String, i64> = HashMap::new();
        for (key, value) in surplus.iter() {
            new_surplus.insert(key.clone(), *value);
        }

        let ore_used = calculate(&reactions,
                                 String::from("FUEL"), target_fuel,
                                 &mut new_surplus);
        if ore_used > ore {
            target_fuel /= 2;
        } else {
            fuel += target_fuel;
            ore -= ore_used;
            surplus.clear();
            for (key, value) in new_surplus.iter() {
                surplus.insert(key.clone(), *value);
            }
        }
    }

    fuel
}

fn load_reactions(data: String) -> HashMap<String, Formula> {

    let pattern = Regex::new(r"^((?:\d+ [A-Z]+, )*\d+ [A-Z]+) => (\d+) ([A-Z]+)$").unwrap();
    let mut reactions: HashMap<String, Formula> = HashMap::new();

    for line in data.split("\n") {
        if pattern.is_match(line) {
            let captures = pattern.captures(line).unwrap();
            let output_quantity = captures.get(2).unwrap().as_str().parse::<i64>().unwrap();
            let output_chemical = captures.get(3).unwrap().as_str();

            let inputs = captures.get(1).unwrap().as_str();
            let mut input_chemicals: Vec<Input> = Vec::new();
            for input in inputs.split(", ") {
                let input_data:Vec<&str> = input.split(" ").collect();
                let chemical = input_data.get(1).unwrap().to_string();
                let quantity = input_data.get(0).unwrap().parse::<i64>().unwrap();
                input_chemicals.push(Input::new(chemical, quantity));
            }

            reactions.insert(String::from(output_chemical),
                             (output_quantity, input_chemicals));
        }
    }

    reactions
}

pub fn solve(data: String) {

    let reactions = load_reactions(data);

    let fuel_cost = solve14a(&reactions);
    println!("Part 1: {}", fuel_cost);

    let fuel = solve14b(&reactions, fuel_cost);
    println!("Part 2: {}", fuel);
}


#[cfg(test)]
mod tests {

    use super::*;
    use std::fs;

    fn load_input_file(filename: String) -> String {
        match fs::read_to_string(filename) {
            Ok(contents) => contents,
            Err(_) => {
                println!("test input file not found");
                String::from("")
            }
        }
    }

    #[test]
    fn stoichiometry_test_part1() {
        let test_cases: Vec<(char, i64)> = vec![('a', 31),
                                                ('b', 165),
                                                ('c', 13312),
                                                ('d', 180697),
                                                ('e', 2210736)];

        for (test_case_id, expected_fuel_cost) in test_cases {
            let input_filename = format!("../inputs/testinput14{}.txt", test_case_id);
            let reactions = load_reactions(load_input_file(input_filename));
            let fuel_cost = solve14a(&reactions);
            assert_eq!(fuel_cost, expected_fuel_cost);
        }
    }

    #[test]
    fn stoichiometry_test_part2() {
        let test_cases: Vec<(char, i64, i64)> = vec![('c', 13312, 82892753),
                                                     ('d', 180697, 5586022),
                                                     ('e', 2210736, 460664)];

        for (test_case_id, ore_per_fuel, expected_fuel) in test_cases {
            let input_filename = format!("../inputs/testinput14{}.txt", test_case_id);
            let reactions = load_reactions(load_input_file(input_filename));
            let fuel = solve14b(&reactions, ore_per_fuel);
            assert_eq!(fuel, expected_fuel);
        }
    }
}
