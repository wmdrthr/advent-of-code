use std::collections::HashMap;

type Rules = HashMap<String, HashMap<String, u32>>;

fn bagcheck(rules: &Rules, color: &str) -> bool {

    let inner_rules = rules.get(color).unwrap();
    if inner_rules.contains_key("shiny gold") {
        return true;
    }

    for (bagcolor, _) in inner_rules.iter() {
        if bagcheck(&rules, bagcolor) {
            return true;
        }
    }

    false
}

fn bagcount(rules: &Rules, color: &str) -> u32 {

    let mut count: u32 = 0;

    let inner_rules = rules.get(color).unwrap();

    for (inner_bag_color, inner_bag_count) in inner_rules.iter() {
        count += inner_bag_count + (inner_bag_count * bagcount(&rules, inner_bag_color));
    }

    count
}

fn calculate(rules: Rules) -> (u32, u32) {

    let part1 = rules.keys()
        .filter(|k| bagcheck(&rules, k))
        .count() as u32;

    let part2 = bagcount(&rules, "shiny gold");

    (part1, part2)
}

fn load_rules(data: String) -> Rules {

    let mut rules: Rules = HashMap::new();

    for line in data.split('\n') {
        let rule: Vec<&str> = line.split(" contain ").collect();
        let parent = rule.get(0).unwrap();
        let parent_color = parent.get(0..(parent.len() - 5)).unwrap();
        let children = rule.get(1).unwrap();

        let mut inner_rules: HashMap<String, u32> = HashMap::new();

        if rule.get(1).unwrap() != &"no other bags." {
            for child in children.split(",") {
                let parts: Vec<&str> = child.trim().split(' ').collect();
                let count = parts.get(0).unwrap().parse::<u32>().unwrap();
                let mut bag_color = parts.get(1).unwrap().to_string();
                bag_color.push(' ');
                bag_color.push_str(parts.get(2).unwrap());

                inner_rules.insert(bag_color.to_string(), count);
            }
        }

        rules.insert(parent_color.to_string(), inner_rules);
    }

    rules
}

// Day 7: Handy Haversacks
pub fn solve(data: String) {

    let rules = load_rules(data);
    let (part1, part2) = calculate(rules);

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

        let testcases = vec![("../inputs/testinput07a.txt", (4, 32)),
                             ("../inputs/testinput07b.txt", (0, 126))];

        for (testfile, expected_value) in testcases {
            let input = load_input_file(testfile);
            let rules = load_rules(input);
            assert_eq!(calculate(rules), expected_value);
        }
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input07.txt");
        let rules = load_rules(input);
        assert_eq!(calculate(rules), (248, 57281));
    }

}
