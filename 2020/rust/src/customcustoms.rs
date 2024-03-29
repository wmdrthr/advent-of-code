use std::collections::HashMap;

fn process_responses(group: Vec<&str>) -> HashMap<char, u32> {

    let mut counter = HashMap::new();

    for response in group {
        for answer in response.chars() {
            let val = counter.entry(answer).or_insert(0);
            *val += 1;
        }
    }

    counter
}

fn calculate(groups: Vec<Vec<&str>>) -> (u32, u32) {

    let mut anyone = 0;
    let mut everyone = 0;

    for group in groups {
        let group_size: u32 = group.len() as u32;
        let responses = process_responses(group);

        anyone += responses.len() as u32;

        everyone += responses.values()
            .filter(|v| *v == &group_size)
            .count() as u32;
    }

    (anyone, everyone)
}

// Day 6: Custom Customs
pub fn solve(data: String) {

    let groups: Vec<Vec<&str>> = data.split("\n\n")
        .map(|g| g.split("\n").collect())
        .collect();

    let (anyone, everyone) = calculate(groups);
    println!("Part 1: {}\nPart 2: {}", anyone, everyone);
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

        let testcases = vec![("../inputs/testinput06a.txt", (6, 3)),
                             ("../inputs/testinput06b.txt", (11, 6))];

        for (testfile, expected_value) in testcases {
            let input = load_input_file(testfile);
            let groups: Vec<Vec<&str>> = input.split("\n\n")
                .map(|g| g.split("\n").collect())
                .collect();

            assert_eq!(calculate(groups), expected_value);
        }
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input06.txt");
        let groups: Vec<Vec<&str>> = input.split("\n\n")
            .map(|g| g.split("\n").collect())
            .collect();

        assert_eq!(calculate(groups), (6742, 3447));
    }

}
