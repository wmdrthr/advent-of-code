use std::collections::{HashMap, HashSet};

fn parse(data: String) -> HashMap<String, HashSet<String>> {

    let mut graph: HashMap<String, HashSet<String>> = HashMap::new();

    for line in data.split('\n') {
        let parts: Vec<&str> = line.split('-').collect();
        let a = String::from(parts[0]);
        let b = String::from(parts[1]);

        let entry_a = graph.entry(a.clone()).or_insert(HashSet::new());
        entry_a.insert(b.clone());

        let entry_b = graph.entry(b).or_insert(HashSet::new());
        entry_b.insert(a);
    }

    graph
}

fn islower(s: String) -> bool {

    s.chars().all(|c| c.is_lowercase())
}

fn dfs(graph: &HashMap<String, HashSet<String>>,
       current: String,
       history: HashSet<String>,
       flag: bool) -> u32 {

    if current == "end" {
        return 1;
    }

    let mut n: u32 = 0;

    if let Some(neighbors) = graph.get(&current) {

        for neighbor in neighbors {

            if neighbor == "start" {
                continue;
            }

            if islower(neighbor.to_string()) && history.contains(&neighbor.to_string()) {
                if !flag {
                    n +=  dfs(&graph, neighbor.to_string(), history.clone(), true);
                }
            } else {
                let mut subhistory = history.clone();
                subhistory.insert(neighbor.to_string());
                n +=  dfs(&graph,
                          neighbor.to_string(),
                          subhistory,
                          flag);
            }
        }
    }

    n
}

// Day 12: Passage Pathing
pub fn solve(data: String) {

    let graph = parse(data);

    println!("{}", dfs(&graph, String::from("start"), HashSet::new(), true));
    println!("{}", dfs(&graph, String::from("start"), HashSet::new(), false));
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

        let test_data = vec![("../inputs/testinput12a.txt", (10, 36)),
                            ("../inputs/testinput12b.txt", (19, 103)),
                            ("../inputs/testinput12c.txt", (226, 3509))];
        for (filename, answers) in test_data {
            let input = load_input_file(filename);
            let graph = parse(input);

            assert_eq!(dfs(&graph, String::from("start"), HashSet::new(), true), answers.0);
            assert_eq!(dfs(&graph, String::from("start"), HashSet::new(), false), answers.1);
        }
    }

    #[cfg(feature="slow_tests")]
    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input12.txt");
        let graph = parse(input);

        assert_eq!(dfs(&graph, String::from("start"), HashSet::new(), true), 3410);
        assert_eq!(dfs(&graph, String::from("start"), HashSet::new(), false), 98796);
    }
}
