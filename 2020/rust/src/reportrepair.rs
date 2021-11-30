use itertools::Itertools;

fn calculate(entries: Vec<u32>) -> (u32, u32) {

    let mut product1 = 0;
    let mut product2 = 0;

    for pair in entries.iter().combinations(2) {
        if pair[0] + pair[1] == 2020 {
            product1 = pair[0] * pair[1];
            break;
        }
    }

    for triple in entries.iter().combinations(3) {
        if triple[0] + triple[1] + triple[2] == 2020 {
            product2 = triple[0] * triple[1] * triple[2];
            break;
        }
    }

    (product1, product2)
}


// Day 1 : Report Repair
pub fn solve(data: String) {

    let mut entries: Vec<u32> = data.split("\n").
        map(|e| e.parse::<u32>().unwrap())
        .collect();
    entries.sort_unstable();

    let (product1, product2) = calculate(entries);

    println!("Part 1: {}\nPart2: {}", product1, product2);
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
        assert_eq!(calculate(vec![1721, 979, 366, 299, 675, 1456]), (514579, 241861950));
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input01.txt");
        let entries: Vec<u32> = input.split("\n").
            map(|e| e.parse::<u32>().unwrap())
            .collect();

        assert_eq!(calculate(entries), (703131, 272423970));
    }
}
