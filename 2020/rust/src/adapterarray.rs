use std::collections::{HashSet, HashMap};


fn solve10a(ratings: &HashSet<u64>)-> u64 {

    let max_rating = ratings.iter().max().unwrap();
    let mut differences: Vec<u64> = Vec::new();
    let mut current: u64 = 0;

    while current < *max_rating {
        for difference in vec![1, 2, 3] {
            if ratings.contains(&(current + difference)) {
                differences.push(difference);
                current += difference;
                break;
            }
        }
    }
    differences.push(3); // from max rating to built-in adapter

    let onecount = differences.iter().filter(|v| *v == &1).count();
    let threecount = differences.iter().filter(|v| *v == &3).count();

    (onecount * threecount) as u64
}


fn traverse_adapters(cache: &mut HashMap<u64, u64>, ratings: &HashSet<u64>,
                     max_rating: u64, current: u64) -> u64 {

    match cache.get(&current) {
        Some(result) => result.clone(),
        None => {
            let result = {
                if current == max_rating {
                    return 1;
                }

                let mut count: u64 = 0;
                for difference in vec![1, 2, 3] {
                    if ratings.contains(&(current + difference)) {
                        count += traverse_adapters(cache, &ratings, max_rating, current + difference);
                    }
                }
                count
            };
            cache.insert(current, result.clone());
            result
        }
    }
}

fn solve10b(ratings: HashSet<u64>) -> u64 {
    let max_rating = ratings.iter().max().unwrap().clone();

    traverse_adapters(&mut HashMap::new(), &ratings, max_rating, 0)
}

// Day 10: Adapter Array
pub fn solve(data: String) {

    let ratings: HashSet<u64> = data.split('\n').map(|v| v.parse().unwrap()).collect();

    println!("Part 1: {}", solve10a(&ratings));
    println!("Part 2: {}", solve10b(ratings));
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

        let testcases = vec![("../inputs/testinput10a.txt", (35, 8)),
                             ("../inputs/testinput10b.txt", (220, 19208))];

        for (testfile, expected_value) in testcases {
            let input = load_input_file(testfile);
            let ratings: HashSet<u64> = input.split('\n').map(|v| v.parse().unwrap()).collect();
            assert_eq!(solve10a(&ratings), expected_value.0);
            assert_eq!(solve10b(ratings), expected_value.1);
        }
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input10.txt");
        let ratings: HashSet<u64> = input.split('\n').map(|v| v.parse().unwrap()).collect();
        assert_eq!(solve10a(&ratings), 2590);
        assert_eq!(solve10b(ratings), 226775649501184);
    }

}
