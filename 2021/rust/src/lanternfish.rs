use std::collections::HashMap;


fn simulate(starting_pop: &Vec<u32>, days: u32) -> u64 {

    let mut population: HashMap<u32, u64> = HashMap::new();
    for age in starting_pop {
        let entry = population.entry(*age).or_insert(0);
        *entry += 1
    }

    for _ in 0..days {
        let mut next_generation: HashMap<u32, u64> = HashMap::new();

        for (age, count) in population.iter() {
            if *age > 0 {
                let entry = next_generation.entry(age - 1).or_insert(0);
                *entry += count;
            } else {
                let entry6 = next_generation.entry(6).or_insert(0);
                *entry6 += count;
                let entry8 = next_generation.entry(8).or_insert(0);
                *entry8 += count;
            }
        }

        population.clear();
        population.extend(next_generation.iter());
    }

    population.values().sum()
}

// Day 6: Lanternfish
pub fn solve(data: String) {

    let starting_population: Vec<u32> = data.split(",").map(|n| n.parse::<u32>().unwrap()).collect();

    println!("{}\n{}", simulate(&starting_population, 80), simulate(&starting_population, 256));

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

        let starting_population = vec![3,4,3,1,2];

        assert_eq!(simulate(&starting_population, 80), 5934);
        assert_eq!(simulate(&starting_population, 256), 26984457539);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input06.txt");
        let starting_population: Vec<u32> =
            input.split(",")
            .map(|n| n.parse::<u32>().unwrap())
            .collect();

        assert_eq!(simulate(&starting_population, 80), 361169);
        assert_eq!(simulate(&starting_population, 256), 1634946868992);
    }
}
