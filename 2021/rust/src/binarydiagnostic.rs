use std::collections::HashMap;

fn count_bits(numbers: &Vec<&str>, column: usize) -> HashMap<char, u32> {

    let mut counter = HashMap::new();

    for number in numbers {
        let c: char = number.chars().nth(column).unwrap();
        let val = counter.entry(c).or_insert(0);
        *val += 1;
    }

    counter
}

fn solve1(numbers: &Vec<&str>) -> u32 {

    let mut gamma: String = String::new();
    let mut epsilon: String = String::new();

    for column in 0..numbers[0].len() {

        let counter = count_bits(&numbers, column);

        if counter[&'1'] > counter[&'0'] {
            gamma.push('1');
            epsilon.push('0');
        } else {
            gamma.push('0');
            epsilon.push('1');
        }
    }

    u32::from_str_radix(&gamma, 2).unwrap() * u32::from_str_radix(&epsilon, 2).unwrap()
}

#[derive(PartialEq, Debug)]
enum Gas {
    Oxygen,
    CarbonDioxide
}

fn life_support_rating(mut numbers: Vec<&str>, gas: Gas) -> u32 {

    let mut column: usize = 0;

    while numbers.len() > 1 && column < numbers[0].len() {

        let counter = count_bits(&numbers, column);

        let selected_bit = match gas {
            Gas::Oxygen => {
                if counter[&'1'] >= counter[&'0'] {
                    '1'
                } else {
                    '0'
                }
            },
            Gas::CarbonDioxide => {
                if counter[&'0'] <= counter[&'1'] {
                    '0'
                } else {
                    '1'
                }
            }
        };

        numbers.retain(|&n| {
            let c: char = n.chars().nth(column).unwrap();
            c == selected_bit
        });

        column += 1;
    }

    u32::from_str_radix(numbers[0], 2).unwrap()
}



fn solve2(numbers: Vec<&str>) -> u32 {

    let oxygen = life_support_rating(numbers.clone(), Gas::Oxygen);
    let carbondioxide = life_support_rating(numbers.clone(), Gas::CarbonDioxide);

    oxygen * carbondioxide
}

// Day 3: Binary Diagnostic
pub fn solve(data: String) {

    let numbers: Vec<&str> = data.split('\n').collect();


    println!("Part 1: {}\nPart 2: {}", solve1(&numbers), solve2(numbers));

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
        let test_input = vec!["00100",
                              "11110",
                              "10110",
                              "10111",
                              "10101",
                              "01111",
                              "00111",
                              "11100",
                              "10000",
                              "11001",
                              "00010",
                              "01010"];
        assert_eq!(solve1(&test_input), 198);
        assert_eq!(solve2(test_input), 230);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input03.txt");
        let numbers: Vec<&str> = input.split('\n').collect();

        assert_eq!(solve1(&numbers), 2035764);
        assert_eq!(solve2(numbers), 2817661);
    }
}
