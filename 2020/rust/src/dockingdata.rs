use std::collections::HashMap;

fn solve14a(program: Vec<&str>) -> u64 {

    let mut memory: HashMap<u64, u64> = HashMap::new();

    let mut and_mask: u64 = 0;
    let mut or_mask: u64 = 0;

    for line in program {
        if line.starts_with("mask") {

            let mut mask = line.clone().trim().get(7..).unwrap().replace("X", "0");
            or_mask = u64::from_str_radix(&mask, 2).unwrap();

            mask = line.trim().get(7..).unwrap().replace("X", "1");
            and_mask = u64::from_str_radix(&mask, 2).unwrap();
        } else {
            let parts: Vec<&str> = line.split(" = ").collect();
            let address: u64 = parts[0][4..parts[0].len() - 1].parse::<u64>().unwrap();
            let mut value = parts[1].parse::<u64>().unwrap();

            value = (value | or_mask) & and_mask;
            memory.insert(address, value);
        }
    }

    memory.values().sum()

}

fn floating_addresses(pos: u64, mask: String) -> Vec<u64> {

    let mut addresses: Vec<u64> = Vec::new();

    if mask.len() == 0 {
        addresses.push(0);
        return addresses;
    }

    if mask.chars().last().unwrap() == '0' {
        for addr in floating_addresses(pos / 2, mask[0..mask.len()-1].to_string()) {
            addresses.push(2 * addr + pos % 2);
        }
    } else if mask.chars().last().unwrap() == '1' {
        for addr in floating_addresses(pos / 2, mask[0..mask.len()-1].to_string()) {
            addresses.push(2 * addr + 1);
        }
    } else if mask.chars().last().unwrap() == 'X' {
        for addr in floating_addresses(pos / 2, mask[0..mask.len()-1].to_string()) {
            addresses.push(2 * addr + 0);
            addresses.push(2 * addr + 1);
        }
    } else {
        println!("blah: {}", mask.chars().last().unwrap());
    }

    addresses
}

fn solve14b(program: Vec<&str>) -> u64 {

    let mut memory: HashMap<u64, u64> = HashMap::new();

    let mut mask: String = String::from("");

    for line in program {
        if line.starts_with("mask") {
            mask = line.get(7..).unwrap().trim().to_string().clone();
        } else {
            let parts: Vec<&str> = line.split(" = ").collect();
            let address: u64 = parts[0][4..parts[0].len() - 1].parse::<u64>().unwrap();
            let value = parts[1].parse::<u64>().unwrap();

            let addresses = floating_addresses(address, mask.clone());
            for addr in addresses {
                memory.insert(addr, value);
            }
        }
    }

    memory.values().sum()
}

// Day 14: Docking Data
pub fn solve(data: String) {

    let program: Vec<&str> = data.split('\n').collect();

    let part1 = solve14a(program.clone());
    println!("Part 1: {}", part1);

    let part2 = solve14b(program);
    println!("Part 2: {}", part2);
}


#[cfg(test)]
mod tests {

    use super::*;
    use std::fs;

    fn load_input_file(filename: &str) -> String {
        fs::read_to_string(filename).unwrap().trim().to_string()
    }

    #[test]
    fn testinput_part1() {
        let input = load_input_file("../inputs/testinput14a.txt");
        let program: Vec<&str> = input.split('\n').collect();
        assert_eq!(solve14a(program), 165);
    }

    #[test]
    fn testinput_part2() {
        let input = load_input_file("../inputs/testinput14b.txt");
        let program: Vec<&str> = input.split('\n').collect();
        assert_eq!(solve14b(program), 208);
    }

    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input14.txt");
        let program: Vec<&str> = input.split('\n').collect();
        assert_eq!(solve14a(program.clone()), 14925946402938);
        assert_eq!(solve14b(program), 3706820676200);
    }

}
