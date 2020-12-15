
fn solve13a(input: String) -> u64 {

    let schedule: Vec<&str> = input.split('\n').collect();
    if schedule.len() == 1 {
        return 0;
    }

    let start_ts: u64 = schedule[0].parse::<u64>().unwrap();
    let buses: Vec<u64> = schedule[1].split(',')
        .filter(|x| *x != "x")
        .map(|x| x.parse::<u64>().unwrap())
        .collect();

    let mut ts = start_ts;
    loop {
        for bus in &buses {
            if (ts + 1) % bus == 0 {
                let delay = (ts + 1) - start_ts;
                return delay * bus;
            }
        }
        ts += 1;
    }
}

fn mod_pow(mut base: i64, mut exp: i64, modulus: i64) -> i64 {


    if exp == 0 {
        return 1;
    }

    let mut result = 1;

    base = base % modulus;
    while exp > 0 {
        if exp % 2 == 1 {
            result = result * base % modulus;
        }
        exp = exp / 2;
        base = base * base % modulus
    }
    result
}

fn solve13b(input: String) -> i64 {

    let schedule: Vec<&str> = input.split('\n').collect();
    let buses: Vec<(i64, i64)> = schedule.last().unwrap().split(',')
        .enumerate()
        .filter(|(_,x)| *x != "x")
        .map(|(i,n)| {
            let v = n.parse::<i64>().unwrap();
            (v - i as i64, v)
        })
        .collect();

    let mut total: i64 = 0;
    let mut product: i64 = 1;

    for (_, bus) in &buses {
        product *= bus;
    }

    for (idx, bus) in &buses {
        let d = product / bus;
        total += idx * d * mod_pow(d, bus - 2, *bus);
    }

    total % product
}

// Day 13: Shuttle Search
pub fn solve(data: String) {

    let part1 = solve13a(data.clone());
    println!("Part 1: {}", part1);

    let part2 = solve13b(data);
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
    fn testinputs() {
        let input = load_input_file("../inputs/testinput13.txt");
        assert_eq!(solve13a(input.clone()), 295);
        assert_eq!(solve13b(input), 1068781);
    }

    #[test]
    fn extra() {
        assert_eq!(solve13b(String::from("0\n17,x,13,19")), 3417);
        assert_eq!(solve13b(String::from("0\n67,7,59,61")), 754018);
        assert_eq!(solve13b(String::from("0\n67,x,7,59,61")), 779210);
        assert_eq!(solve13b(String::from("0\n67,7,x,59,61")), 1261476);
        assert_eq!(solve13b(String::from("0\n1789,37,47,1889")), 1202161486);
    }

    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input13.txt");
        assert_eq!(solve13a(input.clone()), 3606);
        assert_eq!(solve13b(input), 379786358533423);
    }

}
