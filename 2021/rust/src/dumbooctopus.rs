use std::collections::{HashMap, HashSet};

type V2 = (i32, i32);
type Grid = HashMap<V2, u32>;

static DIRECTIONS: [V2; 8] = [( 0, -1), ( 0, 1), (-1,  0), (1, 0),
                              (-1, -1), (-1, 1), ( 1, -1), (1, 1)];

fn parse_digit(c: char) -> u32 {

    match c {
        '0' => 0,
        '1' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
         _  => { panic!("Invalid input!"); }
    }
}

fn parse(data: String) -> Grid {

    let mut grid: Grid = HashMap::new();

    for (row, line) in data.split('\n').enumerate() {
        for (col, char) in line.chars().enumerate() {

            grid.insert((row as i32, col as i32), parse_digit(char));
        }
    }

    grid
}

fn calculate(mut octopuses: Grid) -> (u32, u32) {

    let mut flashes: Vec<u32> = Vec::new();
    flashes.push(0);

    let mut steps: u32 = 0;

    loop {

        let total_energy_level:u32 = octopuses.values().sum();
        if total_energy_level == 0 {
            break;
        }

        for (_, val) in octopuses.iter_mut() {
            *val += 1;
        }

        let mut flashed: HashSet<V2> = HashSet::new();

        loop {
            let ready_to_flash:Vec<V2> =
                octopuses.iter()
                .filter(|(_, v)| *v > &9)
                .map(|(k, _)| k.clone())
                .collect();
            if ready_to_flash.len() == 0 {
                break;
            }

            for octopus in ready_to_flash {

                if flashed.contains(&octopus) {
                    continue;
                }

                flashed.insert(octopus);

                for (dx, dy) in DIRECTIONS {
                    let nx = octopus.0 + dx;
                    let ny = octopus.1 + dy;

                    if let Some(v) = octopuses.get_mut(&(nx, ny)) {
                        *v += 1;
                    }
                }
            }

            for octopus in &flashed {
                if let Some(v) = octopuses.get_mut(&octopus) {
                    *v = 0;
                }
            }
        }

        steps += 1;
        flashes.push(flashes.last().unwrap() + flashed.len() as u32);

    }

    (flashes[100], steps)

}


// Day 11: Dumbo Octopus
pub fn solve(data: String) {

    let octopuses = parse(data);

    let (flashes, steps) = calculate(octopuses);
    println!("{}\n{}", flashes, steps);
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

        let test_input = load_input_file("../inputs/testinput11b.txt");
        let octopuses = parse(test_input);

        let (flashes, steps) = calculate(octopuses);
        assert_eq!(flashes, 1656);
        assert_eq!(steps, 195);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input11.txt");
        let octopuses = parse(input);

        let (flashes, steps) = calculate(octopuses);
        assert_eq!(flashes, 1603);
        assert_eq!(steps, 222);
    }
}
