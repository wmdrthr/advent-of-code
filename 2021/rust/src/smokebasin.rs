use std::collections::{HashMap, HashSet};
use std::collections::VecDeque;

type V2 = (i32, i32);

static DIRECTIONS: [V2; 4] = [(0, -1), (0, 1), (-1, 0), (1, 0)];

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

fn solve1(heightmap: &HashMap<V2, u32>) -> (Vec<V2>, u32) {

    let mut lowpoints: Vec<V2> = Vec::new();
    let mut risk_level: u32 = 0;

    for (point, height) in heightmap.iter() {

        let mut flag: bool = true;

        for (dx, dy) in DIRECTIONS.iter() {
            let nx = point.0 + dx;
            let ny = point.1 + dy;

            match heightmap.get(&(nx, ny)) {
                Some(h) => {
                    if h <= height {
                        flag = false;
                        break;
                    }
                },
                None => { continue; }
            };
        }

        if flag {
            lowpoints.push(*point);
            risk_level += *height + 1;
        }
    }

    (lowpoints, risk_level)
}

fn bfs(heightmap: &HashMap<V2, u32>, start: V2) -> Vec<V2> {

    let mut history: HashSet<V2> = HashSet::from([start]);
    let mut queue: VecDeque<V2> = VecDeque::from([start]);

    while !queue.is_empty() {

        let point = queue.pop_front().unwrap();
        let height = heightmap[&point];

        for (dx, dy) in DIRECTIONS.iter() {
            let nx = point.0 + dx;
            let ny = point.1 + dy;

            if history.contains(&(nx, ny)) {
                continue;
            }

            match heightmap.get(&(nx, ny)) {
                Some(h) => {
                    if h != &9 && h > &height {
                        history.insert((nx, ny));
                        queue.push_back((nx, ny));
                    }
                },
                None => { continue; }
            };
        }

    }

    history.drain().collect::<Vec<V2>>()
}

fn solve2(heightmap: HashMap<V2, u32>, lowpoints: Vec<V2>) -> u32 {

    let mut basin_sizes: Vec<u32> = Vec::new();
    for lowpoint in lowpoints {
        let basin = bfs(&heightmap, lowpoint);
        basin_sizes.push(basin.len() as u32);
    }

    basin_sizes.sort_unstable();
    basin_sizes.reverse();

    basin_sizes[0] * basin_sizes[1] * basin_sizes[2]
}

fn parse(data: String) -> HashMap<V2, u32> {

    let mut heightmap: HashMap<V2, u32> = HashMap::new();

    for (row, line) in data.split('\n').enumerate() {
        for (col, char) in line.chars().enumerate() {

            heightmap.insert((row as i32, col as i32), parse_digit(char));
        }
    }

    heightmap
}

// Day 9: Smoke Basin
pub fn solve(data: String) {

    let heightmap = parse(data);

    let (lowpoints, risk_level) = solve1(&heightmap);
    let basins_size = solve2(heightmap, lowpoints);

    println!("{}", risk_level);
    println!("{}", basins_size);
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

        let input = load_input_file("../inputs/testinput09.txt");
        let heightmap = parse(input);

        let (lowpoints, risk_level) = solve1(&heightmap);
        assert_eq!(risk_level, 15);

        let basins_size = solve2(heightmap, lowpoints);
        assert_eq!(basins_size, 1134);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input09.txt");
        let heightmap = parse(input);

        let (lowpoints, risk_level) = solve1(&heightmap);
        assert_eq!(risk_level, 417);

        let basins_size = solve2(heightmap, lowpoints);
        assert_eq!(basins_size, 1148965);
    }
}
