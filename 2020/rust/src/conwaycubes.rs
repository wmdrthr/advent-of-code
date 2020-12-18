use std::collections::{HashSet, HashMap};
use itertools::Itertools;

type Vn = Vec<i32>;

fn step(active_cubes: HashSet<Vn>, dimensions: usize) -> HashSet<Vn> {

    let mut neighbors: HashMap<Vn, i32> = HashMap::new();

    for cube in &active_cubes {
        for offset in (0..dimensions).map(|_| (-1)..2).multi_cartesian_product() {
            if offset == vec![0; dimensions] {
                continue;
            }

            let neighbor: Vn = cube.iter().zip(offset.iter())
                .map(|(a,b)| a + b)
                .collect();
            *neighbors.entry(neighbor).or_insert(0) += 1;
        }
    }

    let mut new_active_cubes: HashSet<Vn> = HashSet::new();

    for (cube, count) in neighbors.iter() {
        if active_cubes.contains(cube) {
            if *count == 2 || *count == 3 {
                new_active_cubes.insert(cube.clone());
            }
        } else if *count == 3 {
            new_active_cubes.insert(cube.clone());
        }
    }

    new_active_cubes
}

fn parse(data: String, dimensions: usize) -> HashSet<Vn> {

    let mut active_cubes: HashSet<Vn> = HashSet::new();

    for (y, row) in data.split('\n').enumerate() {
        for (x, ch) in row.chars().enumerate() {
            if ch == '#' {
                let mut cube: Vn = vec![0; dimensions];
                cube[0] = x as i32;
                cube[1] = y as i32;
                active_cubes.insert(cube);
            }
        }
    }

    active_cubes
}

pub fn solve17a(data: String) -> usize {

    let mut active_cubes_3d = parse(data, 3);

    for _ in 0..6 {
        active_cubes_3d = step(active_cubes_3d, 3);
    }

    active_cubes_3d.len()
}

pub fn solve17b(data: String) -> usize {

    let mut active_cubes_4d = parse(data, 4);

    for _ in 0..6 {
        active_cubes_4d = step(active_cubes_4d, 4);
    }

    active_cubes_4d.len()
}

// Day 17: Conway Cubes
pub fn solve(data: String) {

    let part1 = solve17a(data.clone());
    println!("Part 1: {}", part1);

    let part2 = solve17b(data.clone());
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
        let input = load_input_file("../inputs/testinput17.txt");
        assert_eq!(solve17a(input.clone()), 112);
        assert_eq!(solve17b(input), 848);
    }

    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input17.txt");
        assert_eq!(solve17a(input.clone()), 391);
        assert_eq!(solve17b(input), 2264);
    }

}
