use std::collections::HashSet;

type V2 = (u32, u32);

fn traverse(treemap: &HashSet<V2>, limits: V2, slope: V2) -> u32 {

    let mut current: V2 = (0, 0);
    let mut count: u32 = 0;

    loop {
        if treemap.contains(&(current.0 % limits.0, current.1)) {
            count += 1;
        }

        if current.1 >= limits.1 {
            break count;
        }

        current = (current.0 + slope.0, current.1 + slope.1);
    }
}

fn make_treemap(data: String) -> (HashSet<V2>, V2) {

    let rows: Vec<&str> = data.split("\n").collect();
    let limits = (rows[0].len() as u32, rows.len() as u32);

    let mut treemap: HashSet<V2> = HashSet::new();

    for (y, line) in rows.iter().enumerate() {
        for (x, val) in line.char_indices() {
            if val == '#' {
                treemap.insert((x as u32, y as u32));
            }
        }
    }

    (treemap, limits)
}

// Day 3: Toboggan Trajectory
pub fn solve(data: String) {

    let (treemap, limits) = make_treemap(data);

    println!("{}", traverse(&treemap, limits, (3, 1)));

    let mut product: u32 = 1;
    for slope in vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] {
        product *= traverse(&treemap, limits, slope)
    }
    println!("{}", product);
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

        let input = load_input_file("../inputs/testinput03.txt");
        let (treemap, limits) = make_treemap(input);

        assert_eq!(traverse(&treemap, limits, (3, 1)), 7);

        let mut product: u32 = 1;
        for slope in vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] {
            product *= traverse(&treemap, limits, slope)
        }
        assert_eq!(product, 336);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input03.txt");
        let (treemap, limits) = make_treemap(input);

        assert_eq!(traverse(&treemap, limits, (3, 1)), 162);

        let mut product: u32 = 1;
        for slope in vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] {
            product *= traverse(&treemap, limits, slope)
        }
        assert_eq!(product, 3064612320);
    }
}
