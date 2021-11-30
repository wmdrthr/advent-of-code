use std::collections::HashMap;
use std::slice::Iter;
use self::Direction::*;
use std::io::{stdout, Write};

type Position = (usize, usize);

#[derive(PartialEq, Clone, Debug)]
enum Seat {
    Floor,
    Empty,
    Occupied
}

struct SeatingLayout {
    grid: HashMap<Position, Seat>,
    rows: usize,
    columns: usize
}

impl SeatingLayout {

    fn new(rows: usize,  columns: usize) -> SeatingLayout {
        let grid: HashMap<Position, Seat> = HashMap::new();

        SeatingLayout {grid, rows, columns}
    }

    fn from(data: String) -> SeatingLayout {
        let mut grid: HashMap<Position, Seat> = HashMap::new();
        let mut rows: usize = 0;
        let mut columns: usize = 0;

        for (y,row) in data.split('\n').enumerate() {
            rows += 1;
            if columns == 0 {
                columns = row.len();
            }
            for (x,ch) in row.chars().enumerate() {
                match ch {
                    'L' => { grid.insert((x, y), Seat::Empty);    }
                    '.' => { grid.insert((x, y), Seat::Floor);    }
                    '#' => { grid.insert((x, y), Seat::Occupied); }
                     _  => { panic!("invalid input");             }
                }
            }
        }

        SeatingLayout { grid, rows, columns }
    }

    fn valid_position(&self, position: Position) -> bool {
        if position.0 >= self.columns || position.1 >= self.rows {
            return false;
        }

        true
    }

    fn clone(&self) -> SeatingLayout {

        let grid = self.grid.clone();
        let rows = self.rows.clone();
        let columns = self.columns.clone();

        SeatingLayout { grid, rows, columns }
    }

    fn _display(&self) {

    for y in 0..self.rows {
        for x in 0..self.columns {
            print!("{}", match self.grid.get(&(x, y)).unwrap() {
                Seat::Empty    => 'L',
                Seat::Floor    => '.',
                Seat::Occupied => '#'
            });
        }
        println!();
    }
    println!();
}


}

enum Direction {
    Up, Down, Left, Right,
    UpLeft, UpRight, DownLeft, DownRight
}

impl Direction {

    fn iterator() -> Iter<'static, Direction> {
        static DIRECTIONS: [Direction; 8] = [Up, Down, Left, Right,
                                             UpLeft, UpRight, DownLeft, DownRight];
        DIRECTIONS.iter()
    }
}

fn next(position: Position, direction: &Direction) -> Option<Position> {

    match direction {
        Up => {
            if position.1 > 0 {
                Some((position.0, position.1 - 1))
            } else {
                None
            }
        },
        Down => { Some((position.0, position.1 + 1)) },
        Left => {
            if position.0 > 0 {
                Some((position.0 - 1, position.1    ))
            } else {
                None
            }
        },
        Right => { Some((position.0 + 1, position.1)) },
        UpLeft => {
            if position.0 > 0 && position.1 > 0 {
                Some((position.0 - 1, position.1 - 1))
            } else {
                None
            }
        },
        UpRight => {
            if position.1 > 0 {
                Some((position.0 + 1, position.1 - 1))
            } else {
                None
            }
        },
        DownLeft => {
            if position.0 > 0 {
                Some((position.0 - 1, position.1 + 1))
            } else {
                None
            }
        },
        DownRight => { Some((position.0 + 1, position.1 + 1)) }
    }
}

fn neighbors(layout: &SeatingLayout, position: Position) -> Vec<Position> {

    let mut neighbors: Vec<Position> = Vec::with_capacity(8);
    for direction in Direction::iterator() {
        match next(position, &direction) {
            Some(neighbor) => {
                if layout.valid_position(neighbor) {
                    neighbors.push(neighbor.clone());
                }
            },
            None => {}
        }
    }

    neighbors
}

fn raycasting_neighbors(layout: &SeatingLayout, position: Position) -> Vec<Position> {

    let mut neighbors: Vec<Position> = Vec::with_capacity(8);
    for direction in Direction::iterator() {
        let mut current_position = position.clone();
        loop {
            let next_position = next(current_position, &direction);
            match next_position {
                Some(neighbor) => {
                    if layout.valid_position(neighbor) {
                        if layout.grid.get(&neighbor).unwrap() != &Seat::Floor {
                            neighbors.push(neighbor.clone());
                            break;
                        } else {
                            current_position = neighbor;
                        }
                    } else {
                        break;
                    }
                },
                None => {
                    break;
                }
            }
        }
    }

    neighbors
}

fn step(layout: SeatingLayout,
        neighbor_function: fn(&SeatingLayout, Position) -> Vec<Position>,
        max_occupancy: u32) -> (SeatingLayout, u32) {

    let mut new_layout: SeatingLayout = SeatingLayout::new(layout.rows, layout.columns);
    let mut changes: u32 = 0;

    for y in 0..layout.rows {
        for x in 0..layout.columns {
            if layout.grid.get(&(x, y)).unwrap() == &Seat::Empty {
                let mut empty_neighbors: u32 = 0;
                let mut total_neighbors: u32 = 0;
                for neighbor in neighbor_function(&layout, (x, y)) {
                    total_neighbors += 1;
                    if layout.grid.get(&neighbor).unwrap() == &Seat::Occupied {
                        break;
                    }
                    empty_neighbors += 1;
                }
                if empty_neighbors == total_neighbors {
                    new_layout.grid.insert((x, y).clone(), Seat::Occupied);
                    changes += 1;
                } else {
                    new_layout.grid.insert((x, y).clone(), Seat::Empty);
                }
            } else if layout.grid.get(&(x, y)).unwrap() == &Seat::Occupied {
                let mut occupied_neighbors: u32 = 0;
                for neighbor in neighbor_function(&layout, (x, y)) {
                    if layout.grid.get(&neighbor).unwrap() == &Seat::Occupied {
                        occupied_neighbors += 1;
                        if occupied_neighbors >= max_occupancy {
                            new_layout.grid.insert((x, y).clone(), Seat::Empty);
                            changes += 1;
                            break;
                        }
                    }
                }
                if occupied_neighbors < max_occupancy {
                    new_layout.grid.insert((x, y).clone(), Seat::Occupied);
                }
            } else {
                new_layout.grid.insert((x, y).clone(), Seat::Floor);
            }
        }
    }

    (new_layout, changes)
}

fn solve11a(base_layout: SeatingLayout) -> usize {

    let mut layout = base_layout.clone();

    loop {
        let (new_layout, changes) = step(layout, neighbors, 4);
        if changes == 0 {
            break new_layout.grid.values().filter(|s| *s == &Seat::Occupied).count();
        }
        layout = new_layout;
    }
}

fn solve11b(base_layout: SeatingLayout) -> usize {

    let mut layout = base_layout.clone();

    loop {
        let (new_layout, changes) = step(layout, raycasting_neighbors, 5);
        if changes == 0 {
            break new_layout.grid.values().filter(|s| *s == &Seat::Occupied).count();
        }
        layout = new_layout;
    }
}

// Day 11: Seating System
pub fn solve(data: String) {

    let layout = SeatingLayout::from(data);

    println!("Part 1: {}", solve11a(layout.clone()));
    if cfg!(debug_assertions) {
        println!("Part 2 is going to take some time.");
        stdout().flush().unwrap();
    }
    println!("Part 2: {}", solve11b(layout));
}


#[cfg(test)]
mod tests {

    use super::*;
    use std::fs;

    fn load_input_file(filename: &str) -> String {
        fs::read_to_string(filename).unwrap().trim().to_string()
    }

    #[test]
    fn test_raycasting_neighbors() {

        let testcases = vec![("../inputs/testinput11b.txt", (3, 4), 8, vec![(3, 1), (3, 8), (2, 4), (8, 4),
                                                                            (1, 2), (7, 0), (0, 7), (4, 5)]),
                             ("../inputs/testinput11c.txt", (1, 1), 1, vec![(3, 1)]),
                             ("../inputs/testinput11d.txt", (3, 3), 0, vec![])];

        for(testfile, position, neighbor_count, expected_result) in testcases {
            let input = load_input_file(testfile);
            let layout = SeatingLayout::from(input);

            let neighbors = raycasting_neighbors(&layout, position);
            assert_eq!(neighbors.len(), neighbor_count);
            assert_eq!(neighbors, expected_result);
        }

    }

    #[test]
    fn testinputs() {
        let input = load_input_file("../inputs/testinput11a.txt");
        let layout = SeatingLayout::from(input);

        assert_eq!(solve11a(layout.clone()), 37);
        assert_eq!(solve11b(layout), 26);
    }

    #[cfg(feature="slow_tests")]
    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input11.txt");
        let layout = SeatingLayout::from(input);

        assert_eq!(solve11a(layout.clone()), 2481);
        assert_eq!(solve11b(layout), 2227);
    }
}
