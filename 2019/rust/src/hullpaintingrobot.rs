use crate::intcode::*;
use crate::crossingwires::manhattan;
use std::collections::HashMap;
use std::collections::HashSet;


type V2 = (i32, i32);

#[derive(Debug)]
enum Color {
    Black,
    White,
}

impl Color {
    fn value(&self) -> i64 {
        match *self {
            Color::Black => 0,
            Color::White => 1
        }
    }
}

impl From<i64> for Color {
    fn from(v: i64) -> Self {
        match v {
            0 => Color::Black,
            1 => Color::White,
            _ => panic!("World gone mad!")
        }
    }
}

enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn next(location: V2, direction: Direction, turn: i64) -> (V2, Direction) {

    let (x, y) = location;

    match (direction, turn) {
        (Direction::Up   , 0) => { ((x - 1, y    ), Direction::Left ) },
        (Direction::Up   , 1) => { ((x + 1, y    ), Direction::Right) },
        (Direction::Left , 0) => { ((x    , y + 1), Direction::Down ) },
        (Direction::Left , 1) => { ((x    , y - 1), Direction::Up   ) },
        (Direction::Right, 0) => { ((x    , y - 1), Direction::Up   ) },
        (Direction::Right, 1) => { ((x    , y + 1), Direction::Down ) },
        (Direction::Down , 0) => { ((x + 1, y    ), Direction::Right) },
        (Direction::Down , 1) => { ((x - 1, y    ), Direction::Left ) },
        _ => panic!("world gone mad!")
    }
}


fn solve11(tape: Vec<i64>, panel: &mut HashMap<V2, Color>) -> HashSet<V2> {

    let mut painted: HashSet<V2> = HashSet::new();

    let (tx, rx) = intcode_spawn(tape.clone(), vec![]);
    let mut robot_location = (0, 0);
    let mut robot_direction = Direction::Up;

    loop {

        let color = panel.get(&robot_location).unwrap_or(&Color::Black);
        match tx.send(color.value()) {
            Err(_) => { break; },
            Ok(_)  => {}
        }

        // color
        match rx.recv() {
            Err(_) => { break; },
            Ok(color) => {
                panel.insert(robot_location.clone(), Color::from(color));
                painted.insert(robot_location.clone());
            }
        }

        // turn
        let turn = rx.recv().unwrap();
        let result = next(robot_location, robot_direction, turn);
        robot_location = result.0;
        robot_direction = result.1;
    }

    painted
}

// Day 11: Space Police

pub fn solve(data: String) {
    let tape: Vec<i64> = data.split(",").map(|l| l.parse::<i64>().unwrap()).collect();

    let mut panel: HashMap<V2, Color> = HashMap::new();

    let painted = solve11(tape.clone(), &mut panel);
    println!("Part 1: {}", painted.len());


    let mut panel: HashMap<V2, Color> = HashMap::new();
    panel.insert((0, 0), Color::White);
    solve11(tape.clone(), &mut panel);

    let mut points = panel.keys().map(|k| k.clone()).collect::<Vec<V2>>();
    points.sort_unstable_by(|a, b| manhattan(*a, (0, 0)).cmp(&manhattan(*b, (0, 0))));

    println!("Part 2: EFCKUEGC");
    let (maxx, maxy) = points.last().unwrap();
    for y in 0..maxy + 1 {
        for x in 0..maxx + 1 {
            match panel.get(&(x, y)) {
                None => { print!(" "); },
                Some(color) => {
                    match color {
                        Color::Black => { print!("░"); },
                        Color::White => { print!("█"); }
                    }
                }
            }
        }
        println!();
    }
}
