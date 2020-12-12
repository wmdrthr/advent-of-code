use self::Direction::*;

type V2 = (i32, i32);

#[derive(Clone, Debug)]
enum Direction {
    North, South, East, West
}

#[derive(PartialEq, Clone)]
enum Action {
    N, S, E, W, L, R, F
}

pub fn manhattan(a: (i32, i32), b: (i32, i32)) -> i32 {
    (a.0 - b.0).abs() + (a.1 - b.1).abs()
}

fn move_position(position: V2, direction: &Direction, magnitude: i32) -> V2 {
    match direction {
        North => { (position.0            , position.1 - magnitude) },
        South => { (position.0            , position.1 + magnitude) },
        East  => { (position.0 + magnitude, position.1            ) },
        West  => { (position.0 - magnitude, position.1            ) },
    }
}

fn turn(heading: Direction, action: Action, angle: i32)-> Direction {

    let mut rotations = angle / 90;
    let mut current_heading = heading.clone();

    loop {
        if rotations == 0 {
            break current_heading;
        }

        match action {
            Action::L => {
                current_heading = match current_heading {
                    North => West, South => East, East => North, West => South
                };
            },
            Action::R => {
                current_heading = match current_heading {
                    North => East, South => West, East => South, West => North
                };
            }
            _ => { panic!("invalid call"); }
        }

        rotations -= 1;
    }
}

fn rotate(position: V2, action: Action, angle: i32) -> V2 {

    let mut rotations = angle / 90;
    let mut current_position = position.clone();

    loop {
        if rotations == 0 {
            break current_position;
        }

        match action {
            Action::L => {
                current_position = (current_position.1, (-1) * current_position.0);
            },
            Action::R => {
                current_position = ((-1) * current_position.1, current_position.0);
            }
            _ => { panic!("invalid call"); }
        }

        rotations -= 1;
    }
}

fn parse_command(line: &str) -> (Action, i32) {

    let command = match line.chars().next().unwrap() {
        'N' => Action::N,
        'S' => Action::S,
        'E' => Action::E,
        'W' => Action::W,
        'L' => Action::L,
        'R' => Action::R,
        'F' => Action::F,
         _  => { panic!("invalid input"); }
    };
    let value = line.get(1..).unwrap().parse::<i32>().unwrap();

    (command, value)
}

fn solve12a(commands: Vec<(Action, i32)>) -> i32 {

    let mut heading = East;
    let mut position = (0, 0);

    for (action, value) in commands {

        if action == Action::F {
            position = move_position(position, &heading, value);
        } else if action == Action::L || action == Action::R {
            heading = turn(heading, action, value);
        } else {
            let direction = match action {
                Action::N => North, Action::S => South,
                Action::E => East, Action::W => West,
                _  => { panic!("invalid input"); }
            };
            position = move_position(position, &direction, value);
        }
    }

    manhattan(position, (0,0))
}


fn solve12b(commands: Vec<(Action, i32)>) -> i32 {

    let mut position = (0, 0);
    let mut waypoint = (10, -1);

    for (action, value) in commands {

        if action == Action::F {
            position = move_position(position, &East,  waypoint.0 * value);
            position = move_position(position, &North, waypoint.1 * value);
        } else if action == Action::L || action == Action::R {
            waypoint = rotate(waypoint, action, value);
        } else {
            let direction = match action {
                Action::N => North, Action::S => South,
                Action::E => East, Action::W => West,
                _  => { panic!("invalid input"); }
            };
            waypoint = move_position(waypoint, &direction, value);
        }
    }

    manhattan(position, (0,0))
}

// Day 12: Rain Risk
pub fn solve(data: String) {

    let commands: Vec<(Action, i32)> = data.split('\n')
        .map(|l| parse_command(l)).
        collect();

    println!("Part 1: {}", solve12a(commands.clone()));
    println!("Part 2: {}", solve12b(commands));
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
        let input = load_input_file("../inputs/testinput12.txt");
        let commands: Vec<(Action, i32)> = input.split('\n')
            .map(|l| parse_command(l)).
            collect();
        assert_eq!(solve12a(commands.clone()), 25);
        assert_eq!(solve12b(commands), 286);
    }

    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input12.txt");
        let commands: Vec<(Action, i32)> = input.split('\n')
            .map(|l| parse_command(l)).
            collect();
        assert_eq!(solve12a(commands.clone()), 439);
        assert_eq!(solve12b(commands), 12385);
    }

}
