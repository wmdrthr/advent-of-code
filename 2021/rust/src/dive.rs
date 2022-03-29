

#[derive(PartialEq, Debug)]
enum Command {
    Forward, Down, Up
}

fn parse(line: &str) -> (Command, u32) {

    let parts: Vec<&str> = line.split(' ').collect();

    let command = match *parts.get(0).unwrap() {
        "forward" => Command::Forward,
        "down"    => Command::Down,
        "up"      => Command::Up,
        _         => { panic!("invalid input"); }
    };

    let value = parts.get(1).unwrap().parse::<u32>().unwrap();

    (command, value)
}

fn navigate(course: &Vec<(Command, u32)>, using_aim: bool) -> u32 {

    let mut aim: u32 = 0;
    let mut depth: u32 = 0;
    let mut position: u32 = 0;

    for (command, value) in course {

        match command {
            Command::Forward => {
                position += value;
                if using_aim {
                    depth += aim * value;
                }
            },
            Command::Down => {
                if using_aim {
                    aim += value;
                } else {
                    depth += value;
                }
            },
            Command::Up => {
                if using_aim {
                    aim -= value;
                } else {
                    depth -= value;
                }
            }
        }
    }

    depth * position
}


// Day 2: Dive!
pub fn solve(data: String) {

    let course: Vec<(Command, u32)> = data.split('\n')
        .map(|l| parse(l))
        .collect();

    println!("Part 1: {}\nPart 2: {}",
             navigate(&course, false),
             navigate(&course, true));
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
        let test_input = vec![(Command::Forward, 5),
                              (Command::Down, 5),
                              (Command::Forward, 8),
                              (Command::Up, 3),
                              (Command::Down, 8),
                              (Command::Forward, 2)];
        assert_eq!(navigate(&test_input, false), 150);
        assert_eq!(navigate(&test_input, true), 900);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input02.txt");
        let course: Vec<(Command, u32)> = input.split('\n')
            .map(|l| parse(l))
            .collect();

        assert_eq!(navigate(&course, false), 1648020);
        assert_eq!(navigate(&course, true), 1759818555);
    }
}
