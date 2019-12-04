pub use crate::adventofcode::*;
use std::collections::HashMap;
use std::collections::HashSet;

fn parse(segment: &str) -> Vec<V2> {
    let direction = segment.chars().next().unwrap();
    let distance = segment[1..]
        .parse::<i32>()
        .expect("invalid input: distance");

    let vectors: Vec<V2> = (0..distance)
        .map(|_| match direction {
            'L' => (-1, 0),
            'R' => (1, 0),
            'U' => (0, -1),
            'D' => (0, 1),
            _ => panic!("invalid input"),
        })
        .collect();

    vectors
}

fn path(wire: &str) -> HashSet<V2> {
    let vectors: Vec<V2> = wire.split(",").map(|p| parse(p)).flatten().collect();

    let mut visited = HashSet::new();
    let mut current = (0, 0);
    for v in vectors {
        current = (current.0 + v.0, current.1 + v.1);
        visited.insert(current);
    }

    visited
}

fn path_with_signal_delay(wire: &str) -> (HashSet<V2>, HashMap<V2, u32>) {
    let vectors: Vec<V2> = wire.split(",").map(|p| parse(p)).flatten().collect();

    let mut visited: HashSet<V2> = HashSet::new();
    let mut signal_delays: HashMap<V2, u32> = HashMap::new();
    let mut current: V2 = (0, 0);
    let mut delay: u32 = 1;
    for v in vectors {
        current = (current.0 + v.0, current.1 + v.1);
        visited.insert(current);
        signal_delays.insert(current, delay);
        delay += 1;
    }

    (visited, signal_delays)
}

fn solve1(wire1: &str, wire2: &str) -> i32 {
    let path1 = path(wire1);
    let path2 = path(wire2);

    let mut intersections = path1
        .intersection(&path2)
        .map(|p| manhattan(*p, (0, 0)))
        .collect::<Vec<i32>>();
    intersections.sort_unstable();

    *intersections.first().expect("world gone mad")
}

fn solve2(wire1: &str, wire2: &str) -> u32 {
    let (path1, delays1) = path_with_signal_delay(wire1);
    let (path2, delays2) = path_with_signal_delay(wire2);

    let mut intersections = path1
        .intersection(&path2)
        .map(|p| delays1.get(p).unwrap() + delays2.get(p).unwrap())
        .collect::<Vec<u32>>();
    intersections.sort_unstable();

    *intersections.first().expect("world gone mad")
}

pub fn solve(data: String) {
    let wires: Vec<&str> = data.split("\n").collect();

    println!("Part 1: {}", solve1(wires[0], wires[1]));
    println!("Part 2: {}", solve2(wires[0], wires[1]));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crossingwires_a1() {
        assert_eq!(solve1("R8,U5,L5,D3", "U7,R6,D4,L4"), 6);
    }

    #[test]
    fn crossingwires_a2() {
        assert_eq!(solve2("R8,U5,L5,D3", "U7,R6,D4,L4"), 30);
    }

    #[test]
    fn crossingwires_b1() {
        assert_eq!(
            solve1(
                "R75,D30,R83,U83,L12,D49,R71,U7,L72",
                "U62,R66,U55,R34,D71,R55,D58,R83"
            ),
            159
        );
    }

    #[test]
    fn corssingwires_b2() {
        assert_eq!(
            solve2(
                "R75,D30,R83,U83,L12,D49,R71,U7,L72",
                "U62,R66,U55,R34,D71,R55,D58,R83"
            ),
            610
        );
    }

    #[test]
    fn crossingwires_c1() {
        assert_eq!(
            solve1(
                "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
                "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
            ),
            135
        );
    }

    #[test]
    fn crossingwires_c2() {
        assert_eq!(
            solve2(
                "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
                "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
            ),
            410
        );
    }
}
