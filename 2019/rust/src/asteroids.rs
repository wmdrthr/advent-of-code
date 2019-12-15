use std::mem;
use std::hash::{Hash, Hasher};
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

type V2 = (i32, i32);

#[derive(Copy, Clone, Debug)]
struct Angle {
    val: f64,
    bits: u64
}

impl Angle {
    pub fn new(val: f64) -> Angle {
        let bits: u64 = unsafe { mem::transmute(val) };
        Angle { val, bits }
    }
}

impl PartialEq for Angle {
    fn eq(&self, other: &Self) -> bool {
        self.bits == other.bits
    }
}

impl Eq for Angle {}

impl Hash for Angle {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.bits.hash(state);
    }
}

fn angle(x: V2, y: V2) -> Angle {
    let dx = (x.0 - y.0) as f64;
    let dy = (y.1 - x.1) as f64;
    let mut angle: f64 = (dx).atan2(dy);

    if angle < 0.0 {
        angle = angle + (2_f64 * std::f64::consts::PI);
    }
    Angle::new(angle)
}


fn solve10a(asteroids: &Vec<V2>) -> (V2, usize)  {

    let mut angles: HashMap<V2, HashSet<Angle>> = HashMap::new();

    for asteroid_pair in (0..asteroids.len()).permutations(2) {
        let asteroid = asteroids[asteroid_pair[0]];
        let other = asteroids[asteroid_pair[1]];
        angles.entry(asteroid).or_insert(HashSet::new()).insert(angle(asteroid, other));
    }

    let mut max_visible: usize = 0;
    let mut station = (0_i32, 0);
    for (asteroid, visible) in angles {
        if visible.len() > max_visible {
            max_visible = visible.len();
            station = asteroid.clone();
        }
    }

    (station, max_visible)
}


#[derive(Eq, PartialEq, Debug)]
struct Target {
    coords: V2,
    distance: i32
}

impl Target {
    fn new(coords: V2, distance: i32) -> Target {
        Target { coords, distance }
    }
}

fn distance(x: V2, y: V2) -> i32 {
    let dx = x.0 - y.0;
    let dy = x.1 - y.1;

    (dx * dx) + (dy * dy)
}

fn solve10b(station: V2, asteroids: &Vec<V2>) -> Option<V2> {

    let mut targets: HashMap<Angle, Vec<Target>> = HashMap::new();

    for asteroid in asteroids {
        if *asteroid != station {
            let target = Target::new(asteroid.clone(), distance(station, *asteroid));
            targets.entry(angle(*asteroid, station)).or_insert(Vec::new()).push(target);
        }
    }

    for (_, target_list) in targets.iter_mut() {
        target_list.sort_by_key(|k| k.distance);
    }

    let mut angles = targets.keys().map(|k| k.clone()).collect::<Vec<Angle>>();
    angles.sort_by_key(|a| a.bits);

    let mut counter: u8 = 0;
    loop {
        let mut no_targets = true;
        for angle in &angles {
            let target_list = targets.get_mut(&angle).unwrap();
            if target_list.len() > 0 {
                let asteroid = target_list.remove(0);
                counter += 1; no_targets = false;
                if counter == 200 {
                    return Some(asteroid.coords);
                }
            }
        }

        if no_targets == true {
            return None;
        }
    }
}

fn load_asteroids(data: String) -> Vec<V2> {

    let mut asteroids: Vec<V2> = Vec::new();

    for (y, line) in data.split("\n").enumerate() {
        for (x, char) in line.chars().enumerate() {
            if char == '#' {
                asteroids.push((x as i32, y as i32));
            }
        }
    }

    asteroids
}

pub fn solve10(data: String) {

    let asteroids = load_asteroids(data);

    let (station, visible) = solve10a(&asteroids);
    println!("{}", visible);

    let asteroid = solve10b(station, &asteroids).unwrap();
    println!("{:?}", (asteroid.0 * 100 + asteroid.1));
}


#[cfg(test)]
mod tests {

    use super::*;
    use std::fs;

    fn load_input_file(filename: String) -> String {
        match fs::read_to_string(filename) {
            Ok(contents) => contents,
            Err(_) => {
                println!("test input file not found");
                String::from("")
            }
        }
    }

    #[test]
    fn asteroids_test_part1() {

        let test_cases: Vec<(char, V2, usize)> = vec![('a', (3, 4), 8),
                                                      ('b', (5, 8), 33),
                                                      ('c', (1, 2), 35),
                                                      ('d', (6, 3), 41),
                                                      ('e', (11, 13), 210)];

        for (test_case_id, expected_station, expected_visible) in test_cases {
            let input_filename = format!("../inputs/testinput10{}.txt", test_case_id);
            let asteroids = load_asteroids(load_input_file(input_filename));
            let (station, visible) = solve10a(&asteroids);
            assert_eq!(station, expected_station);
            assert_eq!(visible, expected_visible);
        }
    }

    #[test]
    fn asteroids_test_part2() {
        let asteroids = load_asteroids(load_input_file("../inputs/testinput10e.txt".to_string()));
        let (station, _) = solve10a(&asteroids);
        let asteroid = solve10b(station, &asteroids);
        assert_eq!(asteroid.unwrap(), (8, 2));
    }
}
