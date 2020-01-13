use std::fmt;
use regex::Regex;
use itertools::Itertools;
use std::sync::mpsc;
use std::thread;

#[derive(Copy, Clone)]
struct V3 {
    x: i64,
    y: i64,
    z: i64
}

impl fmt::Debug for V3 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{},{},{}>", self.x, self.y, self.z)
    }
}

impl V3 {

    fn zero() -> V3 {
        V3 { x:0, y:0, z:0 }
    }

    fn new(x: i64, y: i64, z: i64) -> V3 {
        V3 { x, y, z }
    }

    fn get(&self, dim: u8) -> i64 {
        match dim {
            0 => self.x,
            1 => self.y,
            2 => self.z,
            _ => panic!("invalid dimension")
        }
    }

    fn set(&mut self, dim: u8, val: i64) {
        match dim {
            0 => { self.x = val; }
            1 => { self.y = val; }
            2 => { self.z = val; }
            _ => panic!("invalid dimension")
        }
    }

    fn inc(&mut self, dim: u8) {
        self.set(dim, self.get(dim) + 1);
    }

    fn dec(&mut self, dim: u8) {
        self.set(dim, self.get(dim) - 1);
    }

    fn value(&self) -> i64 {
        return self.x.abs() + self.y.abs() + self.z.abs()
    }

    fn iadd(&mut self, other: V3) {
        self.x += other.x;
        self.y += other.y;
        self.z += other.z;
    }
}

#[derive(Copy, Clone)]
struct Moon {
    position: V3,
    velocity: V3
}

impl fmt::Debug for Moon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(P{:?},V{:?})", self.position, self.velocity)
    }
}

impl Moon {
    fn new(position: V3, velocity: V3) -> Moon {
        Moon { position, velocity }
    }

    fn time_step(&mut self) {
        self.position.iadd(self.velocity);
    }

    fn energy(&self) -> i64 {
        self.position.value() * self.velocity.value()
    }
}

fn gravity(ma: &mut Moon, mb: &mut Moon, dim: u8) {

    if ma.position.get(dim) < mb.position.get(dim) {
        ma.velocity.inc(dim);
        mb.velocity.dec(dim);
    } else if ma.position.get(dim) > mb.position.get(dim) {
        ma.velocity.dec(dim);
        mb.velocity.inc(dim);
    }
}

fn gcd(a: i64, b: i64) -> i64 {

    let (mut a, mut b) = if a > b {
        (a, b)
    } else {
        (b, a)
    };

    while b != 0 {
        let r = a % b;
        a = b;
        b = r;
    }

    a
}

fn lcm(a: i64, b: i64) -> i64 {
    (a * b) / gcd(a, b)
}

fn solve12a(mut moons: Vec<Moon>, reps: u8) -> i64 {

    let number_of_moons = moons.len();

    for _ in 0..reps {
        for pair in (0..number_of_moons).combinations(2) {
            for dim in 0..3 {
                let mut ma = moons[pair[0]];
                let mut mb = moons[pair[1]];
                gravity(&mut ma, &mut mb, dim);
                moons[pair[0]] = ma;
                moons[pair[1]] = mb;
            }
        }
        for moon in &mut moons {
            moon.time_step()
        }
    }

    let mut total_energy: i64 = 0;
    for moon in moons {
        total_energy += moon.energy();
    }

    total_energy
}

fn solve12b(moons: Vec<Moon>) -> i64 {

    let (tx, rx) = mpsc::channel();

    for dim in 0..3 {

        let mut moons = moons.clone();
        let number_of_moons = moons.len();
        let tx = tx.clone();

        thread::spawn(move || {
            let mut steps = 0;

            loop {
                for pair in (0..number_of_moons).combinations(2) {
                    let mut ma = moons[pair[0]];
                    let mut mb = moons[pair[1]];
                    gravity(&mut ma, &mut mb, dim);
                    moons[pair[0]] = ma;
                    moons[pair[1]] = mb;
                }
                for moon in &mut moons {
                    moon.time_step()
                }

                steps += 1;

                let mut total_velocity = 0;
                for moon in &moons {
                    total_velocity += moon.velocity.value();
                }

                if total_velocity == 0 {
                    steps *= 2;
                    break;
                }
            }

            tx.send(steps).unwrap();
        });
    }

    let a = rx.recv().unwrap();
    let b = rx.recv().unwrap();
    let c = rx.recv().unwrap();

    lcm(lcm(a, b), c)
}

// Day 12: The N-Body Problem

pub fn solve(data: String) {

    let pattern = Regex::new(r"^<x=([-\d]+), y=([-\d]+), z=([-\d]+)>$").unwrap();

    let mut moons: Vec<Moon> = Vec::new();
    for line in data.split("\n") {
        if pattern.is_match(line) {
            let captures = pattern.captures(line).unwrap();
            let x = captures.get(1).unwrap().as_str().parse::<i64>().unwrap();
            let y = captures.get(2).unwrap().as_str().parse::<i64>().unwrap();
            let z = captures.get(3).unwrap().as_str().parse::<i64>().unwrap();
            moons.push(Moon::new(V3::new(x, y, z), V3::zero()));
        }
    }

    println!("Part 1: {}", solve12a(moons.clone(), 10));
    println!("Part 2: {}", solve12b(moons));
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn nbodyproblem_energy_test1() {
        let system = vec![Moon::new(V3::new(-1, 0, 2), V3::zero()),
                          Moon::new(V3::new(2, -10, -7), V3::zero()),
                          Moon::new(V3::new(4, -8, 8), V3::zero()),
                          Moon::new(V3::new(3, 5, -1), V3::zero())];
        assert_eq!(solve12a(system, 10), 179);
    }

    #[test]
    fn nbodyproblem_energy_test2() {
        let system = vec![Moon::new(V3::new(-8, -10, 0), V3::zero()),
                          Moon::new(V3::new(5, 5, 10), V3::zero()),
                          Moon::new(V3::new(2, -7, 3), V3::zero()),
                          Moon::new(V3::new(9, -8, -3), V3::zero())];
        assert_eq!(solve12a(system, 100), 1940);
    }

    #[test]
    fn nbodyproblem_repetition_test1() {
        let system = vec![Moon::new(V3::new(-1, 0, 2), V3::zero()),
                          Moon::new(V3::new(2, -10, -7), V3::zero()),
                          Moon::new(V3::new(4, -8, 8), V3::zero()),
                          Moon::new(V3::new(3, 5, -1), V3::zero())];
        assert_eq!(solve12b(system), 2772);
    }

    #[test]
    fn nbodyproblem_repetition_test2() {
        let system = vec![Moon::new(V3::new(-8, -10, 0), V3::zero()),
                          Moon::new(V3::new(5, 5, 10), V3::zero()),
                          Moon::new(V3::new(2, -7, 3), V3::zero()),
                          Moon::new(V3::new(9, -8, -3), V3::zero())];
        assert_eq!(solve12b(system), 4686774924);
    }
}
