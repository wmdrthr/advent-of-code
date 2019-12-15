use std::collections::HashMap;
use std::collections::HashSet;

struct OrbitalMap {
    orbital_map: HashMap<String, String>,
    satellites: HashMap<String, HashSet<String>>,
}

impl OrbitalMap {

    fn from(orbits: String) -> OrbitalMap {

        let satellites: HashMap<String, HashSet<String>> = HashMap::new();
        let mut orbital_map: HashMap<String, String> = HashMap::new();

        for orbit in orbits.split("\n")
            .map(|l| l.split(")").collect::<Vec<&str>>())
            .filter(|v| v.len() > 1) {
                orbital_map.insert(orbit[1].to_string(), orbit[0].to_string());
            }

        OrbitalMap { orbital_map, satellites }
    }

    fn count_orbits(&self, object: &String) -> u32 {
        match self.orbital_map.get(object) {
            None => 0,
            Some(primary) => 1 + self.count_orbits(primary)
        }
    }

    fn build_transfer_map(&mut self) {
        for object in self.orbital_map.keys() {
            let mut primary = self.orbital_map.get(object);
            loop {
                match primary {
                    None => break,
                    Some(p) => {
                        self.satellites.entry(p.to_string()).or_insert(HashSet::new()).insert(object.to_string());
                        primary = self.orbital_map.get(p)
                    }
                }
            }
        }
    }

    fn count_transfers(&self, current: &str, target: &str) -> u32 {
        let mut transfers = 0;
        let mut object = self.orbital_map.get(&current.to_string()).unwrap();
        loop {
            object = self.orbital_map.get(object).unwrap();
            transfers += 1;
            if self.satellites.get(object).unwrap().contains(&target.to_string()) {
                break;
            }
        }

        transfers
    }
}

// Day 6: Universal Orbit Map

pub fn solve(data: String) {

    let mut om = OrbitalMap::from(data);

    let total_orbits: u32 = om.orbital_map.keys().map(|o| om.count_orbits(o)).sum();
    println!("Part 1: {}", total_orbits);

    om.build_transfer_map();
    let transfers = om.count_transfers("YOU", "SAN") + om.count_transfers("SAN", "YOU");
    println!("Part 2: {}", transfers);
}


#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn orbitalmap_part_a() {

        let data = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L".to_string();
        let om = OrbitalMap::from(data);
        let orbits: u32 = om.orbital_map.keys().map(|o| om.count_orbits(o)).sum();
        assert_eq!(orbits, 42);
    }

    #[test]
    fn orbital_map_b() {
        let data = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN".to_string();
        let mut om = OrbitalMap::from(data);

        om.build_transfer_map();
        assert_eq!(om.count_transfers("YOU", "SAN") + om.count_transfers("SAN", "YOU"), 4);
    }
}
