use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug)]
struct Claim {
    id: u32,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
}

fn parse_claim(pattern: &Regex, s: &str) -> Claim {
    let v: Vec<u32> = pattern
        .find_iter(s)
        .into_iter()
        .map(|m| m.as_str().parse::<u32>().unwrap())
        .collect();

    let claim = Claim {
        id: v[0],
        x: v[1],
        y: v[2],
        width: v[3],
        height: v[4],
    };

    claim
}

fn resolve_claim(claim: &Claim) -> Vec<(u32, u32)> {
    let mut v: Vec<(u32, u32)> = vec![];

    for i in claim.x..claim.x + claim.width {
        for j in claim.y..claim.y + claim.height {
            v.push((i, j));
        }
    }
    v
}

pub fn solve(data: String) {
    let pattern: Regex = Regex::new(r"\d+").unwrap();

    let mut fabric: HashMap<(u32, u32), Vec<u32>> = HashMap::new();
    let mut overlaps: HashMap<u32, HashSet<u32>> = HashMap::new();

    for line in data.split('\n') {
        if line.len() == 0 {
            continue;
        }
        let claim = parse_claim(&pattern, &line);

        overlaps.insert(claim.id, HashSet::new());

        for point in resolve_claim(&claim) {
            if fabric.contains_key(&point) {
                let otherclaims = fabric.get(&point).unwrap();
                for otherclaim in otherclaims.into_iter() {
                    overlaps.get_mut(otherclaim).unwrap().insert(claim.id);
                    overlaps.get_mut(&claim.id).unwrap().insert(*otherclaim);
                }
            }
            let v = fabric.entry(point).or_insert(Vec::new());
            v.push(claim.id);
        }
    }

    let mut part1: u32 = 0;
    for (_key, val) in fabric {
        if val.len() > 1 {
            part1 += 1;
        }
    }

    println!("{}", part1);

    for (key, val) in overlaps {
        if val.len() == 0 {
            println!("{}", key);
            break;
        }
    }
}
