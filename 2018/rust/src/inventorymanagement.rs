use std::collections::HashMap;


fn cardinalities(s: &String) -> HashMap<char, u8> {

    let mut counts = HashMap::new();

    for c in s.chars() {
        let count = counts.entry(c).or_insert(0);
        *count += 1;
    }

    counts
}

fn solve1(boxids: &[String]) -> u32 {

    let mut twocount: u32 = 0;
    let mut threecount: u32 = 0;

    for counts in boxids.into_iter()
        .filter(|l| !l.is_empty())
        .map(cardinalities)
    {

        let mut two: bool = false;
        let mut three: bool = false;

        for (_, value) in counts {
            if !two && value == 2 {
                twocount += 1;
                two = true;
            }
            if !three && value == 3 {
                threecount += 1;
                three = true;
            }
            if two && three {
                break;
            }
        }
    }

    twocount * threecount
}

fn hamming_one(x: &String, y: &String) -> bool {

    let mut difference: u8 = 0;

    for pair in x.chars().zip(y.chars()) {
        if pair.0 != pair.1 {
            difference += 1;
            if difference > 1 {
                return false
            }
        }
    }
    difference == 1
}

fn solve2(boxids: &[String]) -> String {

    for (bx, by) in iproduct!(boxids, boxids.clone())
        .filter(|(bx, by)| !bx.is_empty() && !by.is_empty() && bx != by) {
            if hamming_one(bx, by) {
                let mut result: String = String::with_capacity(bx.len());
                for pair in bx.chars().zip(by.chars()) {
                    if pair.0 == pair.1 {
                        result.push(pair.0);
                    }
                }
                return result;
            }
        }
    panic!("no solution found!");
}

pub fn solve(data: &String) {

    let mut boxids : Vec<String> = vec![];

    for line in data.split('\n') {
        boxids.push(line.to_string());
    }

    println!("Part 1: {}", solve1(&boxids));
    println!("Part 2: {}", solve2(&boxids));
}
