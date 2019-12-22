use std::collections::HashSet;


fn parse_input(data: String) -> Vec<i64> {
    let mut v: Vec<i64> = vec![];
    let split = data.split('\n');
    for line in split {
        let n = match line.parse() {
            Ok(n) => n,
            Err(_) => { continue; }
        };
        v.push(n);
    }
    v
}

fn solve1(changes: &[i64]) -> i64 {
    changes.iter().sum()
}

fn solve2(changes: &[i64]) -> i64 {
    let mut mem = HashSet::new();

    changes
        .iter()
        .cycle()
        .scan(0, |acc, &x| {
            *acc = *acc + x;
            Some(*acc)
        })
        .find(|&x| { !mem.insert(x) })
        .unwrap()
}

pub fn solve(data: String) {
    let changes = parse_input(data);
    println!("Part 1: {}", solve1(&changes));
    println!("Part 2: {}", solve2(&changes));
}
