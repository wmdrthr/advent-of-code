
fn translate(c: char) -> char {
    match c {
        'F' => '0',
        'B' => '1',
        'L' => '0',
        'R' => '1',
         _  => '2'
    }
}

fn get_seat_id(pass: &str) -> u32 {

    let mut seat_id = String::new();
    for ch in pass.chars().map(|c| translate(c)) {
        seat_id.push(ch);
    }

    u32::from_str_radix(&seat_id, 2).unwrap()
}

fn calculate(passes: Vec<&str>) -> (u32, u32) {

    let mut seat_ids: Vec<u32> = passes.iter().map(|p| get_seat_id(p)).collect();
    seat_ids.sort();

    for (idx, seat_id) in seat_ids.iter().enumerate() {
        if seat_ids[idx+1] != (seat_id + 1) {
            return (*seat_ids.last().unwrap(), seat_id + 1);
        }
    }

    (0, 0)
}

// Day 5: Binary Boarding
pub fn solve(data: String) {

    let boarding_passes = data.split('\n').collect();

    let (part1, part2) = calculate(boarding_passes);
    println!("Part 1: {}\nPart 2: {}", part1, part2);
}


#[cfg(test)]
mod tests {

    use super::*;
    use std::fs;

    fn load_input_file(filename: &str) -> String {
        fs::read_to_string(filename).unwrap().trim().to_string()
    }

    #[test]
    fn seat_id_test() {
        assert_eq!(get_seat_id("FBFBBFFRLR"), 357);
        assert_eq!(get_seat_id("BFFFBBFRRR"), 567);
        assert_eq!(get_seat_id("FFFBBBFRRR"), 119);
        assert_eq!(get_seat_id("BBFFBBFRLL"), 820);
    }

    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input05.txt");

        let passes = input.split('\n').collect();
        assert_eq!(calculate(passes), (933, 711));
    }
}
