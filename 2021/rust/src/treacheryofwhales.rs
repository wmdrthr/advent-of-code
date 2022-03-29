

fn median(array: &Vec<i32>) -> i32 {

    if (array.len() % 2) == 0 {
        let mid = array.len() / 2;
        (array[mid - 1] + array[mid]) / 2
    } else {
        array[(array.len() / 2)]
    }
}

fn mean(array: &Vec<i32>) -> i32 {

    array.iter().sum::<i32>() / (array.len() as i32)
}

fn solve1(positions: &Vec<i32>) -> i32 {

    let m = median(positions);

    positions.iter().map(|p| (p - m).abs()).sum::<i32>()
}

fn fuel_cost(moves: i32) -> i32 {

    moves * (moves + 1) / 2
}

fn solve2(positions: &Vec<i32>) -> i32 {

    let m = mean(positions);

    let values = vec![m - 1, m, m + 1];
    values.iter().map(|v| { positions.iter().map(|p| fuel_cost((p - v).abs())).sum::<i32>()}).min().unwrap()
}

// ay 7: The Treachery of Whales
pub fn solve(data: String) {

    let mut positions: Vec<i32> =
        data.split(",")
        .map(|n| n.parse::<i32>().unwrap())
        .collect();

    positions.sort_unstable();

    println!("{}", solve1(&positions));
    println!("{}", solve2(&positions));

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

        let mut positions = vec![16,1,2,0,4,2,7,1,2,14];

        positions.sort_unstable();

        assert_eq!(solve1(&positions), 37);
        assert_eq!(solve2(&positions), 168);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input07.txt");
        let mut positions: Vec<i32> =
            input.split(",")
            .map(|n| n.parse::<i32>().unwrap())
            .collect();

        positions.sort_unstable();

        assert_eq!(solve1(&positions), 349812);
        assert_eq!(solve2(&positions), 99763899);
    }
}
