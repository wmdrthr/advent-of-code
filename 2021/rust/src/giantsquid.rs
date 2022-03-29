use std::collections::{HashMap, HashSet};
use itertools::izip;
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug)]
struct BingoBoard {

    index: u32,
    grid: Vec<Vec<u32>>,
    marked: Vec<Vec<bool>>,
    lookup: HashMap<u32, (usize, usize)>
}

impl BingoBoard {

    fn from_input(index: u32, data: String) -> BingoBoard {

        let lines: Vec<&str> = data.split("\n").collect();

        let mut grid: Vec<Vec<u32>> = Vec::new();
        let mut marked: Vec<Vec<bool>> = Vec::new();
        let mut lookup: HashMap<u32, (usize, usize)> = HashMap::new();

        for (row, line) in lines.iter().enumerate() {

            let numbers: Vec<u32> = line.split(" ")
                .filter(|n| n.len() > 0)
                .map(|n| n.parse::<u32>().unwrap() )
                .collect();

            grid.push(numbers.clone());

            let mut marked_row: Vec<bool> = Vec::new();

            for (col, num) in numbers.iter().enumerate() {
                lookup.insert(*num, (row, col));
                marked_row.push(false);
            }

            marked.push(marked_row);
        }

        BingoBoard { index, grid, marked, lookup }
    }

    fn call(&mut self, number: &u32) {

        match self.lookup.get(&number) {
            Some((row, col)) => { self.marked[*row][*col] = true; },
            None => {}
        }
    }

    fn winner(&self) -> bool {

        let rows = self.marked.iter().any(|row| row.iter().all(|v| *v));
        let columns =
            izip!(&self.marked[0], &self.marked[1], &self.marked[2], &self.marked[3], &self.marked[4])
            .any(|col| { *col.0 && *col.1 && *col.2 && *col.3 && *col.4 });

        rows || columns
    }

    fn score(&self, number: u32) -> u32 {

        let mut unmarked_sum = 0;

        for (row, col) in self.lookup.values() {

            if !self.marked[*row][*col] {
                unmarked_sum += self.grid[*row][*col];
            }
        }

        unmarked_sum * number
    }
}

impl Hash for BingoBoard {

    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl PartialEq for BingoBoard {

    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for BingoBoard {}

impl std::fmt::Display for BingoBoard {

    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut output: Vec<String> = Vec::new();
        for row in 0..5 {
            let mut line: String = String::new();
            for col in 0..5 {
                if self.marked[row][col] {
                    line.push_str(format!("{:>3}*", self.grid[row][col]).as_str());
                } else {
                    line.push_str(format!("{:>3} ", self.grid[row][col]).as_str());
                }
            }
            output.push(line);
        }
        output.push("\n".to_string());

        write!(f, "{}", output.join("\n"))
    }
}

fn parse(data: String) -> (Vec<u32>, Vec<BingoBoard>) {

    let lines:Vec<&str> = data.split("\n\n").collect();
    let numbers: Vec<u32> = lines[0].split(",").map(|n| n.parse::<u32>().unwrap()).collect();

    let boards =
        lines.iter()
        .skip(1)
        .enumerate()
        .map(|(i, l)| BingoBoard::from_input(i as u32, l.to_string())).collect();

    (numbers, boards)
}

fn solve1(numbers: &Vec<u32>, mut boards: Vec<BingoBoard>) -> u32 {

    let mut winning_score: u32 = 0;

    for number in numbers {
        if winning_score != 0 {
            break;
        }
        for board in &mut boards {
            board.call(&number);
            if board.winner() {
                winning_score = board.score(*number);
                break;
            }
        }
    }
    winning_score
}

fn solve2(numbers: &Vec<u32>, mut boards: Vec<BingoBoard>) -> u32 {

    let mut winning_boards: HashSet<u32> = HashSet::new();
    let mut winning_score: u32 = 0;
    let number_of_boards = boards.len();

    for number in numbers {
        if winning_score != 0 {
            break;
        }
        for board in &mut boards {
            board.call(&number);
            if board.winner() {
                winning_boards.insert(board.index);
                if winning_boards.len() == number_of_boards {
                    winning_score = board.score(*number);
                    break;
                }
            }
        }
    }

    winning_score
}

// Day 3: Giant Squid
pub fn solve(data: String) {

    let (numbers, boards) = parse(data);

    println!("{}", solve1(&numbers, boards.clone()));
    println!("{}", solve2(&numbers, boards));
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

        let input = load_input_file("../inputs/testinput04.txt");
        let (numbers, boards) = parse(input);

        assert_eq!(solve1(&numbers, boards.clone()), 4512);
        assert_eq!(solve2(&numbers, boards), 1924);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input04.txt");
        let (numbers, boards) = parse(input);

        assert_eq!(solve1(&numbers, boards.clone()), 50008);
        assert_eq!(solve2(&numbers, boards), 17408);
    }
}
