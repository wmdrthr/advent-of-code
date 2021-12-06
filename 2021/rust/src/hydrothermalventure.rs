use std::collections::HashMap;

type V2 = (i32, i32);

fn parse(line: &str) -> (V2, V2) {

    let parts: Vec<&str> = line.split(" -> ").collect();

    let start: Vec<&str> = parts[0].split(",").collect();
    let end: Vec<&str> = parts[1].split(",").collect();

    (
        (start[0].parse::<i32>().unwrap(),
         start[1].parse::<i32>().unwrap()),
        (end[0].parse::<i32>().unwrap(),
         end[1].parse::<i32>().unwrap())
    )
}

fn _print_grid(grid: HashMap<V2, u32>) {

    for row in 0..10 {
        for col in 0..10 {
            let val = grid.get(&(row,col));
            match val {
                Some(v) => {
                    if *v == 0 {
                        print!(".");
                    } else {
                        print!("{}", grid[&(row,col)]);
                    }
                },
                None => { print!(".") }
            };
        }
        println!();
    }
}


fn mark_points(points: &Vec<(V2, V2)>, skip_diagonals: bool) -> u32 {

    let mut grid: HashMap<V2, u32> = HashMap::new();

    for (x, y) in points {
        let (x1, y1) = x;
        let (x2, y2) = y;

        if skip_diagonals && !(x1 == x2 || y1 == y2) {
            continue;
        }

        let dx = x2 - x1;
        let dy = y2 - y1;
        let length = dx.abs().max(dy.abs());
        let step_x = dx / length;
        let step_y = dy / length;
        for i in 0..(length + 1) {
            let v = grid.entry((x1 + i * step_x, y1 + i * step_y)).or_insert(0);
            *v += 1;
        }
    }

    grid.values().filter(|&v| v > &1).collect::<Vec<&u32>>().len() as u32

}

// Day 5: Hydrothermal Venture
pub fn solve(data: String) {

    let points: Vec<(V2, V2)> = data.split("\n").map(|l| parse(l)).collect();

    println!("{}", mark_points(&points, true));
    println!("{}", mark_points(&points, false));

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


        let points = vec![((0, 9), (5, 9)),
                          ((8, 0), (0, 8)),
                          ((9, 4), (3, 4)),
                          ((2, 2), (2, 1)),
                          ((7, 0), (7, 4)),
                          ((6, 4), (2, 0)),
                          ((0, 9), (2, 9)),
                          ((3, 4), (1, 4)),
                          ((0, 0), (8, 8)),
                          ((5, 5), (8, 2))];

        assert_eq!(mark_points(&points, true), 5);
        assert_eq!(mark_points(&points, false), 12);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input05.txt");
        let points: Vec<(V2, V2)> = input.split("\n").map(|l| parse(l)).collect();

        assert_eq!(mark_points(&points, true), 5306);
        assert_eq!(mark_points(&points, false), 17787);
    }
}
