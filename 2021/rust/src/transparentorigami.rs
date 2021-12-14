use std::collections::HashSet;

type V2 = (i32, i32);

#[derive(PartialEq, Debug)]
enum Fold {
    Horizontal(i32), // up
    Vertical(i32)    // left
}

impl Fold {

    fn from(s: &str) -> Fold {

        let words: Vec<&str> = s.split(' ').collect();
        let fold: Vec<&str> = words[2].split('=').collect();

        let magnitude = fold[1].parse::<i32>().unwrap();

        match fold[0] {
            "x" => Fold::Vertical(magnitude),
            "y" => Fold::Horizontal(magnitude),
            _ => { panic!("invalid input"); }
        }
    }

}

fn fold_points(points: &HashSet<V2>, fold: &Fold) -> HashSet<V2> {

    let mut folded_points: HashSet<V2> = HashSet::new();

    match fold {
        Fold::Horizontal(v) => {
            for point in points {
                if point.0 < *v {
                    folded_points.insert(*point);
                } else {
                    let (x, y) = point;
                    folded_points.insert((2 * v - *x, *y));
                }
            }
        },
        Fold::Vertical(v) => {
            for point in points {
                if point.1 < *v {
                    folded_points.insert(*point);
                } else {
                    let (x, y) = point;
                    folded_points.insert((*x, 2 * v - *y));
                }
            }
        }
    };

    folded_points
}


fn display(points: HashSet<V2>, pretty: bool) -> String {

    let mut maxx: i32 = 0;
    let mut maxy: i32 = 0;

    let display_chars: Vec<char> =
        if pretty {
            vec!['█', '░']
        } else {
            vec!['1', '0']
        };

    for point in &points {
        maxx = maxx.max(point.0);
        maxy = maxy.max(point.1);
    }

    let mut output: String = String::new();

    for x in 0..maxx+1 {
        for y in 0..maxy+1 {
            if points.contains(&(x, y)) {
                output.push(display_chars[0]);
            } else {
                output.push(display_chars[1]);
            }
        }
        output.push('\n')
    }

    output.trim_end().to_string()
}

fn parse(data: String) -> (HashSet<V2>, Vec<Fold>) {

    let mut points: HashSet<V2> = HashSet::new();
    let mut folds: Vec<Fold> = Vec::new();

    let parts: Vec<&str> = data.split("\n\n").collect();

    for line in parts[0].split("\n") {
        let point_data: Vec<i32> = line.split(",").map(|v| v.parse::<i32>().unwrap()).collect();
        points.insert((point_data[1], point_data[0]));
    }

    for line in parts[1].split("\n") {
        folds.push(Fold::from(&line));
    }

    (points, folds)
}

fn calculate(mut points: HashSet<V2>, folds: Vec<Fold>) -> (usize, HashSet<V2>) {

    let mut single_fold_count = 0;

    for (idx, fold) in folds.iter().enumerate() {

        let folded = fold_points(&points, &fold);

        if idx == 0 {
            single_fold_count = folded.len();
        }

        points.clear();
        points.extend(folded.iter());
    }

    (single_fold_count, points)
}

// Day 13: Transparent Origami
pub fn solve(data: String) {

    let (points, folds) = parse(data);

    let (single_fold_count, folded_points) = calculate(points, folds);
    println!("{}", single_fold_count);
    println!("{}", display(folded_points, true));
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

        let test_input = load_input_file("../inputs/testinput13.txt");
        let (points, folds) = parse(test_input);

        let (single_fold_count, folded_points) = calculate(points, folds);
        assert_eq!(single_fold_count, 17);
        assert_eq!(folded_points.len(), 16);
        assert_eq!(display(folded_points, false), "11111\n10001\n10001\n10001\n11111");
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input13.txt");
        let (points, folds) = parse(input);

        let (single_fold_count, folded_points) = calculate(points, folds);
        let output = display(folded_points, false);

        assert_eq!(single_fold_count, 621);
        assert_eq!(folded_points.len(), 95);
        assert_eq!(output.split('\n').collect::<Vec<&str>>(),
                   ["100101001010010001100110001100001101111",
                    "100101010010010000101001010010000100001",
                    "111101100010010000101000010010000100010",
                    "100101010010010000101011011110000100100",
                    "100101010010010100101001010010100101000",
                    "100101001001100011000111010010011001111"]);
    }
}
