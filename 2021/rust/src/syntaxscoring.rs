fn median(array: Vec<&i64>) -> i64 {

    if (array.len() % 2) == 0 {
        let mid = array.len() / 2;
        (array[mid - 1] + array[mid]) / 2
    } else {
        *array[(array.len() / 2)]
    }
}

fn check(line: &str) -> i64 {

    let mut stack: Vec<char> = Vec::new();

    for ch in line.chars() {
        if ch == '(' || ch == '{' || ch == '[' || ch == '<' {

            stack.push(match ch {
                '{' => '}',
                '[' => ']',
                '(' => ')',
                '<' => '>',
                _   => { panic!("invalid data1"); }
            });

        } else if ch != stack.pop().unwrap() {

            return (-1) * match ch {
                ')' => 3,
                ']' => 57,
                '}' => 1197,
                '>' => 25137,
                _   => { panic!("invalid data1"); }
            };

        }
    }

    stack.reverse();

    let mut score = 0;
    for ch in stack {
        score = (score * 5) + match ch {
            ')' => 1,
            ']' => 2,
            '}' => 3,
            '>' => 4,
            _   => { panic!("invalid data1"); }
        };
    }

    score
}

fn calculate(lines: Vec<&str>) -> (i64, i64) {


    let scores: Vec<i64> = lines.iter().map(|l| check(l)).collect();

    let part1 = scores.iter().filter(|v| *v < &0).sum::<i64>().abs();

    let mut incomplete_scores: Vec<&i64> = scores.iter().filter(|v| *v > &0).collect();
    incomplete_scores.sort_unstable();

    let part2 = median(incomplete_scores);

    (part1, part2)
}

// Day 10: Syntax Scoring
pub fn solve(data: String) {

    let lines: Vec<&str> = data.split('\n').collect();

    let (part1, part2) = calculate(lines);
    println!("{}\n{}", part1, part2);
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
        let test_input = vec!["[({(<(())[]>[[{[]{<()<>>",
                              "[(()[<>])]({[<{<<[]>>(",
                              "{([(<{}[<>[]}>{[]{[(<()>",
                              "(((({<>}<{<{<>}{[]{[]{}",
                              "[[<[([]))<([[{}[[()]]]",
                              "[{[{({}]{}}([{[{{{}}([]",
                              "{<[[]]>}<{[{[{[]{()[[[]",
                              "[<(<(<(<{}))><([]([]()",
                              "<{([([[(<>()){}]>(<<{{",
                              "<{([{{}}[<[[[<>{}]]]>[]]"];


        let (part1, part2) = calculate(test_input);
        assert_eq!(part1, 26397);
        assert_eq!(part2, 288957);
    }

    #[test]
    fn solution() {

        let input = load_input_file("../inputs/input10.txt");
        let lines: Vec<&str> = input.split('\n').collect();

        let (part1, part2) = calculate(lines);
        assert_eq!(part1, 369105);
        assert_eq!(part2, 3999363569);
    }
}
