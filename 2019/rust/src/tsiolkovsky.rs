fn calculate(modules: Vec<f32>) -> (u32, u32) {

    let mut total_basic_fuel: u32 = 0;
    let mut total_fuel: u32 = 0;

    for module in modules {
        let mut fuel = ((module / 3.0).floor() as u32) - 2;
        total_basic_fuel += fuel;
        total_fuel += fuel;
        loop {
            let extra_fuel = ((fuel as f32 / 3.0).floor() as i32) - 2;
            if extra_fuel <= 0 {
                break;
            }
            total_fuel += extra_fuel as u32;
            fuel = extra_fuel as u32;
        }
    }

    (total_basic_fuel, total_fuel)
}

pub fn solve(data: String) {

    let modules: Vec<f32> = data.split("\n")
        .map(|l| l.parse::<f32>().unwrap())
        .collect();

    let (total_basic_fuel, total_fuel) = calculate(modules);


    println!("Part 1: {}\nPart 2: {}", total_basic_fuel, total_fuel);
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn tsiolkovsky_12() {
        assert_eq!(calculate(vec![12.0]), (2, 2));
    }

    #[test]
    fn tsiolkovsky_14() {
        assert_eq!(calculate(vec![14.0]), (2, 2));
    }

    #[test]
    fn tsiolkovsky_1969() {
        assert_eq!(calculate(vec![1969.0]), (654, 966));
    }

    #[test]
    fn tsiolkovsky_100756() {
        assert_eq!(calculate(vec![100756.0]), (33583, 50346));
    }
}
