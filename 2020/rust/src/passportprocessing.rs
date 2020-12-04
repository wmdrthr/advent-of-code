use regex::Regex;
use std::collections::HashMap;

fn has_required_fields(passport: &HashMap<&str, &str>) -> bool {

    for key in vec!["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] {
        if !passport.contains_key(&key) {
            return false;
        }
    }

    true
}

fn valid_passport(passport: &HashMap<&str, &str>) -> bool {

    if !has_required_fields(passport) {
        return false;
    }

    let eye_colors = vec!["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];

    lazy_static! {
        static ref HCL_REGEX: Regex = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
        static ref PID_REGEX: Regex = Regex::new(r"^\d{9}$").unwrap();
    }

    for key in vec!["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] {

        if key == "byr" {
            let val: u32 = passport.get(&key).unwrap().parse::<u32>().unwrap();
            if !(val >= 1920 && val <= 2002) {
                return false;
            }
        } else if key == "iyr" {
            let val: u32 = passport.get(&key).unwrap().parse::<u32>().unwrap();
            if !(val >= 2010 && val <= 2020) {
                return false;
            }
        } else if key == "eyr" {
            let val: u32 = passport.get(&key).unwrap().parse::<u32>().unwrap();
            if !(val >= 2020 && val <= 2030) {
                return false;
            }
        } else if key == "hgt" {
            let val = passport.get(&key).unwrap();
            let unit = val.get(val.len()-2..).unwrap();
            let height: u32 = val.get(0..val.len()-2).unwrap().parse::<u32>().unwrap();
            if unit == "cm" {
                if !(height >= 150 && height <= 193) {
                    return false;
                }
            } else if unit == "in" {
                if !(height >= 59 && height <= 76) {
                    return false;
                }
            } else {
                return false;
            }
        } else if key == "hcl" {
            if !HCL_REGEX.is_match(passport.get(&key).unwrap()) {
                return false;
            }
        } else if key == "ecl" {
            if !eye_colors.contains(passport.get(&key).unwrap()) {
                return false;
            }
        } else if key == "pid" {
            if !PID_REGEX.is_match(passport.get(&key).unwrap()) {
                return false;
            }
        }
    }

    true
}

fn parse_passport(passport_data: &str) -> HashMap<&str, &str> {

    let mut passport: HashMap<&str, &str> = HashMap::new();
    let fields: Vec<&str> = passport_data.split(|c| c == '\n' || c == ' ').collect();
    for field in fields {
        let pair: Vec<&str> = field.split(':').collect();
        passport.insert(pair.get(0).unwrap(), pair.get(1).unwrap());
    }

    passport
}

fn calculate(passports: Vec<HashMap<&str, &str>>) -> (u32, u32) {

    let valid1: u32 = passports.iter()
        .filter(|p| has_required_fields(p))
        .count() as u32;

    let valid2: u32 = passports.iter()
        .filter(|p| valid_passport(p))
        .count() as u32;

    (valid1, valid2)
}

// Day 4: Passport Processing
pub fn solve(data: String) {

    let passports: Vec<HashMap<&str, &str>> = data.split("\n\n")
        .map(|pd| parse_passport(pd))
        .collect();

    let (valid1, valid2) = calculate(passports);

    println!("Part 1: {}\nPart 2: {}", valid1, valid2);
}


#[cfg(test)]
mod tests {

    use super::*;
    use std::fs;

    fn load_input_file(filename: &str) -> String {
        fs::read_to_string(filename).unwrap().trim().to_string()
    }

    #[test]
    fn test_has_required_fields() {

        assert_eq!(
            has_required_fields(
                &parse_passport("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm")),
            true);
        assert_eq!(
            has_required_fields(
                &parse_passport("iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929")),
            false);
        assert_eq!(
            has_required_fields(
                &parse_passport("hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm")),
            true);
        assert_eq!(
            has_required_fields(
                &parse_passport("hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in")),
            false);
    }

    #[test]
    fn test_valid_passport() {

        assert_eq!(
            valid_passport(
                &parse_passport("eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926")),
            false);

        assert_eq!(
            valid_passport(
                &parse_passport("iyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946")),
            false);

        assert_eq!(
            valid_passport(
                &parse_passport("hcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277")),
            false);

        assert_eq!(
            valid_passport(
                &parse_passport("hgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007")),
            false);

        assert_eq!(
            valid_passport(
                &parse_passport("pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f")),
            true);

        assert_eq!(
            valid_passport(
                &parse_passport("eyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm")),
            true);

        assert_eq!(
            valid_passport(
                &parse_passport("hcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022")),
            true);

        assert_eq!(
            valid_passport(
                &parse_passport("iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")),
            true);
    }

    #[test]
    fn solution() {
        let input = load_input_file("../inputs/input04.txt");

        let passports: Vec<HashMap<&str, &str>> = input.split("\n\n")
            .map(|pd| parse_passport(pd))
            .collect();

        assert_eq!(calculate(passports), (202, 137));
    }

}
