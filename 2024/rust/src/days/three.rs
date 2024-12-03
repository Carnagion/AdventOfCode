use regex::Regex;

pub fn part_one(input: &str) -> u64 {
    let regex = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();
    regex
        .captures_iter(input)
        .map(|cap| {
            let l = cap[1].parse::<u64>().unwrap();
            let r = cap[2].parse::<u64>().unwrap();
            l * r
        })
        .sum()
}

pub fn part_two(input: &str) -> u64 {
    let regex = Regex::new(r"(?<mul>mul\((\d+),(\d+)\))|(?<do>do\(\))|(?<dont>don't\(\))").unwrap();
    let mut enabled = true;
    regex
        .captures_iter(input)
        .filter_map(|cap| {
            if cap.name("do").is_some() {
                enabled = true;
            } else if cap.name("dont").is_some() {
                enabled = false;
            }

            if enabled && cap.name("mul").is_some() {
                let l = cap[2].parse::<u64>().unwrap();
                let r = cap[3].parse::<u64>().unwrap();
                Some(l * r)
            } else {
                None
            }
        })
        .sum()
}
