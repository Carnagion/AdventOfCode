mod twelve;

pub fn solve(day: u8, part: u8, input: String) -> String {
    match day {
        12 => twelve::solve(part, input),
        _ => String::from("Invalid day"),
    }
}