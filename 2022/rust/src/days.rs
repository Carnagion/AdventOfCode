mod twelve;
mod sixteen;
mod seventeen;
mod nineteen;

pub fn solve(day: u8, part: u8, input: String) -> String {
    match day {
        12 => twelve::solve(part, input),
        16 => sixteen::solve(part, input),
        17 => seventeen::solve(part, input),
        19 => nineteen::solve(part, input),
        _ => String::from("Invalid day"),
    }
}