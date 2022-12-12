use std::env;
use std::fs;

use aoc;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let output = match (args[1].parse(), args[2].parse(), fs::read_to_string(&args[3])) {
        (Ok(day), Ok(part), Ok(input)) => aoc::solve(day, part, input),
        (day_res, part_res, file_res) => format!("{:?}\n{:?}\n{:?}", day_res, part_res, file_res),
    };
    println!("{}", output);
}