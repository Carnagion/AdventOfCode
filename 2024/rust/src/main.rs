use std::fs;

use aoc::days;

fn main() {
    let input = fs::read_to_string("../inputs/7.txt").expect("input not available");
    let answer = days::seven::part_two(&input);
    println!("{}", answer);
}
