use std::fs;

use aoc::days;

fn main() {
    let input = fs::read_to_string("../inputs/10.txt").expect("input not available");
    let answer = days::ten::part_two(&input);
    println!("{}", answer);
}
