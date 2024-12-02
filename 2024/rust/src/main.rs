use std::fs;

use aoc::days;

fn main() {
    let input = fs::read_to_string("../inputs/2.txt").expect("input not available");
    let answer = days::two::part_two(&input);
    println!("{}", answer);
}
