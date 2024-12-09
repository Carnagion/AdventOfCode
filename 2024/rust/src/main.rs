use std::fs;

use aoc::days;

fn main() {
    let input = fs::read_to_string("../inputs/8.txt").expect("input not available");
    let answer = days::eight::part_two(&input);
    println!("{}", answer);
}
