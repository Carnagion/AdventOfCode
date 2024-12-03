use std::fs;

use aoc::days;

fn main() {
    let input = fs::read_to_string("../inputs/3.txt").expect("input not available");
    let answer = days::three::part_one(&input);
    println!("{}", answer);
}
