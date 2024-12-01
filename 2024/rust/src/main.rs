#![feature(cmp_minmax)]

use std::fs;

mod days;

fn main() {
    let input = fs::read_to_string("../inputs/1.txt").expect("input not available");
    let answer = days::one::part_two(&input);
    println!("{}", answer);
}
