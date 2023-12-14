#![feature(iter_intersperse)]
#![feature(slice_group_by)]

use std::fs;

pub mod days;

fn main() {
    let input = fs::read_to_string("../../inputs/14.txt").expect("input not available");
    let answer = days::fourteen::part_two(&input);
    println!("{}", answer);
}
