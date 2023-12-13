#![feature(iter_intersperse)]
#![feature(slice_group_by)]

use std::fs;

pub mod days;

fn main() {
    let input = fs::read_to_string("../../inputs/13.txt").expect("input not available");
    let answer = days::thirteen::part_two(&input);
    println!("{}", answer);
}
