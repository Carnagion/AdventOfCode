#![feature(iter_intersperse)]
#![feature(slice_group_by)]
#![feature(iter_next_chunk)]

use std::fs;

pub mod days;

fn main() {
    let input = fs::read_to_string("../../inputs/22.txt").expect("input not available");
    let answer = days::twenty_two::part_two(&input);
    println!("{}", answer);
}
