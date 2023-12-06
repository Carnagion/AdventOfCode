use std::fs;

pub mod days;

fn main() {
    let input = fs::read_to_string("../../inputs/6.txt").expect("input not available");
    let answer = days::six::part_two(&input);
    println!("{}", answer);
}
