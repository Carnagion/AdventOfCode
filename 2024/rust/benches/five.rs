use std::fs;

use divan::Bencher;

use aoc::days;

fn main() {
    divan::main();
}

#[divan::bench]
fn part_one(bencher: Bencher) {
    let input = fs::read_to_string("../inputs/5.txt").expect("input not available");
    bencher.bench(|| days::five::part_one(&input));
}

#[divan::bench]
fn part_two(bencher: Bencher) {
    let input = fs::read_to_string("../inputs/5.txt").expect("input not available");
    bencher.bench(|| days::five::part_two(&input));
}
