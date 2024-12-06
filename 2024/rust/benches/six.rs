use std::fs;

use divan::Bencher;

use aoc::days;

fn main() {
    divan::main();
}

#[divan::bench]
fn part_one(bencher: Bencher) {
    let input = fs::read_to_string("../inputs/6.txt").expect("input not available");
    bencher.bench(|| days::six::part_one(&input));
}

#[divan::bench(sample_count = 1, sample_size = 10)]
fn part_two(bencher: Bencher) {
    let input = fs::read_to_string("../inputs/6.txt").expect("input not available");
    bencher.bench(|| days::six::part_two(&input));
}
