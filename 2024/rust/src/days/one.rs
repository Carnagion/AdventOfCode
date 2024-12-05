use std::{cmp, collections::HashMap, iter};

pub fn part_one(input: &str) -> u64 {
    let (mut left, mut right) = input
        .lines()
        .map(|line| {
            let mut nums = line.split_ascii_whitespace();
            let l = nums.next().unwrap().parse::<u64>().unwrap();
            let r = nums.next().unwrap().parse::<u64>().unwrap();
            (l, r)
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();

    left.sort_unstable();
    right.sort_unstable();

    iter::zip(left, right)
        .map(|(left, right)| {
            let [min, max] = cmp::minmax(left, right);
            max - min
        })
        .sum()
}

pub fn part_two(input: &str) -> u64 {
    let (list, counts) = input
        .lines()
        .map(|line| {
            let mut nums = line.split_ascii_whitespace();
            let l = nums.next().unwrap().parse::<u64>().unwrap();
            let r = nums.next().unwrap().parse::<u64>().unwrap();
            (l, r)
        })
        .fold(
            (Vec::new(), HashMap::<u64, u64>::new()),
            |(mut list, mut counts), nums| {
                list.push(nums.0);
                *counts.entry(nums.1).or_default() += 1;
                (list, counts)
            },
        );

    list.into_iter()
        .map(|num| num * counts.get(&num).copied().unwrap_or(0))
        .sum()
}
