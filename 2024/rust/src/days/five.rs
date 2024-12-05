use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

pub fn part_one(input: &str) -> u32 {
    solve(input, false)
}

pub fn part_two(input: &str) -> u32 {
    solve(input, true)
}

fn solve(input: &str, part_two: bool) -> u32 {
    let (rules, updates) = input.split_once("\n\n").unwrap();

    let rules = rules
        .lines()
        .map(|line| {
            let (before, after) = line.split_once('|').unwrap();
            let before = before.parse::<u32>().unwrap();
            let after = after.parse::<u32>().unwrap();
            (before, after)
        })
        .fold(
            HashMap::<u32, HashSet<u32>>::new(),
            |mut map, (before, after)| {
                map.entry(before).or_default().insert(after);
                map
            },
        );

    updates
        .lines()
        .map(|line| {
            line.split(',')
                .map(|n| n.parse::<u32>().unwrap())
                .collect::<Vec<_>>()
        })
        .filter_map(|mut update| {
            let original = update.clone();

            update.sort_unstable_by(|before, after| match rules.get(before) {
                Some(afters) if afters.contains(after) => Ordering::Less,
                _ => Ordering::Equal,
            });

            let elem = update[update.len() / 2];
            if part_two {
                (update != original).then_some(elem)
            } else {
                (update == original).then_some(elem)
            }
        })
        .sum()
}
