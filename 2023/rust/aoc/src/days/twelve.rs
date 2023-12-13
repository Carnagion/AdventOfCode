use std::{collections::HashMap, iter};

pub fn part_one(input: &str) -> u64 {
    parse_records(input, 1)
        .map(|record| {
            arrangements(
                &record.conditions,
                &record.damaged_groups,
                &mut HashMap::default(),
            )
        })
        .sum()
}

pub fn part_two(input: &str) -> u64 {
    parse_records(input, 5)
        .map(|record| {
            arrangements(
                &record.conditions,
                &record.damaged_groups,
                &mut HashMap::default(),
            )
        })
        .sum()
}

fn arrangements(
    conditions: &[Condition],
    damaged_groups: &[usize],
    cache: &mut HashMap<Record, u64>,
) -> u64 {
    if let Some(&count) = cache.get(&Record {
        conditions: conditions.to_owned(),
        damaged_groups: damaged_groups.to_owned(),
    }) {
        return count;
    }

    match (conditions, damaged_groups) {
        ([], damaged_groups) => damaged_groups.is_empty() as u64,
        (conditions, []) => conditions.iter().all(|&cond| cond != Condition::Damaged) as u64,
        (
            conditions @ [condition, remaining_conditions @ ..],
            damaged_groups @ [damaged_group, remaining_groups @ ..],
        ) => {
            let condition = *condition;
            let damaged_group = *damaged_group;

            let mut count = 0;

            if condition != Condition::Damaged {
                count += arrangements(remaining_conditions, damaged_groups, cache);
            }

            if condition != Condition::Operational
                && damaged_group <= conditions.len()
                && !conditions[..damaged_group].contains(&Condition::Operational)
                && !conditions
                    .get(damaged_group)
                    .is_some_and(|&cond| cond == Condition::Damaged)
            {
                count += arrangements(
                    &conditions[(damaged_group + 1).min(conditions.len())..],
                    remaining_groups,
                    cache,
                );
            }

            cache.insert(
                Record {
                    conditions: conditions.to_owned(),
                    damaged_groups: damaged_groups.to_owned(),
                },
                count,
            );

            count
        },
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Condition {
    Operational,
    Damaged,
    Unknown,
}

impl Condition {
    fn parse(condition: u8) -> Self {
        match condition {
            b'.' => Self::Operational,
            b'#' => Self::Damaged,
            b'?' => Self::Unknown,
            _ => panic!("invalid condition"),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Record {
    conditions: Vec<Condition>,
    damaged_groups: Vec<usize>,
}

impl Record {
    fn parse(record: &str, folds: usize) -> Self {
        let (conditions, damaged_groups) = record.split_once(' ').unwrap();

        let conditions = iter::repeat(
            iter::once(Condition::Unknown).chain(conditions.bytes().map(Condition::parse)),
        )
        .take(folds)
        .flatten()
        .skip(1)
        .collect();

        let damaged_groups =
            iter::repeat(damaged_groups.split(',').map(|num| num.parse().unwrap()))
                .take(folds)
                .flatten()
                .collect();

        Self {
            conditions,
            damaged_groups,
        }
    }
}

fn parse_records(input: &str, folds: usize) -> impl Iterator<Item = Record> + '_ {
    input.lines().map(move |line| Record::parse(line, folds))
}
