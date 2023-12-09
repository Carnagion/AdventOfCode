pub fn part_one(input: &str) -> i32 {
    input
        .lines()
        .map(|line| {
            line.split_ascii_whitespace()
                .map(|num| num.parse().unwrap())
                .collect::<Vec<_>>()
        })
        .map(|history| next_value(&history))
        .sum()
}

pub fn part_two(input: &str) -> i32 {
    input
        .lines()
        .map(|line| {
            line.split_ascii_whitespace()
                .map(|num| num.parse().unwrap())
                .rev()
                .collect::<Vec<_>>()
        })
        .map(|history| next_value(&history))
        .sum()
}

fn next_value(history: &[i32]) -> i32 {
    let last = history.iter().fold(None, |last, &val| match last {
        None if val == 0 => None,
        _ => Some(val),
    });
    match last {
        None => 0,
        Some(last) => last + next_value(&step_difference(history).collect::<Vec<_>>()),
    }
}

fn step_difference(history: &[i32]) -> impl Iterator<Item = i32> + '_ {
    history.windows(2).map(|vals| vals[1] - vals[0])
}
