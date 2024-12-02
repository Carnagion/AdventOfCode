pub fn part_one(data: &str) -> usize {
    data.lines()
        .filter(|line| {
            let report = line
                .split_ascii_whitespace()
                .map(|n| n.parse::<u32>().unwrap());
            is_safe(report)
        })
        .count()
}

pub fn part_two(data: &str) -> usize {
    data.lines()
        .filter(|line| {
            let report = line
                .split_ascii_whitespace()
                .map(|n| n.parse::<u32>().unwrap())
                .collect::<Vec<_>>();
            is_safe_dampened(&report)
        })
        .count()
}

fn is_safe(levels: impl Iterator<Item = u32>) -> bool {
    let mut diffs = levels.map_windows(|&[l, r]| l.checked_signed_diff(r).unwrap());
    let sign = match diffs.next() {
        None => return true,
        Some(diff) if (1..=3).contains(&diff.abs()) && diff.signum() != 0 => diff.signum(),
        _ => return false,
    };
    diffs.all(|diff| diff.signum() == sign && (1..=3).contains(&diff.abs()))
}

fn is_safe_dampened(levels: &[u32]) -> bool {
    (0..levels.len()).any(|n| {
        let sublist = levels
            .iter()
            .take(n)
            .chain(levels.iter().skip(n + 1))
            .copied();
        is_safe(sublist)
    })
}
