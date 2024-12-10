use std::collections::HashSet;

pub fn part_one(input: &str) -> usize {
    let grid = input
        .lines()
        .map(|line| line.as_bytes().to_vec())
        .collect::<Vec<_>>();

    let trailheads = grid.iter().enumerate().flat_map(|(y, row)| {
        row.iter()
            .enumerate()
            .filter_map(move |(x, &height)| (height == b'0').then_some((x, y)))
    });

    trailheads.map(|trailhead| score(trailhead, &grid)).sum()
}

type Pos = (usize, usize);

fn score(trailhead: Pos, grid: &[Vec<u8>]) -> usize {
    let mut nines = HashSet::new();

    let mut next = Vec::from_iter(adjacents(trailhead, grid));
    while let Some((x, y, height)) = next.pop() {
        if height == b'9' {
            nines.insert((x, y));
        } else {
            next.extend(adjacents((x, y), grid));
        }
    }

    nines.len()
}

pub fn part_two(input: &str) -> usize {
    let grid = input
        .lines()
        .map(|line| line.as_bytes().to_vec())
        .collect::<Vec<_>>();

    let trailheads = grid.iter().enumerate().flat_map(|(y, row)| {
        row.iter()
            .enumerate()
            .filter_map(move |(x, &height)| (height == b'0').then_some((x, y)))
    });

    trailheads.map(|trailhead| rating(trailhead, &grid)).sum()
}

fn rating(trailhead: Pos, grid: &[Vec<u8>]) -> usize {
    let mut count = 0;

    let mut next = Vec::from_iter(adjacents(trailhead, grid));
    while let Some((x, y, height)) = next.pop() {
        if height == b'9' {
            count += 1;
        } else {
            next.extend(adjacents((x, y), grid));
        }
    }

    count
}

fn adjacents(
    (x, y): Pos,
    grid: &[Vec<u8>],
) -> impl IntoIterator<Item = (usize, usize, u8)> + use<'_> {
    [
        (Some(x), y.checked_sub_signed(1)),
        (Some(x + 1), Some(y)),
        (Some(x), Some(y + 1)),
        (x.checked_sub_signed(1), Some(y)),
    ]
    .into_iter()
    .filter_map(move |(to_x, to_y)| {
        let (to_x, to_y) = to_x.zip(to_y)?;
        let &from_height = grid.get(y).and_then(|row| row.get(x))?;
        let &to_height = grid.get(to_y).and_then(|row| row.get(to_x))?;
        to_height
            .checked_signed_diff(from_height)
            .is_some_and(|diff| diff == 1)
            .then_some((to_x, to_y, to_height))
    })
}
