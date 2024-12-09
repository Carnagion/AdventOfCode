use std::collections::{HashMap, HashSet};

type Pos = (usize, usize);

pub fn part_one(input: &str) -> usize {
    let mut bounds = (0, 0);

    let antennas = find_antennas(input, &mut bounds);
    let antinodes = antennas
        .iter()
        .flat_map(|(&from, antenna)| {
            let matching = antennas
                .iter()
                .filter_map(move |(&to, other)| (other == antenna && to != from).then_some(to));
            matching.map(move |to| next_projected_antinode(from, to))
        })
        .filter(|(row, col)| (0..=bounds.0).contains(row) && (0..=bounds.1).contains(col))
        .collect::<HashSet<_>>();

    antinodes.len()
}

pub fn part_two(input: &str) -> usize {
    let mut bounds = (0, 0);

    let antennas = find_antennas(input, &mut bounds);
    let antinodes = antennas
        .iter()
        .flat_map(|(&from, antenna)| {
            let matching = antennas
                .iter()
                .filter_map(move |(&to, other)| (other == antenna && to != from).then_some(to));
            matching.flat_map(move |to| all_projected_antinodes(from, to, bounds))
        })
        .chain(antennas.keys().copied())
        .collect::<HashSet<_>>();

    antinodes.len()
}

fn find_antennas(input: &str, bounds: &mut (usize, usize)) -> HashMap<Pos, u8> {
    input
        .lines()
        .enumerate()
        .flat_map(|(row, line)| {
            line.bytes()
                .enumerate()
                .map(move |(col, tile)| (row, col, tile))
        })
        .filter_map(|(row, col, tile)| {
            bounds.0 = bounds.0.max(row);
            bounds.1 = bounds.1.max(col);
            (tile != b'.').then_some(((row, col), tile))
        })
        .collect()
}

fn next_projected_antinode((from_row, from_col): Pos, (to_row, to_col): Pos) -> Pos {
    (to_row + (to_row - from_row), to_col + (to_col - from_col))
}

fn all_projected_antinodes(mut from: Pos, mut to: Pos, bounds: (usize, usize)) -> Vec<Pos> {
    let mut antinodes = Vec::new();

    let (mut row, mut col) = next_projected_antinode(from, to);
    while (0..=bounds.0).contains(&row) && (0..=bounds.1).contains(&col) {
        antinodes.push((row, col));
        from = to;
        to = (row, col);
        (row, col) = next_projected_antinode(from, to);
    }

    antinodes
}
