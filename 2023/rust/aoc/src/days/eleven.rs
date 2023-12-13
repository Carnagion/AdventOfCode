use std::collections::HashSet;

pub fn part_one(input: &str) -> u64 {
    let galaxies = parse_galaxies(input, 1);
    galaxy_distances(&galaxies)
}

pub fn part_two(input: &str) -> u64 {
    let galaxies = parse_galaxies(input, 999_999);
    galaxy_distances(&galaxies)
}

type Pos = (u64, u64);

fn parse_galaxies(input: &str, expansion: u64) -> Vec<Pos> {
    let (rows, cols) = (
        input.lines().count() as u64,
        input.lines().next().unwrap().len() as u64,
    );

    let empty_rows = HashSet::<u64>::from_iter(0..rows);
    let empty_cols = HashSet::<u64>::from_iter(0..cols);

    let (mut galaxies, empty_rows, empty_cols) = input
        .lines()
        .enumerate()
        .flat_map(|(row, line)| {
            line.bytes()
                .enumerate()
                .map(move |(col, tile)| (row as u64, col as u64, tile))
        })
        .fold(
            (Vec::new(), empty_rows, empty_cols),
            |(mut galaxies, mut empty_rows, mut empty_cols), (row, col, tile)| {
                if let b'#' = tile {
                    galaxies.push((row, col));
                    empty_rows.remove(&row);
                    empty_cols.remove(&col);
                }
                (galaxies, empty_rows, empty_cols)
            },
        );

    for (row, col) in &mut galaxies {
        *row += empty_rows
            .iter()
            .filter(|&empty_row| empty_row < row)
            .count() as u64
            * expansion;
        *col += empty_cols
            .iter()
            .filter(|&empty_col| empty_col < col)
            .count() as u64
            * expansion;
    }

    galaxies
}

fn galaxy_distances(galaxies: &[Pos]) -> u64 {
    galaxies
        .iter()
        .copied()
        .enumerate()
        .flat_map(|(idx, galaxy_start)| {
            galaxies[idx..]
                .iter()
                .copied()
                .map(move |galaxy_end| (galaxy_start, galaxy_end))
        })
        .map(|((start_row, start_col), (end_row, end_col))| {
            end_row.abs_diff(start_row) + end_col.abs_diff(start_col)
        })
        .sum()
}
