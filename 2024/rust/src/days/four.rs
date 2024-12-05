use std::iter;

pub fn part_one(input: &str) -> usize {
    let grid = input
        .lines()
        .map(|line| line.as_bytes().to_vec())
        .collect::<Vec<_>>();

    let rows = grid.len();
    let cols = grid.first().map_or(0, |row| row.len());

    (0..rows)
        .flat_map(|y| (0..cols).map(move |x| (x, y)))
        .map(|(x, y)| xmas_count(&grid, x, y))
        .sum()
}

fn xmas_count(grid: &[Vec<u8>], x: usize, y: usize) -> usize {
    [-1, 0, 1]
        .into_iter()
        .flat_map(|dx| [-1, 0, 1].map(|dy| (dx, dy)))
        .filter(|&(dx, dy)| is_xmas(grid, x, y, dx, dy))
        .count()
}

fn is_xmas(grid: &[Vec<u8>], x: usize, y: usize, dx: isize, dy: isize) -> bool {
    let xs = [0, dx, dx * 2, dx * 3].map(|dx| x.checked_add_signed(dx));
    let ys = [0, dy, dy * 2, dy * 3].map(|dy| y.checked_add_signed(dy));

    iter::zip(xs, ys).zip(b"XMAS").all(|((x, y), target)| {
        let Some((x, y)) = x.zip(y) else { return false };
        let letter = grid.get(y).and_then(|row| row.get(x));
        letter.is_some_and(|letter| letter == target)
    })
}

pub fn part_two(input: &str) -> usize {
    let grid = input
        .lines()
        .map(|line| line.as_bytes().to_vec())
        .collect::<Vec<_>>();

    let rows = grid.len();
    let cols = grid.first().map_or(0, |row| row.len());

    (1..rows - 1)
        .flat_map(|y| (1..cols - 1).map(move |x| (x, y)))
        .map(|(x, y)| x_mas_count(&grid, x, y))
        .sum()
}

fn x_mas_count(grid: &[Vec<u8>], x: usize, y: usize) -> usize {
    let coords = [
        (x, y),
        (x - 1, y - 1),
        (x + 1, y - 1),
        (x - 1, y + 1),
        (x + 1, y + 1),
    ];
    let letters = coords.map(|(x, y)| grid[y][x]);
    [b"AMSMS", b"AMMSS", b"ASMSM", b"ASSMM"]
        .into_iter()
        .filter(|&target| &letters == target)
        .count()
}
