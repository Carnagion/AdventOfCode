use std::collections::HashSet;

pub fn part_one(input: &str) -> u64 {
    let (garden, start) = parse_garden(input);
    count_reachable(&garden, start, 64)
}

pub fn part_two(input: &str) -> u64 {
    let (garden, start @ (row, col)) = parse_garden(input);

    let steps = 26501365_u64;

    let size = input.lines().count() as u64;
    let grid_size = steps / size - 1;

    let odd_count = (grid_size / 2 * 2 + 1).pow(2);
    let even_count = ((grid_size + 1) / 2 * 2).pow(2);

    let odd_reachable = count_reachable(&garden, start, size * 2 + 1);
    let even_reachable = count_reachable(&garden, start, size * 2);

    let full_reachable = (odd_count * odd_reachable) + (even_count * even_reachable);

    let corner_top = count_reachable(&garden, (size - 1, col), size - 1);
    let corner_right = count_reachable(&garden, (row, 0), size - 1);
    let corner_bottom = count_reachable(&garden, (0, col), size - 1);
    let corner_left = count_reachable(&garden, (row, size - 1), size - 1);

    let corner_reachable = corner_top + corner_right + corner_bottom + corner_left;

    let small_diag_top_right = count_reachable(&garden, (size - 1, 0), size / 2 - 1);
    let small_diag_bottom_right = count_reachable(&garden, (0, 0), size / 2 - 1);
    let small_diag_bottom_left = count_reachable(&garden, (0, size - 1), size / 2 - 1);
    let small_diag_top_left = count_reachable(&garden, (size - 1, size - 1), size / 2 - 1);

    let small_diag_reachable = (grid_size + 1)
        * (small_diag_top_right
            + small_diag_bottom_right
            + small_diag_bottom_left
            + small_diag_top_left);

    let big_diag_top_right = count_reachable(&garden, (size - 1, 0), size * 3 / 2 - 1);
    let big_diag_bottom_right = count_reachable(&garden, (0, 0), size * 3 / 2 - 1);
    let big_diag_bottom_left = count_reachable(&garden, (0, size - 1), size * 3 / 2 - 1);
    let big_diag_top_left = count_reachable(&garden, (size - 1, size - 1), size * 3 / 2 - 1);

    let big_diag_reachable = grid_size
        * (big_diag_top_right + big_diag_bottom_right + big_diag_bottom_left + big_diag_top_left);

    let diag_reachable = small_diag_reachable + big_diag_reachable;

    full_reachable + corner_reachable + diag_reachable
}

fn count_reachable(garden: &HashSet<Pos>, start: Pos, steps: u64) -> u64 {
    let reachable = HashSet::from_iter([start]);
    (0..steps)
        .fold(reachable, |reachable, _| {
            reachable
                .into_iter()
                .flat_map(neighbours)
                .filter(|neighbour| garden.contains(neighbour))
                .collect::<HashSet<_>>()
        })
        .len() as u64
}

type Pos = (u64, u64);

fn parse_garden(input: &str) -> (HashSet<Pos>, Pos) {
    let mut start = None;
    let mut garden = HashSet::new();
    for (row, line) in input.lines().enumerate() {
        for (col, tile) in line.bytes().enumerate() {
            let pos = (row as u64, col as u64);
            match tile {
                b'S' => {
                    start = Some(pos);
                    garden.insert(pos);
                },
                b'.' => {
                    garden.insert(pos);
                },
                b'#' => continue,
                _ => panic!("invalid tile"),
            }
        }
    }
    (garden, start.unwrap())
}

fn neighbours((row, col): Pos) -> [Pos; 4] {
    [
        (row - 1, col),
        (row, col + 1),
        (row + 1, col),
        (row, col - 1),
    ]
}
