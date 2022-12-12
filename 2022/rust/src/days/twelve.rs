use std::collections::HashMap;

use pathfinding::directed::bfs;

pub fn solve(part: u8, input: String) -> String {
    let grid = input.lines()
        .zip(0..)
        .flat_map(|(line, row)| line.chars().zip(0..)
            .map(move |(char, col)| (row, col, char)))
        .fold(HashMap::new(), |mut map, (row, col, char)| {
            map.insert((row, col), char);
            map
        });
    match part {
        1 => {
            let start = find_node(&grid, |char| char == &'S').unwrap();
            format!("{}", bfs::bfs(&(start.0, start.1, *start.2), |node| find_neighbours(&grid, *node), |node| node.2 == 'E')
                .unwrap_or(vec![])
                .len() - 1)
        },
        2 => {
            format!("{}", grid.iter()
                .filter(|node| elevation(*node.1) == 0)
                .filter_map(|((row, col), char)| bfs::bfs(&(*row, *col, *char), |node| find_neighbours(&grid, *node), |node| node.2 == 'E'))
                .map(|path| path.len() - 1)
                .min()
                .unwrap_or(0))
        },
        _ => String::from("Invalid part"),
    }
}

fn find_node<N>(grid: &HashMap<(i32, i32), N>, found: impl Fn(&N) -> bool) -> Option<(i32, i32, &N)> {
    grid.iter()
        .find(|(_, node)| found(node))
        .map(|((row, col), node)| (*row, *col, node))
}

fn find_neighbours(grid: &HashMap<(i32, i32), char>, node: (i32, i32, char)) -> impl IntoIterator<Item = (i32, i32, char)> + '_ {
    let (row, col, char_l) = node;
    [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)].into_iter()
        .filter_map(|pos| grid.get_key_value(&pos))
        .map(|((row, col), char)| (*row, *col, *char))
        .filter(move |(_, _, char_r)| elevation(*char_r) <= elevation(char_l) + 1)
}

fn elevation(char: char) -> u32 {
    match char {
        'S' => elevation('a'),
        'E' => elevation('z'),
        char => char as u32 - 'a' as u32,
    }
}