use std::collections::{HashMap, HashSet};

pub fn part_one(input: &str) -> usize {
    let (mut pipes, start) = parse_pipes(input);
    replace_start(start, &mut pipes);

    let mut visited = HashSet::from([start]);
    let mut prev = start;
    let mut next = pipes[&start].neighbours(start)[0];
    while next != start {
        let current = next;
        visited.insert(current);
        next = pipes[&current]
            .neighbours(current)
            .into_iter()
            .find(|&neighbour| neighbour != prev)
            .unwrap();
        prev = current;
    }

    visited.len() / 2
}

pub fn part_two(input: &str) -> usize {
    let (mut pipes, start) = parse_pipes(input);
    replace_start(start, &mut pipes);

    let mut visited = HashSet::from([start]);
    let mut prev = start;
    let mut next = pipes[&start].neighbours(start)[0];
    while next != start {
        let current = next;
        visited.insert(current);
        next = pipes[&current]
            .neighbours(current)
            .into_iter()
            .find(|&neighbour| neighbour != prev)
            .unwrap();
        prev = current;
    }

    let ((min_row, min_col), (max_row, max_col)) = visited
        .iter()
        .copied()
        .next()
        .map(|bounds| {
            visited.iter().copied().fold(
                (bounds, bounds),
                |((min_row, min_col), (max_row, max_col)), (row, col)| {
                    (
                        (min_row.min(row), min_col.min(col)),
                        (max_row.max(row), max_col.max(col)),
                    )
                },
            )
        })
        .unwrap();

    let mut count = 0;
    for row in min_row + 1..=max_row - 1 {
        let mut passed = false;
        for col in min_col..=max_col {
            if visited.contains(&(row, col)) {
                if let Pipe::Verical | Pipe::NorthEast | Pipe::NorthWest = pipes[&(row, col)] {
                    passed = !passed;
                }
            } else if passed {
                count += 1;
            }
        }
    }

    count
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Pipe {
    Verical,
    Horizontal,
    NorthEast,
    NorthWest,
    SouthWest,
    SouthEast,
}

impl Pipe {
    fn neighbours(&self, (row, col): Pos) -> [Pos; 2] {
        match self {
            Self::Verical => [(row - 1, col), (row + 1, col)],
            Self::Horizontal => [(row, col - 1), (row, col + 1)],
            Self::NorthEast => [(row - 1, col), (row, col + 1)],
            Self::NorthWest => [(row - 1, col), (row, col - 1)],
            Self::SouthWest => [(row + 1, col), (row, col - 1)],
            Self::SouthEast => [(row + 1, col), (row, col + 1)],
        }
    }
}

type Pos = (usize, usize);

fn parse_pipes(input: &str) -> (HashMap<Pos, Pipe>, Pos) {
    let mut start = None;
    let pipes = input
        .lines()
        .enumerate()
        .flat_map(|(row, line)| {
            line.bytes()
                .enumerate()
                .map(move |(col, tile)| (row, col, tile))
        })
        .filter_map(|(row, col, tile)| {
            let pipe = Some(match tile {
                b'|' => Pipe::Verical,
                b'-' => Pipe::Horizontal,
                b'L' => Pipe::NorthEast,
                b'J' => Pipe::NorthWest,
                b'7' => Pipe::SouthWest,
                b'F' => Pipe::SouthEast,
                b'S' => {
                    start = Some((row, col));
                    None?
                },
                _ => None?,
            })?;
            Some(((row, col), pipe))
        })
        .collect();
    (pipes, start.unwrap())
}

fn replace_start(start: Pos, pipes: &mut HashMap<Pos, Pipe>) {
    let connections = Pipe::Verical
        .neighbours(start)
        .into_iter()
        .chain(Pipe::Horizontal.neighbours(start))
        .map(|neighbour_pos| {
            pipes
                .get(&neighbour_pos)
                .filter(|neighbour| {
                    neighbour
                        .neighbours(neighbour_pos)
                        .into_iter()
                        .any(|neighbour| neighbour == start)
                })
                .is_some()
        })
        .collect::<Vec<_>>();
    let replacement = match connections.as_slice() {
        [true, true, false, false] => Pipe::Verical,
        [false, false, true, true] => Pipe::Horizontal,
        [true, false, false, true] => Pipe::NorthEast,
        [true, false, true, false] => Pipe::NorthWest,
        [false, true, true, false] => Pipe::SouthWest,
        [false, true, false, true] => Pipe::SouthEast,
        _ => panic!("invalid neighbours"),
    };
    pipes.insert(start, replacement);
}
