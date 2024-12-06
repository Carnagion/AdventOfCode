use std::{collections::HashSet, usize};

pub fn part_one(input: &str) -> usize {
    let map = input
        .lines()
        .map(|line| line.as_bytes().to_vec())
        .collect::<Vec<_>>();

    let start = map
        .iter()
        .enumerate()
        .find_map(|(y, row)| {
            let x = row
                .iter()
                .enumerate()
                .find_map(|(x, &col)| (col == b'^').then_some(x))?;
            Some((x, y))
        })
        .unwrap();

    let mut guard = Guard {
        pos: start,
        dir: (0, -1),
    };

    let mut visited = HashSet::new();
    step_until(&map, &mut guard, &mut visited, usize::MAX);
    visited.len()
}

struct Guard {
    pos: (usize, usize),
    dir: (isize, isize),
}

impl Guard {
    fn rotate(&mut self) {
        let dir = match self.dir {
            (0, -1) => (1, 0),
            (1, 0) => (0, 1),
            (0, 1) => (-1, 0),
            (-1, 0) => (0, -1),
            _ => unreachable!(),
        };
        self.dir = dir;
    }
}

fn step(map: &[Vec<u8>], guard: &mut Guard) -> bool {
    let x = guard.pos.0.checked_add_signed(guard.dir.0);
    let y = guard.pos.1.checked_add_signed(guard.dir.1);
    let Some((x, y)) = x.zip(y) else { return false };

    match map.get(y).and_then(|row| row.get(x)) {
        None => false,
        Some(b'#') => {
            guard.rotate();
            true
        },
        Some(_) => {
            guard.pos = (x, y);
            true
        },
    }
}

fn step_until(
    map: &[Vec<u8>],
    guard: &mut Guard,
    visited: &mut HashSet<(usize, usize)>,
    limit: usize,
) -> bool {
    let mut count = 0;

    visited.clear();
    visited.insert(guard.pos);

    loop {
        if count >= limit {
            break false;
        }

        if !step(map, guard) {
            break true;
        }

        count += 1;
        visited.insert(guard.pos);
    }
}

pub fn part_two(input: &str) -> usize {
    let mut map = input
        .lines()
        .map(|line| line.as_bytes().to_vec())
        .collect::<Vec<_>>();

    let start = map
        .iter()
        .enumerate()
        .find_map(|(y, row)| {
            let x = row
                .iter()
                .enumerate()
                .find_map(|(x, &col)| (col == b'^').then_some(x))?;
            Some((x, y))
        })
        .unwrap();

    let mut guard = Guard {
        pos: start,
        dir: (0, -1),
    };

    let mut visited = HashSet::new();
    step_until(&map, &mut guard, &mut visited, usize::MAX);

    let mut count = 0;
    let mut visited_re = visited.clone();
    for pos in visited {
        map[pos.1][pos.0] = b'#';

        let mut guard = Guard {
            pos: start,
            dir: (0, -1),
        };

        if !step_until(&map, &mut guard, &mut visited_re, 5500) {
            count += 1;
        }

        map[pos.1][pos.0] = b'.';
    }

    count
}
