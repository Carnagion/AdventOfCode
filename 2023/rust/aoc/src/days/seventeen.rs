use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashSet},
};

pub fn part_one(input: &str) -> u32 {
    let map = parse_map(input);
    min_heat_loss_path(&map, 0, 3).unwrap().heat_loss
}

pub fn part_two(input: &str) -> u32 {
    let map = parse_map(input);
    min_heat_loss_path(&map, 4, 10).unwrap().heat_loss
}

fn parse_map(input: &str) -> Vec<Vec<u8>> {
    input
        .lines()
        .map(|row| row.bytes().map(|tile| tile - b'0').collect())
        .collect()
}

type Pos = (usize, usize);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct State {
    pos: Pos,
    dir: Dir,
    consecutive: u8,
    heat_loss: u32,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.heat_loss.cmp(&self.heat_loss)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl State {
    fn adjacent<'a>(
        &'a self,
        map: &'a [Vec<u8>],
        min_consecutive: u8,
        max_consecutive: u8,
    ) -> impl Iterator<Item = Self> + 'a {
        let sides = match self.dir {
            Dir::North | Dir::South => [Dir::East, Dir::West],
            Dir::East | Dir::West => [Dir::North, Dir::South],
        };
        (self.consecutive >= min_consecutive)
            .then_some(sides)
            .into_iter()
            .flatten()
            .map(|dir| (dir, 1))
            .chain((self.consecutive < max_consecutive).then_some((self.dir, self.consecutive + 1)))
            .filter_map(|(dir, consecutive)| {
                let pos @ (row, col) = dir.next(self.pos);
                let heat_loss = self.heat_loss + *map.get(row).and_then(|row| row.get(col))? as u32;
                Some(Self {
                    pos,
                    dir,
                    consecutive,
                    heat_loss,
                })
            })
    }
}

fn min_heat_loss_path(map: &[Vec<u8>], min_consecutive: u8, max_consecutive: u8) -> Option<State> {
    let rows = map.len();
    let cols = map[0].len();

    let start = (0, 0);
    let mut queue = BinaryHeap::from_iter([
        State {
            pos: start,
            dir: Dir::East,
            consecutive: 0,
            heat_loss: 0,
        },
        State {
            pos: start,
            dir: Dir::South,
            consecutive: 0,
            heat_loss: 0,
        },
    ]);

    let mut seen = HashSet::new();

    let mut goal = None;
    while let Some(state) = queue.pop() {
        if !seen.insert((state.pos, state.dir, state.consecutive)) {
            continue;
        }

        if state.pos == (rows - 1, cols - 1) && state.consecutive > min_consecutive {
            goal = Some(state);
            break;
        }

        let states = state.adjacent(map, min_consecutive, max_consecutive);
        queue.extend(states);
    }
    goal
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Dir {
    North,
    East,
    South,
    West,
}

impl Dir {
    fn next(self, (row, col): Pos) -> Pos {
        match self {
            Self::North => (row - 1, col),
            Self::East => (row, col + 1),
            Self::South => (row + 1, col),
            Self::West => (row, col - 1),
        }
    }
}
