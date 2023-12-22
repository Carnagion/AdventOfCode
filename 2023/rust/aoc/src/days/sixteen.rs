use std::{
    collections::{HashSet, VecDeque},
    iter,
};

pub fn part_one(input: &str) -> usize {
    let mut grid = Grid::parse(input);
    advance_beam((0, 0), Dir::East, &mut grid);
    grid.energy()
}

pub fn part_two(input: &str) -> usize {
    let mut grid = Grid::parse(input);

    let rows = grid.tiles.len();
    let cols = grid.tiles[0].len();

    let start_rows =
        (0..rows).flat_map(|row| [((row, 0), Dir::East), ((row, cols - 1), Dir::West)]);
    let start_cols =
        (0..cols).flat_map(|col| [((0, col), Dir::South), ((rows - 1, col), Dir::North)]);

    start_rows
        .chain(start_cols)
        .map(|(pos, dir)| {
            grid.clear();
            advance_beam(pos, dir, &mut grid);
            grid.energy()
        })
        .max()
        .unwrap()
}

fn advance_beam(pos: Pos, dir: Dir, grid: &mut Grid) {
    let rows = 0..grid.tiles.len();
    let cols = 0..grid.tiles[0].len();

    let mut positions = VecDeque::from_iter([(pos, dir)]);
    while let Some((pos @ (row, col), dir)) = positions.pop_front() {
        if !rows.contains(&row) || !cols.contains(&col) {
            continue;
        }

        let tile = &mut grid.tiles[row][col];
        if !tile.beams.insert(dir) {
            continue;
        }

        match tile.kind {
            TileKind::Empty => {
                positions.push_back((dir.next(pos), dir));
            },
            TileKind::Mirror(mirror) => {
                let dir = mirror.bounce(dir);
                positions.push_back((dir.next(pos), dir));
            },
            TileKind::Splitter(splitter) => {
                let (dir_one, dir_two) = splitter.split(dir);
                positions.extend(
                    dir_two
                        .into_iter()
                        .chain(iter::once(dir_one))
                        .map(|dir| (dir.next(pos), dir)),
                );
            },
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Grid {
    tiles: Vec<Vec<Tile>>,
}

impl Grid {
    fn parse(input: &str) -> Self {
        let tiles = input
            .lines()
            .map(|row| {
                row.bytes()
                    .map(|tile| {
                        let kind = TileKind::parse(tile);
                        Tile {
                            kind,
                            beams: HashSet::new(),
                        }
                    })
                    .collect()
            })
            .collect();
        Self { tiles }
    }

    fn energy(&self) -> usize {
        self.tiles
            .iter()
            .map(|row| row.iter().filter(|tile| tile.is_energized()).count())
            .sum()
    }

    fn clear(&mut self) {
        self.tiles
            .iter_mut()
            .flatten()
            .for_each(|tile| tile.beams.clear());
    }
}

type Pos = (usize, usize);

#[derive(Clone, Debug, Eq, PartialEq)]
struct Tile {
    kind: TileKind,
    beams: HashSet<Dir>,
}

impl Tile {
    fn is_energized(&self) -> bool {
        !self.beams.is_empty()
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum TileKind {
    Empty,
    Splitter(Splitter),
    Mirror(Mirror),
}

impl TileKind {
    fn parse(tile: u8) -> Self {
        match tile {
            b'.' => Self::Empty,
            b'-' => Self::Splitter(Splitter::Horizontal),
            b'|' => Self::Splitter(Splitter::Vertical),
            b'/' => Self::Mirror(Mirror::Front),
            b'\\' => Self::Mirror(Mirror::Back),
            _ => panic!("invalid tile"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Splitter {
    Horizontal,
    Vertical,
}

impl Splitter {
    fn split(&self, dir: Dir) -> (Dir, Option<Dir>) {
        match (self, dir) {
            (Self::Horizontal, Dir::North | Dir::South) => (Dir::West, Some(Dir::East)),
            (Self::Horizontal, Dir::East | Dir::West) => (dir, None),
            (Self::Vertical, Dir::North | Dir::South) => (dir, None),
            (Self::Vertical, Dir::East | Dir::West) => (Dir::North, Some(Dir::South)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Mirror {
    Front,
    Back,
}

impl Mirror {
    fn bounce(&self, dir: Dir) -> Dir {
        match (self, dir) {
            (Self::Front, Dir::North) => Dir::East,
            (Self::Front, Dir::East) => Dir::North,
            (Self::Front, Dir::South) => Dir::West,
            (Self::Front, Dir::West) => Dir::South,
            (Self::Back, Dir::North) => Dir::West,
            (Self::Back, Dir::East) => Dir::South,
            (Self::Back, Dir::West) => Dir::North,
            (Self::Back, Dir::South) => Dir::East,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Dir {
    North,
    East,
    South,
    West,
}

impl Dir {
    fn next(&self, (row, col): Pos) -> Pos {
        match self {
            Self::North => (row - 1, col),
            Self::East => (row, col + 1),
            Self::South => (row + 1, col),
            Self::West => (row, col - 1),
        }
    }
}
