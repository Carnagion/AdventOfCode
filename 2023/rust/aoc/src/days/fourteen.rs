use std::collections::HashMap;

pub fn part_one(input: &str) -> usize {
    let mut platform = Platform::parse(input);
    platform.tilt_north();
    platform.load()
}

pub fn part_two(input: &str) -> usize {
    let mut platform = Platform::parse(input);
    let mut seen = HashMap::new();
    let cycles = 1000000000;
    for cycle in 0..cycles {
        platform.tilt_cycle();

        let mut rock_positions = platform.rock_positions();
        rock_positions.sort_unstable();
        if let Some(prev) = seen.insert(rock_positions, cycle) {
            let steps = cycle - prev;
            let remaining = cycles - cycle;
            for _ in 0..(remaining % steps) - 1 {
                platform.tilt_cycle();
            }
            break;
        }
    }
    platform.load()
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Tile {
    Empty,
    Cube,
    Round,
}

struct Platform {
    tiles: Vec<Vec<Tile>>,
}

impl Platform {
    fn parse(input: &str) -> Self {
        let tiles = input
            .lines()
            .map(|line| {
                line.bytes()
                    .map(|tile| match tile {
                        b'.' => Tile::Empty,
                        b'#' => Tile::Cube,
                        b'O' => Tile::Round,
                        _ => panic!("invalid tile"),
                    })
                    .collect()
            })
            .collect();
        Self { tiles }
    }

    fn load(&self) -> usize {
        let max = self.tiles.len();
        self.tiles
            .iter()
            .enumerate()
            .flat_map(|(row, line)| {
                line.iter()
                    .filter_map(move |tile| matches!(tile, Tile::Round).then_some(max - row))
            })
            .sum()
    }

    fn tilt_cycle(&mut self) {
        self.tilt_north();
        self.tilt_west();
        self.tilt_south();
        self.tilt_east();
    }

    fn tilt_north(&mut self) {
        let mut roll_distances = vec![0; self.tiles[0].len()];
        for row in 0..self.tiles.len() {
            for col in 0..self.tiles[0].len() {
                let tile = &mut self.tiles[row][col];
                match tile {
                    Tile::Empty => roll_distances[col] += 1,
                    Tile::Cube => roll_distances[col] = 0,
                    Tile::Round => {
                        let roll_distance = roll_distances[col];
                        let (rest_row, rest_col) = (row - roll_distance, col);
                        *tile = Tile::Empty;
                        self.tiles[rest_row][rest_col] = Tile::Round;
                    },
                }
            }
        }
    }

    fn tilt_west(&mut self) {
        let mut roll_distances = vec![0; self.tiles.len()];
        for col in 0..self.tiles[0].len() {
            for row in 0..self.tiles.len() {
                let tile = &mut self.tiles[row][col];
                match tile {
                    Tile::Empty => roll_distances[row] += 1,
                    Tile::Cube => roll_distances[row] = 0,
                    Tile::Round => {
                        let roll_distance = roll_distances[row];
                        let (rest_row, rest_col) = (row, col - roll_distance);
                        *tile = Tile::Empty;
                        self.tiles[rest_row][rest_col] = Tile::Round;
                    },
                }
            }
        }
    }

    fn tilt_south(&mut self) {
        let mut roll_distances = vec![0; self.tiles[0].len()];
        for row in (0..self.tiles.len()).rev() {
            for col in 0..self.tiles[0].len() {
                let tile = &mut self.tiles[row][col];
                match tile {
                    Tile::Empty => roll_distances[col] += 1,
                    Tile::Cube => roll_distances[col] = 0,
                    Tile::Round => {
                        let roll_distance = roll_distances[col];
                        let (rest_row, rest_col) = (row + roll_distance, col);
                        *tile = Tile::Empty;
                        self.tiles[rest_row][rest_col] = Tile::Round;
                    },
                }
            }
        }
    }

    fn tilt_east(&mut self) {
        let mut roll_distances = vec![0; self.tiles.len()];
        for col in (0..self.tiles[0].len()).rev() {
            for row in 0..self.tiles.len() {
                let tile = &mut self.tiles[row][col];
                match tile {
                    Tile::Empty => roll_distances[row] += 1,
                    Tile::Cube => roll_distances[row] = 0,
                    Tile::Round => {
                        let roll_distance = roll_distances[row];
                        let (rest_row, rest_col) = (row, col + roll_distance);
                        *tile = Tile::Empty;
                        self.tiles[rest_row][rest_col] = Tile::Round;
                    },
                }
            }
        }
    }

    fn rock_positions(&self) -> Vec<(usize, usize)> {
        self.tiles
            .iter()
            .enumerate()
            .flat_map(|(row, line)| {
                line.iter()
                    .enumerate()
                    .map(move |(col, tile)| (row, col, tile))
            })
            .filter_map(|(row, col, &tile)| (tile == Tile::Round).then_some((row, col)))
            .collect()
    }
}
