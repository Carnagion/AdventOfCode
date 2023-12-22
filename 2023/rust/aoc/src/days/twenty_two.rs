use std::collections::HashMap;

pub fn part_one(input: &str) -> u32 {
    let mut bricks = input.lines().map(Brick::parse).collect::<Vec<_>>();
    bricks.sort_unstable_by_key(|brick| brick.min.2);
    let (dropped_bricks, _) = drop_bricks(bricks);

    let mut removable = 0;
    for idx in 0..dropped_bricks.len() {
        let bricks = dropped_bricks[..idx]
            .iter()
            .cloned()
            .chain(dropped_bricks[idx + 1..].iter().cloned());
        let (_, fall_count) = drop_bricks(bricks);
        if fall_count == 0 {
            removable += 1;
        }
    }
    removable
}

pub fn part_two(input: &str) -> u32 {
    let mut bricks = input.lines().map(Brick::parse).collect::<Vec<_>>();
    bricks.sort_unstable_by_key(|brick| brick.min.2);
    let (dropped_bricks, _) = drop_bricks(bricks);

    let mut fall_counts = 0;
    for idx in 0..dropped_bricks.len() {
        let bricks = dropped_bricks[..idx]
            .iter()
            .cloned()
            .chain(dropped_bricks[idx + 1..].iter().cloned());
        let (_, fall_count) = drop_bricks(bricks);
        fall_counts += fall_count;
    }
    fall_counts
}

type Pos = (u32, u32, u32);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Brick {
    min: Pos,
    max: Pos,
}

impl Brick {
    fn parse(coords: &str) -> Self {
        let (min, max) = coords.split_once('~').unwrap();

        let [x, y, z] = min
            .split(',')
            .map(|pt| pt.parse().unwrap())
            .next_chunk()
            .unwrap();
        let min = (x, y, z);

        let [x, y, z] = max
            .split(',')
            .map(|pt| pt.parse().unwrap())
            .next_chunk()
            .unwrap();
        let max = (x, y, z);

        Self { min, max }
    }

    fn xy(&self) -> impl Iterator<Item = (u32, u32)> + '_ {
        (self.min.0..=self.max.0).flat_map(|x| (self.min.1..=self.max.1).map(move |y| (x, y)))
    }

    fn dropped(&self, topmost: &HashMap<(u32, u32), u32>) -> Self {
        let topmost_z = self
            .xy()
            .map(|xy| topmost.get(&xy).copied().unwrap_or(0))
            .max()
            .unwrap();

        let fall_distance = self.min.2.saturating_sub(topmost_z).saturating_sub(1);

        let mut dropped = self.clone();
        dropped.min.2 -= fall_distance;
        dropped.max.2 -= fall_distance;

        dropped
    }
}

fn drop_bricks(bricks: impl IntoIterator<Item = Brick>) -> (Vec<Brick>, u32) {
    let mut topmost = HashMap::new();
    let mut dropped = Vec::new();
    let mut fall_count = 0;
    for brick in bricks {
        let dropped_brick = brick.dropped(&topmost);
        if dropped_brick.min.2 != brick.min.2 {
            fall_count += 1;
        }
        topmost.extend(brick.xy().map(|xy| (xy, dropped_brick.max.2)));
        dropped.push(dropped_brick);
    }
    (dropped, fall_count)
}
