use std::str;

pub fn part_one(input: &str) -> i64 {
    let dig_plan = parse_dig_plan(input).collect::<Vec<_>>();
    dig_area(&dig_plan)
}

pub fn part_two(input: &str) -> i64 {
    let dig_plan = parse_dig_plan_from_colours(input).collect::<Vec<_>>();
    dig_area(&dig_plan)
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Dir {
    Up,
    Down,
    Left,
    Right,
}

impl Dir {
    fn parse(dir: u8) -> Self {
        match dir {
            b'U' => Self::Up,
            b'D' => Self::Down,
            b'L' => Self::Left,
            b'R' => Self::Right,
            _ => panic!("invalid direction"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Dig {
    dir: Dir,
    meters: i64,
}

impl Dig {
    fn next(&self, (row, col): Pos) -> Pos {
        let meters = self.meters;
        match self.dir {
            Dir::Up => (row - meters, col),
            Dir::Down => (row + meters, col),
            Dir::Left => (row, col - meters),
            Dir::Right => (row, col + meters),
        }
    }
}

fn parse_dig_plan(input: &str) -> impl Iterator<Item = Dig> + '_ {
    input.lines().map(|line| {
        let mut parts = line.split_ascii_whitespace();
        let [dir, meters] = parts.next_chunk().unwrap();
        let dir = Dir::parse(dir.as_bytes()[0]);
        let meters = meters.parse().unwrap();
        Dig { dir, meters }
    })
}

fn parse_dig_plan_from_colours(input: &str) -> impl Iterator<Item = Dig> + '_ {
    input.lines().map(|line| {
        let colour_code = line
            .split_ascii_whitespace()
            .nth(2)
            .unwrap()
            .trim_matches(['(', ')'].as_slice())
            .trim_start_matches('#')
            .as_bytes();

        let meters = str::from_utf8(&colour_code[..5]).unwrap();
        let meters = i64::from_str_radix(meters, 16).unwrap();

        let dir = match colour_code[5] {
            b'0' => Dir::Right,
            b'1' => Dir::Down,
            b'2' => Dir::Left,
            b'3' => Dir::Up,
            _ => panic!("invalid direction"),
        };

        Dig { dir, meters }
    })
}

type Pos = (i64, i64);

fn dig_area(dig_plan: &[Dig]) -> i64 {
    let exterior = exterior_points(dig_plan);
    let shoelace_area = exterior
        .iter()
        .zip(&exterior[1..])
        .map(|((prev_row, prev_col), (next_row, next_col))| {
            (prev_row - next_row) * (prev_col + next_col) / 2
        })
        .sum::<i64>()
        .abs();
    let perimeter = dig_plan.iter().map(|dig| dig.meters as i64).sum::<i64>();
    shoelace_area + (perimeter / 2) + 1
}

fn exterior_points(dig_plan: &[Dig]) -> Vec<Pos> {
    dig_plan
        .iter()
        .scan((0, 0), |pos, dig| {
            let next = dig.next(*pos);
            *pos = next;
            Some(next)
        })
        .collect()
}

// fn dig_exterior(dig_plan: impl Iterator<Item = Dig>) -> HashSet<Pos> {
//     let (points, _) = dig_plan.fold((HashSet::new(), (0, 0)), |(mut points, (row, col)), dig| {
//         let meters = dig.meters as u32;
//         let next = match dig.dir {
//             Dir::Up => {
//                 (row - meters..=row).map(|row| (row, col)).for_each(|pt| {
//                     points.insert(pt);
//                 });
//                 (row - meters, col)
//             },
//             Dir::Down => {
//                 (row..=row + meters).map(|row| (row, col)).for_each(|pt| {
//                     points.insert(pt);
//                 });
//                 (row + meters, col)
//             },
//             Dir::Left => {
//                 (col - meters..=col).map(|col| (row, col)).for_each(|pt| {
//                     points.insert(pt);
//                 });
//                 (row, col - meters)
//             },
//             Dir::Right => {
//                 (col..=col + meters).map(|col| (row, col)).for_each(|pt| {
//                     points.insert(pt);
//                 });
//                 (row, col + meters)
//             },
//         };
//         (points, next)
//     });
//     points
// }
