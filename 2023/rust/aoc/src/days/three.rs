use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

pub fn part_one(input: &str) -> u32 {
    let mut nums = Vec::new();
    let mut symbols = HashMap::new();

    for (row, line) in input.lines().map(str::as_bytes).enumerate() {
        let mut col = 0;
        while let Some(ch) = line.get(col) {
            match ch {
                b'.' => col += 1,
                digit if digit.is_ascii_digit() => {
                    let num = line[col..]
                        .iter()
                        .copied()
                        .take_while(u8::is_ascii_digit)
                        .fold(0_u32, |num, digit| num * 10 + (digit - b'0') as u32);
                    let num = Num {
                        num,
                        pos: (row, col),
                    };
                    nums.push(num);
                    col += num.len() as usize;
                },
                sym => {
                    symbols.insert((row, col), sym);
                    col += 1;
                },
            }
        }
    }

    let neighbour_idxs = [
        (0_isize, 1_isize),
        (0, -1),
        (1, 0),
        (-1, 0),
        (1, 1),
        (-1, 1),
        (1, -1),
        (-1, -1),
    ];

    let parts = nums
        .into_iter()
        .filter(|num| {
            let row = num.pos.0;
            neighbour_idxs
                .iter()
                .flat_map(|(row_offset, col_offset)| {
                    num.span().map(move |col| {
                        (
                            (row as isize + row_offset) as usize,
                            (col as isize + col_offset) as usize,
                        )
                    })
                })
                .any(|neighbour_pos| symbols.contains_key(&neighbour_pos))
        })
        .map(|num| num.num);

    parts.sum()
}

pub fn part_two(input: &str) -> u32 {
    let mut nums = Vec::new();
    let mut gears = HashMap::new();

    let neighbour_idxs = [
        (0_isize, 1_isize),
        (0, -1),
        (1, 0),
        (-1, 0),
        (1, 1),
        (-1, 1),
        (1, -1),
        (-1, -1),
    ];

    for (row, line) in input.lines().map(str::as_bytes).enumerate() {
        let mut col = 0;
        while let Some(ch) = line.get(col) {
            match ch {
                b'*' => {
                    gears.insert((row, col), HashSet::new());
                    col += 1;
                },
                digit if digit.is_ascii_digit() => {
                    let num = line[col..]
                        .iter()
                        .copied()
                        .take_while(u8::is_ascii_digit)
                        .fold(0_u32, |num, digit| num * 10 + (digit - b'0') as u32);
                    let num = Num {
                        num,
                        pos: (row, col),
                    };
                    nums.push(num);
                    col += num.len() as usize;
                },
                _ => col += 1,
            }
        }
    }

    for num in nums {
        neighbour_idxs
            .iter()
            .flat_map(|(row_offset, col_offset)| {
                let row = num.pos.0;
                num.span().map(move |col| {
                    (
                        (row as isize + row_offset) as usize,
                        (col as isize + col_offset) as usize,
                    )
                })
            })
            .for_each(|gear_pos| {
                gears.entry(gear_pos).and_modify(|gear_nums| {
                    gear_nums.insert(num.clone());
                });
            });
    }

    gears
        .into_values()
        .filter(|gear_nums| gear_nums.len() == 2)
        .map(|gear_nums| gear_nums.into_iter().map(|num| num.num).product::<u32>())
        .sum()
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Num {
    pub num: u32,
    pub pos: (usize, usize),
}

impl Num {
    pub fn len(&self) -> u32 {
        self.num.ilog10() + 1
    }

    pub fn span(&self) -> Range<usize> {
        self.pos.1..self.pos.1 + self.len() as usize
    }
}
