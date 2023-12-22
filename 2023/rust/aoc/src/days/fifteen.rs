use std::array;

pub fn part_one(input: &str) -> u32 {
    input.trim().split(',').map(str::as_bytes).map(hash).sum()
}

pub fn part_two(input: &str) -> usize {
    let boxes = array::from_fn::<_, 256, _>(|_| Vec::<(&str, u8)>::new());
    let boxes = input
        .trim()
        .split(',')
        .map(LensOp::parse)
        .fold(boxes, |mut boxes, op| {
            op.update(&mut boxes);
            boxes
        });
    boxes
        .into_iter()
        .enumerate()
        .map(|(box_idx, b)| {
            b.into_iter()
                .map(|(_, focal_len)| focal_len)
                .enumerate()
                .map(|(lens_idx, focal_len)| (box_idx + 1) * (lens_idx + 1) * (focal_len as usize))
                .sum::<usize>()
        })
        .sum()
}

fn hash(input: &[u8]) -> u32 {
    input
        .iter()
        .copied()
        .fold(0, |acc, ascii| ((acc + ascii as u32) * 17).rem_euclid(256))
}

enum LensOp<'a> {
    Remove { label: &'a str },
    Replace { label: &'a str, focal_len: u8 },
}

impl<'a> LensOp<'a> {
    fn parse(step: &'a str) -> Self {
        let mut step = step.split_inclusive(|op| op == '=' || op == '-');
        let label = step.next().unwrap();
        match label.as_bytes().last() {
            Some(b'-') => Self::Remove {
                label: label.trim_end_matches('-'),
            },
            Some(b'=') => {
                let focal_len = step.next().unwrap().parse().unwrap();
                Self::Replace {
                    label: label.trim_end_matches('='),
                    focal_len,
                }
            },
            _ => panic!("invalid step"),
        }
    }

    fn update(&self, boxes: &mut [Vec<(&'a str, u8)>; 256]) {
        match self {
            LensOp::Remove { label } => {
                let hash = hash(label.as_bytes());
                boxes[hash as usize].retain(|(lbl, _)| lbl != label);
            },
            LensOp::Replace { label, focal_len } => {
                let hash = hash(label.as_bytes());
                let focal_len = *focal_len;
                match boxes[hash as usize]
                    .iter_mut()
                    .find_map(|(lbl, focal)| (lbl == label).then_some(focal))
                {
                    Some(focal) => *focal = focal_len,
                    None => boxes[hash as usize].push((label, focal_len)),
                }
            },
        }
    }
}
