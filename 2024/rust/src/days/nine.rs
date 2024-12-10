use std::cmp::Ordering;

pub fn part_one(input: &str) -> usize {
    let mut blocks = input.bytes().enumerate();
    let mut checksum = 0;

    if blocks.len() % 2 == 0 {
        blocks.next_back();
    }

    let mut file_pos = 0;

    file_checksum(&mut blocks, &mut checksum, &mut file_pos);

    let mut next_space = blocks.next();
    let mut next_file = blocks.next_back();

    loop {
        let (space, file) = match (&mut next_space, &mut next_file) {
            (Some(space), Some(file)) => (space, file),
            (_, None) => break,
            (None, Some((file_idx, file))) => {
                let file_id = *file_idx / 2;
                let file_len = (*file - b'0') as usize;
                checksum += (file_pos..file_pos + file_len).sum::<usize>() * file_id;
                break;
            },
        };

        let (_, space) = space;
        let (file_idx, file) = file;

        let space_len = (*space - b'0') as usize;

        let file_id = *file_idx / 2;
        let file_len = (*file - b'0') as usize;

        match space_len.cmp(&file_len) {
            Ordering::Greater => {
                checksum += (file_pos..file_pos + file_len).sum::<usize>() * file_id;

                file_pos += file_len;

                *space = space.wrapping_sub(*file) + b'0';

                blocks.next_back();
                next_file = blocks.next_back();
            },
            Ordering::Equal => {
                checksum += (file_pos..file_pos + file_len).sum::<usize>() * file_id;

                file_pos += file_len;

                file_checksum(&mut blocks, &mut checksum, &mut file_pos);
                next_space = blocks.next();

                blocks.next_back();
                next_file = blocks.next_back();
            },
            Ordering::Less => {
                checksum += (file_pos..file_pos + space_len).sum::<usize>() * file_id;

                file_pos += space_len;

                *file = file.wrapping_sub(*space) + b'0';

                file_checksum(&mut blocks, &mut checksum, &mut file_pos);
                next_space = blocks.next();
            },
        }
    }

    checksum
}

fn file_checksum<B>(blocks: &mut B, checksum: &mut usize, file_pos: &mut usize)
where
    B: Iterator<Item = (usize, u8)>,
{
    if let Some((file_idx, file)) = blocks.next() {
        let file_id = file_idx / 2;
        let file_len = (file - b'0') as usize;
        *checksum += (*file_pos..*file_pos + file_len).sum::<usize>() * file_id;
        *file_pos += file_len;
    }
}

pub fn part_two(input: &str) -> usize {
    let mut spaces = Vec::with_capacity(input.len());
    let mut files = Vec::with_capacity(input.len());

    let mut pos = 0;
    for (idx, block) in input.bytes().enumerate() {
        let len = (block - b'0') as usize;
        if idx % 2 == 0 {
            let id = idx / 2;
            files.push((id, pos, len));
        } else {
            spaces.push((pos, len));
        }
        pos += len;
    }

    let mut checksum = 0;
    for &(file_id, file_pos, file_len) in files.iter().rev() {
        let space_idx = spaces
            .iter()
            .enumerate()
            .take_while(|(_, &(space_pos, _))| space_pos < file_pos)
            .find_map(|(idx, &(_, space_len))| (space_len >= file_len).then_some(idx));

        let Some(space_idx) = space_idx else {
            checksum += (file_pos..file_pos + file_len).sum::<usize>() * file_id;
            continue;
        };

        let (space_pos, space_len) = &mut spaces[space_idx];
        checksum += (*space_pos..*space_pos + file_len).sum::<usize>() * file_id;

        if *space_len > file_len {
            *space_pos += file_len;
            *space_len -= file_len;
        } else {
            spaces.remove(space_idx);
        }
    }

    checksum
}
