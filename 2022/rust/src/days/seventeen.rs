pub fn solve(part: u8, input: String) -> String {
    match part {
        1 => {
            let jets = input.chars()
                .filter_map(|char| Jet::try_from(char).ok())
                .collect();
            let mut tower = Tower(Vec::new());
            Rock::rock_cycle()
                .take(2022)
                .fold(0, |index, mut rock| tower.drop_rock(&mut rock, &jets, index));
            format!("{}", tower.0.len())
        },
        2 => {
            input
        },
        _ => String::from("Invalid part"),
    }
}

enum Jet {
    Left,
    Right,
}

impl TryFrom<char> for Jet {
    type Error = ();
    fn try_from(char: char) -> Result<Self, Self::Error> {
        match char {
            '<' => Ok(Jet::Left),
            '>' => Ok(Jet::Right),
            _ => Err(()),
        }
    }
}

#[derive(Clone)]
struct Rock(u32);

impl Rock {
    fn rock_cycle() -> impl Iterator<Item = Self> {
        [
            Self(0b_00000000_00000000_00000000_00011110),
            Self(0b_00000000_00001000_00011100_00001000),
            Self(0b_00000000_00000100_00000100_00011100),
            Self(0b_00010000_00010000_00010000_00010000),
            Self(0b_00000000_00000000_00011000_00011000),
        ].into_iter().cycle()
    }

    fn move_with_jet(&mut self, jet: &Jet, surroundings: u32) {
        let position =  match jet {
            Jet::Left => if self.0 & 0x40404040 == 0 {
                self.0 << 1
            } else {
                self.0
            },
            Jet::Right => if self.0 & 0x01010101 == 0 {
                self.0 >> 1
            } else {
                self.0
            },
        };
        if position != self.0 && position & surroundings == 0 {
            self.0 = position;
        }
    }

    fn intersects_surroundings(&self, surroundings: u32) -> bool {
        self.0 & surroundings != 0
    }
}

struct Tower(Vec<u8>);

impl Tower {
    fn surroundings_at_height(&self, height: usize) -> u32 {
        if height < self.0.len() {
            self.0[height..].iter()
                .take(4)
                .rev()
                .fold(0, |surr, row| (surr << 8) | *row as u32)
        } else {
            0
        }
    }

    fn drop_rock(&mut self, rock: &mut Rock, jets: &Vec<Jet>, mut index: usize) -> usize {
        let mut height = self.0.len() + 3;
        loop {
            let jet = &jets[index];
            index = (index + 1) % jets.len();
            let surroundings = self.surroundings_at_height(height);
            rock.move_with_jet(jet, surroundings);
            if height > self.0.len() {
                height -= 1;
            } else if height == 0 || rock.intersects_surroundings(self.surroundings_at_height(height - 1)) {
                for byte in rock.0.to_le_bytes()
                    .into_iter()
                    .take_while(|byte| *byte != 0) {
                    if height < self.0.len() {
                        self.0[height] |= byte;
                    } else {
                        self.0.push(byte);
                    }
                    height += 1;
                }
                return index;
            } else {
                height -= 1;
            }
        }
    }
}