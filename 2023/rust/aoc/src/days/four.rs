use std::collections::HashSet;

pub fn part_one(input: &str) -> u32 {
    parse_cards(input).map(|card| card.points()).sum()
}

pub fn part_two(input: &str) -> u32 {
    let mut cards = parse_cards(input).map(|card| (card, 1)).collect::<Vec<_>>();
    for idx in 0..cards.len() {
        let (card, copies) = &cards[idx];
        let copies = *copies;
        let matches = card.count_matches();
        if matches == 0 {
            continue;
        }
        cards[idx + 1..idx + 1 + matches]
            .iter_mut()
            .for_each(|(_, count)| *count += copies);
    }
    cards.into_iter().map(|(_, count)| count).sum()
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Card {
    id: usize,
    winning_nums: HashSet<u32>,
    owned_nums: HashSet<u32>,
}

impl Card {
    fn count_matches(&self) -> usize {
        self.winning_nums.intersection(&self.owned_nums).count()
    }

    fn points(&self) -> u32 {
        let matches = self.count_matches() as u32;
        match matches {
            0 => 0,
            matches => 2_u32.pow(matches - 1),
        }
    }
}

fn parse_cards(input: &str) -> impl Iterator<Item = Card> + '_ {
    input.lines().map(|line| {
        let (winning, owned) = line.split_once(" | ").unwrap();
        let (id, winning) = winning.split_once(": ").unwrap();
        let id = id.trim_start_matches("Card").trim_start().parse().unwrap();
        let winning_nums = winning
            .split_ascii_whitespace()
            .map(|num| num.parse().unwrap())
            .collect();
        let owned_nums = owned
            .split_ascii_whitespace()
            .map(|num| num.parse().unwrap())
            .collect();
        Card {
            id,
            winning_nums,
            owned_nums,
        }
    })
}
