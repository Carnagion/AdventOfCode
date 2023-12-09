use std::{array, cmp::Ordering};

pub fn part_one(input: &str) -> u32 {
    let mut hand_bids = parse_hand_bids(input);
    hand_bids.sort_unstable_by_key(|&(hand, _)| hand);
    hand_bids
        .into_iter()
        .map(|(_, bid)| bid)
        .zip(1..)
        .map(|(bid, rank)| bid * rank)
        .sum()
}

pub fn part_two(input: &str) -> u32 {
    let mut hand_bids = parse_hand_bids(input);
    hand_bids
        .iter_mut()
        .map(|(hand, _)| hand)
        .for_each(Hand::convert_jokers);
    hand_bids.sort_unstable_by_key(|&(hand, _)| hand);
    hand_bids
        .into_iter()
        .map(|(_, bid)| bid)
        .zip(1..)
        .map(|(bid, rank)| bid * rank)
        .sum()
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Card {
    Joker,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

impl Card {
    fn parse(input: u8) -> Self {
        match input {
            b'2' => Self::Two,
            b'3' => Self::Three,
            b'4' => Self::Four,
            b'5' => Self::Five,
            b'6' => Self::Six,
            b'7' => Self::Seven,
            b'8' => Self::Eight,
            b'9' => Self::Nine,
            b'T' => Self::Ten,
            b'J' => Self::Jack,
            b'Q' => Self::Queen,
            b'K' => Self::King,
            b'A' => Self::Ace,
            _ => panic!("invalid card"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Hand([Card; 5]);

impl Hand {
    fn ty(&self) -> HandType {
        let mut hand = self.0;
        hand.sort_unstable();

        let joker_count = match hand.iter().take_while(|&&card| card == Card::Joker).count() {
            5 => 0,
            count => count,
        };

        let mut card_counts = hand[joker_count..]
            .group_by(Card::eq)
            .map(<[Card]>::len)
            .collect::<Vec<_>>();
        card_counts.sort_unstable();

        if let Some(last_count) = card_counts.last_mut() {
            *last_count += joker_count;
        }

        match card_counts.as_slice() {
            [1, 1, 1, 1, 1] => HandType::HighCard,
            [1, 1, 1, 2] => HandType::OnePair,
            [1, 2, 2] => HandType::TwoPair,
            [1, 1, 3] => HandType::ThreeOfAKind,
            [2, 3] => HandType::FullHouse,
            [1, 4] => HandType::FourOfAKind,
            [5] => HandType::FiveOfAKind,
            _ => panic!("invalid hand"),
        }
    }

    fn parse(input: &str) -> Self {
        let mut cards = input.bytes().map(Card::parse);
        Self(array::from_fn(|_| cards.next().unwrap()))
    }

    fn convert_jokers(&mut self) {
        for card in &mut self.0 {
            if let Card::Jack = card {
                *card = Card::Joker;
            }
        }
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ty()
            .cmp(&other.ty())
            .then_with(|| self.0.cmp(&other.0))
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

fn parse_hand_bids(input: &str) -> Vec<(Hand, u32)> {
    input
        .lines()
        .map(|line| line.split_once(' ').unwrap())
        .map(|(hand, bid)| (Hand::parse(hand), bid.parse::<u32>().unwrap()))
        .collect()
}
