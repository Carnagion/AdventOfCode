use std::{collections::HashMap, thread};

pub fn part_one(input: &str) -> u64 {
    let (instrs, network) = parse_map(input);
    follow_map(instrs, &network, "AAA")
}

pub fn part_two(input: &str) -> u64 {
    let (instrs, network) = parse_map(input);
    thread::scope(|scope| {
        network
            .0
            .keys()
            .filter(|key| key.ends_with('A'))
            .map(|start| scope.spawn(|| follow_map(instrs.clone(), &network, start)))
            .fold(Vec::new(), |mut counts, handle| {
                counts.push(handle.join().unwrap());
                counts
            })
    })
    .into_iter()
    .reduce(lcm)
    .unwrap()
}

pub fn part_two_unthreaded(input: &str) -> u64 {
    let (instrs, network) = parse_map(input);
    network
        .0
        .keys()
        .filter(|key| key.starts_with('A'))
        .map(|start| follow_map(instrs.clone(), &network, start))
        .reduce(lcm)
        .unwrap()
}

fn follow_map<'inp, I>(mut instrs: I, network: &Network<'inp>, start: &'inp str) -> u64
where
    I: Iterator<Item = Instr>,
{
    let mut current = start;
    let mut count = 0;
    while let Some(instr) = instrs.next() {
        match current {
            zzz if zzz.ends_with('Z') => break,
            node => {
                current = network.next(node, instr);
                count += 1;
            },
        }
    }
    count
}

fn lcm(a: u64, b: u64) -> u64 {
    (a * b) / gcd(a, b)
}

fn gcd(a: u64, b: u64) -> u64 {
    match a {
        0 => b,
        a => gcd(b % a, a),
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Instr {
    Left,
    Right,
}

impl Instr {
    fn parse(instr: u8) -> Self {
        match instr {
            b'L' => Self::Left,
            b'R' => Self::Right,
            _ => panic!("invalid instruction"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Network<'inp>(HashMap<&'inp str, Node<'inp>>);

impl<'inp> Network<'inp> {
    fn next(&self, node: &'inp str, instr: Instr) -> &'inp str {
        let next = &self.0[node];
        match instr {
            Instr::Left => next.left,
            Instr::Right => next.right,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Node<'inp> {
    left: &'inp str,
    right: &'inp str,
}

impl<'inp> Node<'inp> {
    fn parse(input: &'inp str) -> Self {
        let (left, right) = input.split_once(", ").unwrap();
        Self {
            left: left.trim_start_matches('('),
            right: right.trim_end_matches(')'),
        }
    }
}

fn parse_map<'inp>(
    input: &'inp str,
) -> (impl Iterator<Item = Instr> + Clone + 'inp, Network<'inp>) {
    let (instrs, network) = input.split_once("\n\n").unwrap();
    let instrs = instrs.bytes().map(Instr::parse).cycle();
    let network = network
        .lines()
        .map(|line| {
            let (name, node) = line.split_once(" = ").unwrap();
            (name, Node::parse(node))
        })
        .collect();
    (instrs, Network(network))
}
