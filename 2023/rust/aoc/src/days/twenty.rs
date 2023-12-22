use std::collections::{HashMap, VecDeque};

pub fn part_one(input: &str) -> u64 {
    let mut modules = Modules::parse(input);
    let (highs, lows) = (0..1000)
        .map(|_| highs_lows(&modules.press_button()))
        .fold((0, 0), |(highs, lows), (high, low)| {
            (highs + high, lows + low)
        });
    highs * lows
}

pub fn part_two(input: &str) -> u64 {
    let modules = Modules::parse(input);
    modules
        .find_rx_inputs()
        .map(|input| {
            println!("{}", input);
            let mut modules = modules.clone();
            modules.press_button_until(|trans| {
                trans
                    .iter()
                    .any(|trans| trans.source == input && trans.pulse == Pulse::High)
            })
        })
        .reduce(lcm)
        .unwrap()
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Transmission<'a> {
    source: &'a str,
    pulse: Pulse,
    dest: &'a str,
}

fn highs_lows(transmissions: &[Transmission<'_>]) -> (u64, u64) {
    transmissions
        .iter()
        .fold((0, 0), |(highs, lows), trans| match trans.pulse {
            Pulse::High => (highs + 1, lows),
            Pulse::Low => (highs, lows + 1),
        })
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Modules<'a>(HashMap<&'a str, Module<'a>>);

impl<'a> Modules<'a> {
    fn parse(input: &'a str) -> Self {
        let mut modules = input.lines().map(Module::parse).collect::<HashMap<_, _>>();
        let cons = modules
            .iter()
            .filter_map(|(&name, module)| match &module.kind {
                ModuleKind::Conjunction { .. } => Some(name),
                _ => None,
            })
            .collect::<Vec<_>>();
        for con in cons {
            let inputs = modules
                .iter()
                .filter_map(|(&name, module)| module.destinations.contains(&con).then_some(name))
                .collect::<Vec<_>>();
            let Some(ModuleKind::Conjunction { memory }) =
                modules.get_mut(con).map(|module| &mut module.kind)
            else {
                unreachable!()
            };
            memory.extend(inputs.into_iter().map(|input| (input, Pulse::Low)));
        }
        Self(modules)
    }

    fn press_button(&mut self) -> Vec<Transmission> {
        let mut transmissions = VecDeque::from_iter([Transmission {
            source: "button",
            pulse: Pulse::Low,
            dest: "broadcaster",
        }]);

        let mut sent_transmissions = Vec::new();

        while let Some(transmission) = transmissions.pop_front() {
            sent_transmissions.push(transmission);

            let Some(module) = self.0.get_mut(transmission.dest) else {
                continue;
            };

            let Some(pulse) = module.relay(transmission.pulse, transmission.source) else {
                continue;
            };

            transmissions.extend(module.destinations.iter().map(|dest| Transmission {
                source: transmission.dest,
                pulse,
                dest,
            }));
        }

        sent_transmissions
    }

    fn press_button_until<F>(&mut self, stop: F) -> u64
    where
        F: Fn(&[Transmission<'_>]) -> bool,
    {
        let mut count = 0;
        loop {
            let transmissions = self.press_button();
            count += 1;
            if stop(&transmissions) {
                break count;
            }
        }
    }

    fn find_rx_inputs(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.0
            .iter()
            .filter_map(|(&name, module)| module.destinations.contains(&"rx").then_some(name))
            .flat_map(|input| {
                self.0.iter().filter_map(move |(&name, module)| {
                    module.destinations.contains(&input).then_some(name)
                })
            })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Module<'a> {
    kind: ModuleKind<'a>,
    destinations: Vec<&'a str>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum ModuleKind<'a> {
    Broadcaster,
    FlipFlop { on: bool },
    Conjunction { memory: HashMap<&'a str, Pulse> },
}

impl<'a> Module<'a> {
    fn parse(input: &'a str) -> (&'a str, Self) {
        let (name, destinations) = input.split_once(" -> ").unwrap();
        let (kind, name) = match name.as_bytes() {
            b"broadcaster" => (ModuleKind::Broadcaster, name),
            [b'%', ..] => (
                ModuleKind::FlipFlop { on: false },
                name.trim_start_matches('%'),
            ),
            [b'&', ..] => (
                ModuleKind::Conjunction {
                    memory: HashMap::new(),
                },
                name.trim_start_matches('&'),
            ),
            _ => panic!("invalid module kind"),
        };
        let destinations = destinations.split(", ").collect();
        (name, Self { kind, destinations })
    }

    fn relay(&mut self, pulse: Pulse, source: &'a str) -> Option<Pulse> {
        match (&mut self.kind, pulse) {
            (ModuleKind::Broadcaster, pulse) => Some(pulse),
            (ModuleKind::FlipFlop { .. }, Pulse::High) => None,
            (ModuleKind::FlipFlop { on }, Pulse::Low) => {
                *on = !*on;
                let pulse = if *on { Pulse::High } else { Pulse::Low };
                Some(pulse)
            },
            (ModuleKind::Conjunction { memory }, pulse) => {
                memory
                    .entry(source)
                    .and_modify(|memory| *memory = pulse)
                    .or_insert(pulse);
                let pulse = if memory.values().all(|&pulse| pulse == Pulse::High) {
                    Pulse::Low
                } else {
                    Pulse::High
                };
                Some(pulse)
            },
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Pulse {
    High,
    Low,
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
