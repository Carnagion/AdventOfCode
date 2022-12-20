use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

use regex::Regex;

pub fn solve(part: u8, input: String) -> String {
    match part {
        1 => {
            let mut flow_rates = HashMap::new();
            let mut tunnels = HashMap::new();
            for valve in input.lines()
                .map(|line| line.to_string())
                .filter_map(parse_valve) {
                tunnels.insert(valve.name.clone(), valve.tunnels);
                if valve.flow_rate > 0 {
                    flow_rates.insert(valve.name.clone(), valve.flow_rate);
                }
            }

            let mut visited = HashSet::new();
            let mut queue = VecDeque::new();
            queue.push_back(State {
                valve: String::from("AA"),
                time: 1,
                pressure: 0,
                flow_rates,
            });

            let mut max = 0;
            while let Some(state) = queue.pop_front() {
                if state.time == 30 || !visited.insert((state.valve.clone(), state.pressure.clone())) {
                    continue;
                }
                max = max.max(state.pressure);
                queue.extend(state.step(&tunnels));
            }
            format!("{}", max)
        },
        2 => {
            input
        },
        _ => String::from("Invalid part"),
    }
}

struct Valve {
    name: String,
    flow_rate: u32,
    tunnels: Vec<String>,
}

fn parse_valve(line: String) -> Option<Valve> {
    let pattern = Regex::new(r#"^Valve (..) has flow rate=(\d+); tunnel.? lead.? to valve.? (.+)$"#).unwrap();
    let captures = pattern.captures(&line)?;
    let (name, flow_rate, tunnels) = (captures[1].to_string(), captures[2].to_string().parse(), captures[3].split(", "));
    Some(Valve {
        name,
        flow_rate: flow_rate.ok()?,
        tunnels: tunnels.map(ToString::to_string).collect(),
    })
}

struct State {
    valve: String,
    time: u32,
    pressure: u32,
    flow_rates: HashMap<String, u32>,
}

impl State {
    fn step<'a>(&'a self, tunnels: &'a HashMap<String, Vec<String>>) -> impl Iterator<Item = State> + '_ {
        let open_valves = self.flow_rates.get(&self.valve)
            .into_iter()
            .map(|rate| {
                let mut flow_rates = self.flow_rates.clone();
                flow_rates.remove(&self.valve);
                State {
                    valve: self.valve.clone(),
                    time: self.time + 1,
                    pressure: self.pressure + rate * (30 - self.time),
                    flow_rates,
                }
            });
        let tunnel = tunnels.get(&self.valve)
            .into_iter()
            .flat_map(|tunnels| tunnels.iter()
                .map(|valve| State {
                    valve: valve.clone(),
                    time: self.time + 1,
                    pressure: self.pressure,
                    flow_rates: self.flow_rates.clone(),
                }));
        open_valves.chain(tunnel)
    }
}