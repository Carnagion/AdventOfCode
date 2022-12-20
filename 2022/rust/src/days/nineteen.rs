use std::collections::HashSet;

use regex::Regex;

pub fn solve(part: u8, input: String) -> String {
    match part {
        1 => {
            format!("{}", input.lines()
                .filter_map(parse_blueprint)
                .map(|blueprint| blueprint.id * most_openable_geodes(blueprint, 24))
                .sum::<u32>())
        },
        2 => {
            format!("{}", input.lines()
                .filter_map(parse_blueprint)
                .take(3)
                .map(|blueprint| most_openable_geodes(blueprint, 32))
                .product::<u32>())
        },
        _ => String::from("Invalid part"),
    }
}

struct Blueprint {
    id: u32,
    ore_robot_cost: u32,
    clay_robot_cost: u32,
    obsidian_robot_cost: (u32, u32),
    geode_robot_cost: (u32, u32),
}

#[derive(Eq, Hash, PartialEq)]
struct State {
    ore_count: u32,
    clay_count: u32,
    obsidian_count: u32,
    geode_count: u32,
    ore_robots: u32,
    clay_robots: u32,
    obsidian_robots: u32,
    geode_robots: u32,
}

impl State {
    fn new(ore_count: u32, clay_count: u32, obsidian_count: u32, geode_count: u32, ore_robots: u32, clay_robots: u32, obsidian_robots: u32, geode_robots: u32) -> Self {
        Self {
            ore_count,
            clay_count,
            obsidian_count,
            geode_count,
            ore_robots,
            clay_robots,
            obsidian_robots,
            geode_robots,
        }
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new(0, 0, 0, 0, 1, 0, 0, 0)
    }
}

fn parse_blueprint(input: &str) -> Option<Blueprint> {
    let blueprint_regex = Regex::new(r#"Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\."#)
        .ok()?;
    let captures = blueprint_regex.captures(input)?;
    Some(Blueprint {
        id: captures[1].parse().ok()?,
        ore_robot_cost: captures[2].parse().ok()?,
        clay_robot_cost: captures[3].parse().ok()?,
        obsidian_robot_cost: (captures[4].parse().ok()?, captures[5].parse().ok()?),
        geode_robot_cost: (captures[6].parse().ok()?, captures[7].parse().ok()?),
    })
}

fn most_openable_geodes(blueprint: Blueprint, limit: u32) -> u32 {
    let mut highest = 0;

    // Initial state with just one ore robot
    let mut states = HashSet::new();
    states.insert(State::default());

    // Go through each state every minute
    for minute in 0..limit {
        highest = 0;
        let mut updated = HashSet::new();
        for state in &states {
            // Increment all resources
            let ore_count = state.ore_count + state.ore_robots;
            let clay_count = state.clay_count + state.clay_robots;
            let obsidian_count = state.obsidian_count + state.obsidian_robots;
            let geode_count = state.geode_count + state.geode_robots;

            // Update maximum geode value
            highest = highest.max(geode_count);

            // Buy no new robots
            updated.insert(State::new(ore_count,
                                      clay_count,
                                      obsidian_count,
                                      geode_count,
                                      state.ore_robots,
                                      state.clay_robots,
                                      state.obsidian_robots,
                                      state.geode_robots));

            // Buy a new ore robot
            if state.ore_count >= blueprint.ore_robot_cost {
                updated.insert(State::new(ore_count - blueprint.ore_robot_cost,
                                          clay_count,
                                          obsidian_count,
                                          geode_count,
                                          state.ore_robots + 1,
                                          state.clay_robots,
                                          state.obsidian_robots,
                                          state.geode_robots));
            }

            // Buy a new clay robot
            if state.ore_count >= blueprint.clay_robot_cost {
                updated.insert(State::new(ore_count - blueprint.clay_robot_cost,
                                          clay_count,
                                          obsidian_count,
                                          geode_count,
                                          state.ore_robots,
                                          state.clay_robots + 1,
                                          state.obsidian_robots,
                                          state.geode_robots));
            }

            // Buy a new obsidian robot
            let (obsidian_ore_cost, obsidian_clay_cost) = blueprint.obsidian_robot_cost;
            if state.ore_count >= obsidian_ore_cost && state.clay_count >= obsidian_clay_cost {
                updated.insert(State::new(ore_count - obsidian_ore_cost,
                                          clay_count - obsidian_clay_cost,
                                          obsidian_count,
                                          geode_count,
                                          state.ore_robots,
                                          state.clay_robots,
                                          state.obsidian_robots + 1,
                                          state.geode_robots));
            }

            // Buy a new geode robot
            let (geode_ore_cost, geode_obsidian_cost) = blueprint.geode_robot_cost;
            if state.ore_count >= geode_ore_cost && state.obsidian_count >= geode_obsidian_cost {
                updated.insert(State::new(ore_count - geode_ore_cost,
                                          clay_count,
                                          obsidian_count - geode_obsidian_cost,
                                          geode_count,
                                          state.ore_robots,
                                          state.clay_robots,
                                          state.obsidian_robots,
                                          state.geode_robots + 1));
            }
        }

        // Reset old states
        states.clear();

        // Add new states to old states
        if minute < limit - 1 {
            states.extend(updated
                .into_iter()
                .filter(|state| state.geode_count + limit - minute > highest));
        }

        dbg!(states.len());
    }

    return highest;
}