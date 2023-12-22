use std::{array, collections::HashMap, str};

pub fn part_one(input: &str) -> u64 {
    let (workflows, parts) = input.split_once("\n\n").unwrap();
    let workflows = workflows.lines().map(Workflow::parse).collect();
    let parts = parts.lines().map(Part::parse);
    parts
        .filter_map(|part| part.process(&workflows).then_some(part.rating()))
        .sum()
}

pub fn part_two(input: &str) -> u64 {
    let (workflows, _) = input.split_once("\n\n").unwrap();
    let workflows = workflows.lines().map(Workflow::parse).collect();
    let ranges = array::from_fn(|_| Vec::from_iter(1..=4000));
    accepted_combinations(&workflows, RuleOutcome::SendTo(b"in"), ranges)
}

fn accepted_combinations(
    workflows: &HashMap<&[u8], Workflow<'_>>,
    outcome: RuleOutcome<'_>,
    mut ranges: [Vec<u64>; 4],
) -> u64 {
    match outcome {
        RuleOutcome::Accept => ranges
            .into_iter()
            .map(|range| range.len())
            .product::<usize>() as u64,
        RuleOutcome::Reject => 0,
        RuleOutcome::SendTo(next) => {
            let mut count = 0;
            let workflow = &workflows[next];
            for Rule { condition, outcome } in &workflow.rules {
                let idx = match condition.category {
                    Category::ExtremelyCool => 0,
                    Category::Musical => 1,
                    Category::Aerodynamic => 2,
                    Category::Shiny => 3,
                };

                let mut split_ranges = ranges.clone();
                (split_ranges[idx], ranges[idx]) =
                    ranges[idx].iter().partition(|&&val| match condition.op {
                        Op::Less => val < condition.value,
                        Op::Greater => val > condition.value,
                    });

                count += accepted_combinations(workflows, *outcome, split_ranges);
            }
            count + accepted_combinations(workflows, workflow.default, ranges)
        },
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Category {
    ExtremelyCool,
    Musical,
    Aerodynamic,
    Shiny,
}

impl Category {
    fn parse(cat: u8) -> Self {
        match cat {
            b'x' => Self::ExtremelyCool,
            b'm' => Self::Musical,
            b'a' => Self::Aerodynamic,
            b's' => Self::Shiny,
            _ => panic!("invalid category"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Part {
    x: u64,
    m: u64,
    a: u64,
    s: u64,
}

impl Part {
    fn parse(part: &str) -> Self {
        let mut parts = part
            .trim_matches(['{', '}'].as_slice())
            .split(',')
            .map(|part| part.split_once('=').unwrap().1.parse().unwrap());
        let [x, m, a, s] = parts.next_chunk().unwrap();
        Self { x, m, a, s }
    }

    fn process(&self, workflows: &HashMap<&[u8], Workflow<'_>>) -> bool {
        let mut workflow = b"in".as_slice();
        loop {
            let outcome = workflows[workflow].process(self);
            match outcome {
                RuleOutcome::Accept => break true,
                RuleOutcome::Reject => break false,
                RuleOutcome::SendTo(next) => {
                    workflow = next;
                    continue;
                },
            }
        }
    }

    fn rating(&self) -> u64 {
        self.x + self.m + self.a + self.s
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Workflow<'a> {
    rules: Vec<Rule<'a>>,
    default: RuleOutcome<'a>,
}

impl<'a> Workflow<'a> {
    fn parse(workflow: &'a str) -> (&'a [u8], Self) {
        let (name, rules) = workflow.split_once('{').unwrap();
        let mut rules = rules.trim_end_matches('}').split(',');
        let default = RuleOutcome::parse(rules.next_back().unwrap().as_bytes());
        let rules = rules.map(Rule::parse).collect();
        (name.as_bytes(), Self { rules, default })
    }

    fn process(&self, part: &Part) -> RuleOutcome<'a> {
        self.rules
            .iter()
            .find_map(|rule| rule.condition.check(part).then_some(rule.outcome))
            .unwrap_or(self.default)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Rule<'a> {
    condition: Condition,
    outcome: RuleOutcome<'a>,
}

impl<'a> Rule<'a> {
    fn parse(rule: &'a str) -> Self {
        let (condition, outcome) = rule.split_once(':').unwrap();
        let condition = Condition::parse(condition.as_bytes());
        let outcome = RuleOutcome::parse(outcome.as_bytes());
        Self { condition, outcome }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Condition {
    category: Category,
    op: Op,
    value: u64,
}

impl Condition {
    fn parse(condition: &[u8]) -> Self {
        let [category, op, value @ ..] = condition else {
            panic!("invalid condition");
        };
        let category = Category::parse(*category);
        let op = Op::parse(*op);
        let value = str::from_utf8(value).unwrap().parse().unwrap();
        Self {
            category,
            op,
            value,
        }
    }

    fn check(&self, part: &Part) -> bool {
        let lhs = match self.category {
            Category::ExtremelyCool => part.x,
            Category::Musical => part.m,
            Category::Aerodynamic => part.a,
            Category::Shiny => part.s,
        };
        self.op.check(lhs, self.value)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Op {
    Less,
    Greater,
}

impl Op {
    fn parse(op: u8) -> Self {
        match op {
            b'<' => Self::Less,
            b'>' => Self::Greater,
            _ => panic!("invalid operation"),
        }
    }

    fn check(self, lhs: u64, rhs: u64) -> bool {
        match self {
            Self::Less => lhs < rhs,
            Self::Greater => lhs > rhs,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum RuleOutcome<'a> {
    Accept,
    Reject,
    SendTo(&'a [u8]),
}

impl<'a> RuleOutcome<'a> {
    fn parse(outcome: &'a [u8]) -> Self {
        match outcome {
            b"A" => Self::Accept,
            b"R" => Self::Reject,
            workflow => Self::SendTo(workflow),
        }
    }
}
