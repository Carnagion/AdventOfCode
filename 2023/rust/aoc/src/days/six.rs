pub fn part_one(input: &str) -> u64 {
    parse_races(input).map(Race::wins).product()
}

pub fn part_two(input: &str) -> u64 {
    parse_races(input)
        .fold(Race { time: 0, record: 0 }, |acc, race| {
            let time = acc.time * 10_u64.pow(race.time.ilog10() + 1) + race.time;
            let record = acc.record * 10_u64.pow(race.record.ilog10() + 1) + race.record;
            Race { time, record }
        })
        .wins()
}

fn parse_races(input: &str) -> impl Iterator<Item = Race> + '_ {
    let mut lines = input.lines().map(|line| {
        line.split_ascii_whitespace()
            .skip(1)
            .map(|num| num.parse().unwrap())
    });
    let times = lines.next().unwrap();
    let distances = lines.next().unwrap();
    times.zip(distances).map(|(time, distance)| Race {
        time,
        record: distance,
    })
}

#[derive(Debug)]
struct Race {
    time: u64,
    record: u64,
}

impl Race {
    fn wins(self) -> u64 {
        (0..=self.time)
            .map(|charge| charge * (self.time - charge))
            .skip_while(|&dist| dist <= self.record)
            .take_while(|&dist| dist > self.record)
            .count() as u64
    }
}
