pub fn part_one(input: &str) -> u64 {
    let mut lines = input.lines();
    let lines = lines.by_ref();

    let seeds = parse_seeds(lines);

    let almanac = parse_almanac(&mut lines.skip(1));

    seeds
        .into_iter()
        .map(|seed| almanac.map(seed))
        .min()
        .unwrap()
}

pub fn part_two(input: &str) -> u64 {
    let mut lines = input.lines();
    let lines = lines.by_ref();

    let seeds = parse_seeds(lines);

    let seeds_ranges = seeds
        .chunks(2)
        .map(|seeds| {
            let start = seeds[0];
            let len = seeds[1];
            start..start + len
        })
        .collect::<Vec<_>>();

    let mut almanac = parse_almanac(&mut lines.skip(1));
    almanac.invert();

    (0..)
        .find(|&loc| {
            let seed = almanac.map(loc);
            seeds_ranges
                .iter()
                .any(|seed_range| seed_range.contains(&seed))
        })
        .unwrap()
}

fn parse_seeds<'ln, L>(lines: &mut L) -> Vec<u64>
where
    L: Iterator<Item = &'ln str>,
{
    lines
        .next()
        .and_then(|seeds| seeds.strip_prefix("seeds: "))
        .unwrap()
        .split_ascii_whitespace()
        .map(|seed| seed.parse().unwrap())
        .collect::<Vec<_>>()
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Almanac([Vec<Mapping>; 7]);

impl Almanac {
    fn map(&self, seed: u64) -> u64 {
        self.0.iter().fold(seed, |src, mappings| {
            mappings
                .iter()
                .find_map(|mapping| mapping.map(src))
                .unwrap_or(src)
        })
    }

    fn invert(&mut self) {
        self.0.reverse();
        for mappings in &mut self.0 {
            mappings.reverse();
            mappings.iter_mut().for_each(Mapping::invert);
        }
    }
}

fn parse_almanac<'ln, L>(lines: &mut L) -> Almanac
where
    L: Iterator<Item = &'ln str>,
{
    let seed_soil = parse_mappings(lines);
    let soil_fert = parse_mappings(lines);
    let fert_water = parse_mappings(lines);
    let water_light = parse_mappings(lines);
    let light_temp = parse_mappings(lines);
    let temp_humid = parse_mappings(lines);
    let humid_loc = parse_mappings(lines);

    Almanac([
        seed_soil,
        soil_fert,
        fert_water,
        water_light,
        light_temp,
        temp_humid,
        humid_loc,
    ])
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Mapping {
    src: u64,
    dest: u64,
    len: u64,
}

impl Mapping {
    fn contains(&self, src: u64) -> bool {
        (self.src..self.src + self.len).contains(&src)
    }

    fn map(&self, src: u64) -> Option<u64> {
        self.contains(src).then(|| self.map_unchecked(src))
    }

    fn map_unchecked(&self, src: u64) -> u64 {
        src - self.src + self.dest
    }

    fn invert(&mut self) {
        let Self { src, dest, .. } = *self;
        self.src = dest;
        self.dest = src;
    }
}

fn parse_mappings<'ln, L>(lines: &mut L) -> Vec<Mapping>
where
    L: Iterator<Item = &'ln str>,
{
    lines
        .skip(1)
        .take_while(|line| !line.is_empty())
        .map(|line| {
            let mut nums = line
                .split_ascii_whitespace()
                .map(|num| num.parse().unwrap());
            let (dest, src, len) = (
                nums.next().unwrap(),
                nums.next().unwrap(),
                nums.next().unwrap(),
            );
            Mapping { src, dest, len }
        })
        .collect::<Vec<_>>()
}
