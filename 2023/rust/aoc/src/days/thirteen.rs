pub fn part_one(input: &str) -> usize {
    input
        .split("\n\n")
        .map(Pattern::parse)
        .map(|pat| pat.reflection_score(0))
        .sum()
}

pub fn part_two(input: &str) -> usize {
    input
        .split("\n\n")
        .map(Pattern::parse)
        .map(|pat| pat.reflection_score(1))
        .sum()
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Pattern(Vec<Vec<u8>>);

impl Pattern {
    fn parse(input: &str) -> Self {
        Self(input.lines().map(|line| line.as_bytes().to_vec()).collect())
    }

    fn reflection_score(&self, smudges: usize) -> usize {
        self.horizontal_reflection_point(smudges)
            .map(|h| h * 100)
            .unwrap_or_else(|| {
                self.transpose()
                    .horizontal_reflection_point(smudges)
                    .unwrap()
            })
    }

    fn horizontal_reflection_point(&self, smudges: usize) -> Option<usize> {
        (1..self.0.len()).find(|&refl_point| {
            let before = &self.0[..refl_point];
            let after = &self.0[refl_point..];
            before
                .iter()
                .rev()
                .zip(after)
                .map(|(before, after)| {
                    before
                        .iter()
                        .zip(after)
                        .filter(|(before, after)| before != after)
                        .count()
                })
                .sum::<usize>()
                == smudges
        })
    }

    fn transpose(&self) -> Self {
        let pattern = (0..self.0[0].len())
            .map(|col| self.0.iter().map(|row| row[col]).collect())
            .collect();
        Self(pattern)
    }
}
