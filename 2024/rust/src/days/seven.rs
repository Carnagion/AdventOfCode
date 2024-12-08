pub fn part_one(input: &str) -> u64 {
    solve(input, false)
}

pub fn part_two(input: &str) -> u64 {
    solve(input, true)
}

fn solve(input: &str, part_two: bool) -> u64 {
    input
        .lines()
        .filter_map(|line| {
            let (target, nums) = line.split_once(':').unwrap();
            let target = target.parse().unwrap();
            let nums = nums.split_ascii_whitespace().map(|n| n.parse().unwrap());
            ops_can_create(nums, target, part_two).then_some(target)
        })
        .sum()
}

fn ops_can_create<I>(nums: I, target: u64, part_two: bool) -> bool
where
    I: IntoIterator<Item = u64, IntoIter: Clone>,
{
    let nums = nums.into_iter();
    ops_can_create_acc(nums, target, 0, part_two)
}

fn ops_can_create_acc<I>(mut nums: I, target: u64, acc: u64, part_two: bool) -> bool
where
    I: Iterator<Item = u64> + Clone,
{
    match nums.next() {
        None => acc == target,
        Some(num) => {
            ops_can_create_acc(nums.clone(), target, acc + num, part_two)
                || ops_can_create_acc(nums.clone(), target, acc.max(1) * num, part_two)
                || (part_two && {
                    let concat = acc * 10_u64.pow(num.ilog(10) + 1) + num;
                    ops_can_create_acc(nums, target, concat, part_two)
                })
        },
    }
}
