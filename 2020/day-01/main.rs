use std::env;
use std::fs;

fn solve1(xs: &Vec<i32>) -> Option<i32> {
    // sort input => O(NlogN)
    // for each element x, binary search 2020 - x => O(NlogN)
    let n = xs.len();
    let mut sxs = xs.clone();
    sxs.sort();

    let mut ans: Option<i32> = None;

    for i in 0..n {
        match sxs.binary_search(&(2020 - sxs[i])) {
            Ok(j) => {
                if i != j {
                    ans = Some(sxs[i] * sxs[j]);
                    break;
                }
            }
            Err(_) => continue,
        }
    }

    return ans;
}

fn solve2(xs: &Vec<i32>) -> Option<i32> {
    // same idea as for part 1, only that an extra loop is needed
    let n = xs.len();
    let mut sxs = xs.clone();
    sxs.sort();

    let mut ans: Option<i32> = None;

    for i in 0..n {
        for j in (i + 1)..n {
            match sxs.binary_search(&(2020 - sxs[i] - sxs[j])) {
                Ok(k) => {
                    if k != j {
                        ans = Some(sxs[i] * sxs[j] * sxs[k]);
                        break;
                    }
                }
                Err(_) => continue,
            }
        }
    }

    return ans;
}

fn main() {
    let input_file = env::args().nth(1).expect("Error with input file!");
    let input = fs::read_to_string(&input_file).unwrap();
    let xs: Vec<i32> = input
        .split('\n')
        .filter(|x| !x.is_empty())
        .map(|x| x.parse::<i32>().unwrap())
        .collect();

    let ans1 = solve1(&xs).expect("No solution found!");
    let ans2 = solve2(&xs).expect("No solution found!");

    println!("Part 1: {}", ans1);
    println!("Part 2: {}", ans2);
}
