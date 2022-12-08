use std::collections::HashMap;
use std::io::{self, BufRead};

/*
 * Upsolving, following observations from main `solve.py`:
 * - don't actually care about `$ ls`, because any output (i.e. no cd) is just a listing
 * - cd manipulates the stack
 * - last dir in stack is cwd
 * - populate parent dir sizes as we go
 */

fn populate_dir2size(lines: Vec<String>) -> HashMap<String, usize> {
    let mut stack: Vec<String> = Vec::new();
    let mut dir2size: HashMap<String, usize> = HashMap::new();

    for line in lines {
        let cmd: Vec<&str> = line.split_whitespace().collect();

        match cmd.as_slice() {
            // cd handles the stack
            ["$", "cd", "/"] => stack = vec![String::from("/")],
            ["$", "cd", ".."] => _ = stack.pop().unwrap(),
            ["$", "cd", dir] => stack.push(format!("{}/", dir)),

            // don't care about `$ ls`, as any non-cmd is just a listing
            ["$", "ls"] => (),

            // don't care about dirs we don't cd into, as their size is 0
            ["dir", _] => (),

            // regular file: increase sizes of parent dirs
            [size, _] => {
                // accumulate all parent dirs,
                // e.g. from ["/", "a/", "b/"] -> ["/", "/a/", "/a/b/"]
                let paths = stack.iter().scan(String::new(), |acc, x| {
                    acc.push_str(&x);
                    Some(acc.to_string())
                });

                for path in paths {
                    *dir2size.entry(path).or_insert(0) += size.parse::<usize>().unwrap();
                }
            }

            _ => eprintln!("UNK {:?}", cmd),
        }
    }

    dir2size
}

fn part1(dir2size: &HashMap<String, usize>) -> usize {
    // we can also use a conditional inside a fold, but it's a bit more verbose
    dir2size.values().filter(|size| **size <= 100000).sum()
}

fn part2(dir2size: &HashMap<String, usize>) -> usize {
    let size_limit: usize = 70000000 - 30000000; // max size - need unused
    let total_used: usize = *dir2size.get("/").unwrap();

    assert!(total_used > size_limit);

    *dir2size
        .values()
        .filter(|size| total_used - **size <= size_limit)
        .min()
        .unwrap()
}

fn main() {
    let lines: Vec<String> = io::stdin().lock().lines().map(|l| l.unwrap()).collect();

    let dir2size = populate_dir2size(lines);

    println!("Part 1: {}", part1(&dir2size));
    println!("Part 2: {}", part2(&dir2size));
}

#[cfg(test)]
mod tests {

    use super::{part1, part2, populate_dir2size};
    use std::fs;

    #[test]
    fn test_sample() {
        // read lines from the file named "sample"
        let lines = fs::read_to_string("sample")
            .unwrap()
            .lines()
            .map(|l| l.to_string())
            .collect();

        let dir2size = populate_dir2size(lines);

        assert_eq!(part1(&dir2size), 95437);
        assert_eq!(part2(&dir2size), 24933642);
    }
}
