use std::fs;
use std::iter;


fn char2int (c: char) -> i32 {
    match c {
        '(' => 1,
        ')' => -1,
        _ => 0,
    }
}

fn solve (s: &mut String) -> (i32, i32) {
    let mut xs = s.drain(..);

    // map function and accumulate results: [x0, x0+x1, ..., x0+x1+...+xN-1]
    // note that the base value (0) is the first one
    let vs: Vec<i32> = iter::successors(
        Some(0), |acc| xs.next().map(|c| *acc + char2int(c))
    ).collect();

    // last element == sum
    let res1: i32 = vs[vs.len() - 1];

    // first index of "basement" (-1)
    let res2: i32 = vs.iter().position(|&x| x == -1).unwrap() as i32;

    return (res1, res2);
}

fn main () {
    let mut s: String = fs::read_to_string("in.txt").expect("DNE");

    let (res1, res2) = solve(&mut s);

    println!("P1: {}\nP2: {}", res1, res2);
}
