use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::num::ParseIntError;
use std::result::Result;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParsingError {
    #[error("Regex error")]
    RegexError(#[from] ParseIntError),
    #[error("IO Error")]
    IOError(#[from] io::Error),
}

type Sequence = Vec<i64>;

fn parse(file_path: &str) -> Result<Vec<Sequence>, ParsingError> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);

    Ok(reader
        .lines()
        .map(|line| {
            line.unwrap()
                .split_whitespace()
                .map(|token| token.parse::<i64>().unwrap())
                .collect()
        })
        .collect())
}

fn extrapolate(seq: Sequence, backward: bool) -> i64 {
    let mut derivatives: Vec<Sequence> = Vec::new();
    derivatives.push(seq);
    loop {
        let newseq: Vec<i64> = derivatives
            .last()
            .unwrap()
            .windows(2)
            .map(|pair| pair[1] - pair[0])
            .collect();
        derivatives.push(newseq);
        if derivatives
            .last()
            .unwrap()
            .iter()
            .filter(|&x| *x != 0)
            .count()
            == 0
        {
            break;
        }
    }

    if backward {
        let res: i64 = derivatives
            .into_iter()
            .flat_map(|seq| seq.first().copied())
            .rev()
            .reduce(|x, y| (y - x))
            .unwrap();
        res
    } else {
        derivatives.iter().flat_map(|seq| seq.last()).sum()
    }
}

fn process(file_path: &str, part_two: bool) -> io::Result<i64> {
    let sequences = parse(file_path).unwrap();
    let res = sequences
        .into_iter()
        .map(|x| extrapolate(x, part_two))
        .sum();
    Ok(res)
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false).unwrap());
    println!("{}", process(input_file, false).unwrap());

    println!("{}", process(test_file, true).unwrap());
    println!("{}", process(input_file, true).unwrap());
}
