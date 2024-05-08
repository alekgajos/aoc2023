use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader};

use ndarray::{Array2, Shape, ShapeBuilder};

struct Problem {
    parts: String,
    sequences: Vec<u64>,
}

impl Problem {
    fn solve(&self) -> u64 {

        // let P: Array2<u64> = Array2::zeros((self.sequences.len(), self.parts.len()+1));
        let mut P = Array2::<u64>::zeros((self.sequences.len(), self.parts.len()+1).f());

        for j in 1..=self.sequences.len() {
            for i in 0..self.parts.len() {
                let x = P[[1, 1]];


            }


        }
        1
    }
}

fn parse(file_path: &str) -> Vec<Problem> {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);

    reader
        .lines()
        .map(|line| match line.unwrap().split_once(' ') {
            Some((parts, sequences)) => Problem {
                parts: parts.to_string(),
                sequences: sequences
                    .split(',')
                    .map(|x| x.parse::<u64>().unwrap())
                    .collect(),
            },
            None => panic!("Incorrect input."),
        })
        .collect()
}

fn process(file_path: &str, part_two: bool) -> i64 {
    let problems = parse(file_path);

    let solutions: Vec<_> = problems.iter().map(|p| p.solve()).collect();
    1
}


fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false));
    println!("{}", process(input_file, false));

    // println!("{}", process(test_file, true));
    // println!("{}", process(input_file, true));
}
