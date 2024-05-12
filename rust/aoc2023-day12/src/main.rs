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
        let mut P = Array2::<u64>::zeros((self.parts.len(), self.sequences.len() + 1).f());

        dbg!(&self.parts);
        dbg!(&self.sequences);

        for i in 0..self.parts.len() {
            P[[i, 0]] = 1;
        }

        let mut position_fixed = false;
        let mut last_fixed_pos = 0;

        for j in 1..=self.sequences.len() {

            let mut start_i = 0;
            if position_fixed {
                start_i = last_fixed_pos+1;
            }

            position_fixed = false;
            last_fixed_pos = 0;

            for i in start_i..self.parts.len() {

                if position_fixed {
                    P[[i,j]] = P[[i-1, j]];
                    continue;
                }

                let cur_len = self.sequences[j - 1] as usize;

                let min_index: usize = (self.sequences.iter().take(j).sum::<u64>() + j as u64 - 1 - 1).try_into().unwrap();
                if i < min_index {
                    continue;
                }

                let mut can_place = true;

                if i + 1 < self.parts.len() && self.parts.chars().nth(i + 1).unwrap() == '#' {
                    can_place = false; // sequence would be longer than required
                }

                if i >= cur_len && self.parts.chars().nth(i - cur_len).unwrap() == '#' {
                    can_place = false; // sequence would be longer than required
                }

                if self.parts[((i + 1) - cur_len)..=i].contains('.') {
                    can_place = false;
                }

                if can_place && j==self.sequences.len() && self.parts[(i+1)..].find('#').is_some() {
                    can_place = false;  // this is the last sequence, must use all remaining #'s
                }

                P[[i, j]] = if i > 0 {
                    P[[i.checked_sub(1).unwrap(), j]]
                } else {
                    0
                };

                if can_place {
                    if let Some(prev) = i.checked_sub(cur_len+1) {
                        P[[i, j]] += P[[prev, j.checked_sub(1).unwrap()]];
                    } else {
                        P[[i, j]] += 1;
                    }

                    if self.parts[((i + 1) - cur_len)..=i].chars().filter(|c| *c=='#').count() == cur_len {
                        println!("Fixing position at {i}, {j}");
                        position_fixed = true; // this is the only possible placement for j
                        last_fixed_pos = i;
                    }

                }
            }
        }


        for i in 0..self.parts.len() {
            print!("\n{}: ", &i);
            for j in 0..self.sequences.len() + 1 {
                print!("{:2} |", P[[i, j]]);
            }
        }
        println!("\n\n");


        P[[self.parts.len() - 1, self.sequences.len()]]
    }
}

fn parse(file_path: &str) -> Vec<Problem> {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);

    reader.lines().map(|line| match line.unwrap().split_once(' ') {
        Some((parts, sequences)) => Problem {
            parts: parts.to_string(),
            sequences: sequences.split(',').map(|x| x.parse::<u64>().unwrap()).collect(),
        },
        None => panic!("Incorrect input."),
    }).collect()
}

fn process(file_path: &str, part_two: bool) -> i64 {
    let problems = parse(file_path);

    // let solutions: Vec<_> = problems.iter().rev().take(13).map(|p| p.solve()).collect();
    let solutions: Vec<_> = problems.iter().map(|p| p.solve()).collect();

    dbg!(&solutions);

    solutions.iter().sum::<u64>() as i64
}


fn main() {
    let test_file = "test2.txt";
    let input_file = "input.txt";

    // println!("{}", process(test_file, false));
    println!("{}", process(input_file, false));

    // println!("{}", process(test_file, true));
    // println!("{}", process(input_file, true));
}
