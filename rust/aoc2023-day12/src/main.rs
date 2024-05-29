use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::repeat;

use ndarray::{Array2, ShapeBuilder};

struct Problem {
    parts: String,
    sequences: Vec<u64>,
}

impl Problem {
    fn solve(&self) -> u64 {
        let mut P = Array2::<u64>::zeros((self.parts.len(), self.sequences.len() + 1).f());

        // dbg!(&self.parts);
        // dbg!(&self.sequences);

        for i in 0..self.parts.len() {
            P[[i, 0]] = 1;
        }

        for j in 1..=self.sequences.len() {
            for i in 0..self.parts.len() {
                let iu64 = i as u64;
                let ju64 = j as u64;

                let cur_len = self.sequences[j - 1] as usize;

                let min_index: u64 = self.sequences.iter().take(j).sum::<u64>() + ju64 - 1 - 1;
                if iu64 < min_index {
                    continue;
                }

                let mut can_place = true;

                if i >= cur_len && self.parts.chars().nth(i - cur_len).unwrap() == '#' {
                    can_place = false;
                }

                if self.parts.chars().nth(i + 1).unwrap_or('.') == '#' {
                    can_place = false;
                }

                if j == 1 && i >= cur_len && self.parts[0..=i - cur_len].contains('#') {
                    can_place = false; // cannot leave #'s left of first sequence
                }

                if j == self.sequences.len() && self.parts[(i + 1)..].contains('#') {
                    can_place = false;  // cannot have #'s right of last sequence
                }

                if self.parts[((i + 1) - cur_len)..=i].contains('.') {
                    can_place = false;
                }

                let next: u64 = if can_place {
                    if i < cur_len + 1 {
                        1
                    } else {
                        P[[i - cur_len - 1, j - 1]]
                    }
                } else {
                    0
                };

                let prev = if i == 0 { 0 } else { P[[i - 1, j]] };

                P[[i, j]] = if self.parts.chars().nth(i).unwrap() == '#' {
                    next
                } else {
                    next + prev
                };
            }
        }


        // for i in 0..self.parts.len() {
        //     print!("\n{}: ", &i);
        //     for j in 0..self.sequences.len() + 1 {
        //         print!("{:2} |", P[[i, j]]);
        //     }
        // }
        // println!("\n\n");

        P[[self.parts.len() - 1, self.sequences.len()]]
    }
}

fn parse(file_path: &str, part_two: bool) -> Vec<Problem> {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);

    reader.lines().map(|line| match line.unwrap().split_once(' ') {
        Some((parts, sequences)) => if part_two {
            Problem {
                parts: repeat(parts).take(5).collect::<Vec<_>>().join("?"),
                sequences: repeat(sequences).take(5).collect::<Vec<_>>().join(",").split(',').map(|x| x.parse::<u64>().unwrap()).collect(),
            }
        } else {
            Problem {
                parts: parts.to_string(),
                sequences: sequences.split(',').map(|x| x.parse::<u64>().unwrap()).collect(),
            }
        },
        None => panic!("Incorrect input."),
    }).collect()
}

fn process(file_path: &str, part_two: bool) -> i64 {
    let problems = parse(file_path, part_two);
    let solutions: Vec<_> = problems.iter().map(|p| p.solve()).collect();
    solutions.iter().sum::<u64>() as i64
}


fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false));
    println!("{}", process(input_file, false));

    println!("{}", process(test_file, true));
    println!("{}", process(input_file, true));
}
