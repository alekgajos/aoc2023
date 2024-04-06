use num::integer::lcm;
use regex::Regex;
use std::collections::HashMap;
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

#[derive(Debug)]
struct Node {
    left: String,
    right: String,
}

#[derive(Debug)]
struct Problem {
    nodes: HashMap<String, Node>,
    instructions: Instructions,
}

#[derive(Debug)]
struct Instructions {
    sequence: Vec<char>,
}

impl Instructions {
    pub fn iter(&self) -> InstructionIterator {
        InstructionIterator {
            data: self,
            position: -1,
        }
    }
}

struct InstructionIterator<'a> {
    data: &'a Instructions,
    position: i32,
}

impl<'a> Iterator for InstructionIterator<'a> {
    type Item = &'a char;
    fn next(&mut self) -> Option<Self::Item> {
        self.position = (self.position + 1) % self.data.sequence.len() as i32;
        Some(&self.data.sequence[self.position as usize])
    }
}

fn parse(file_path: &str) -> Result<Problem, ParsingError> {
    let file = File::open(file_path)?;
    let mut reader = BufReader::new(file);
    let mut buffer = String::new();

    let _ = reader.read_line(&mut buffer);
    let instructions: Vec<_> = buffer.trim().chars().collect();

    let instructions = Instructions {
        sequence: instructions,
    };

    let re = Regex::new(r"(?<source>\w\w\w) = \((?<left>\w\w\w), (?<right>\w\w\w)\)").unwrap();

    let mut dict = HashMap::new();

    reader.lines().for_each(|line| {
        let line = line.unwrap();

        if let Some(captures) = re.captures(&line) {
            let source = captures["source"].to_string().clone();
            let left = captures["left"].to_string();
            let right = captures["right"].to_string();

            dict.insert(source.to_owned(), Node { left, right });
        }
    });

    Ok(Problem {
        instructions,
        nodes: dict,
    })
}

fn walk_the_tree(
    start_label: &str,
    end_predicate: impl Fn(&str) -> bool,
    problem: &Problem,
) -> i64 {
    let mut counter: i64 = 0;
    let mut label = start_label;
    let mut instr_iter = problem.instructions.iter();

    loop {
        if end_predicate(label) {
            break;
        }

        let instr = *(instr_iter.next().unwrap());
        // println!("Instr: {}, label: {}", &instr, &label);
        // dbg!(&problem.nodes.get(label).unwrap());

        if instr == 'L' {
            label = &problem.nodes.get(label).unwrap().left;
        } else {
            label = &problem.nodes.get(label).unwrap().right;
        }
        counter += 1;
    }

    counter
}

fn process(file_path: &str, part_two: bool) -> io::Result<i64> {
    let problem = parse(file_path).unwrap();

    if !part_two {
        Ok(walk_the_tree("AAA", |label: &str| label == "ZZZ", &problem))
    } else {
        let mut numbers: Vec<i64> = Vec::new();
        for (start, _) in problem.nodes.iter() {
            if start.ends_with('A') {
                numbers.push(walk_the_tree(
                    start,
                    |label: &str| label.ends_with('Z'),
                    &problem,
                ))
            }
        }
        Ok(numbers.into_iter().reduce(lcm).unwrap())
    }
}

fn main() {
    let test_file = "test.txt";
    let test_file_part2 = "test3.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false).unwrap());
    println!("{}", process(input_file, false).unwrap());

    println!("{}", process(test_file_part2, true).unwrap());
    println!("{}", process(input_file, true).unwrap());
}
