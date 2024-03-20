use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::num::ParseIntError;
use std::rc::Rc;
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
struct TreeNode {
    label: String,
    left: Option<Rc<TreeNode>>,
    right: Option<Rc<TreeNode>>,
}

#[derive(Debug)]
struct Problem {
    root: Rc<TreeNode>,
    instructions: Instructions,
}

fn get_node(label: &str, dict: &HashMap<String, (String, String)>) -> Option<Rc<TreeNode>> {
    let pair = &dict.get(label);

    dbg!(pair);

    if let Some((left, right)) = pair {
        if label != left && label != right {
            Some(Rc::from(TreeNode {
                label: label.to_owned(),
                left: get_node(left, dict),
                right: get_node(right, dict),
            }))
        } else {
            None
        }
    } else {
        None
    }
}

#[derive(Debug)]
struct Instructions<'a> {
    sequence: Vec<char>,
    position: i32,
}

impl Instructions {
    fn new(sequence: Vec<char>) -> Instructions {
        Instructions {
            sequence,
            position: -1,
        }
    }
}

impl<'a> Iterator for Instructions<'a> {
    type Item = &'a char;
    fn next(&mut self) -> Option<Self::Item> {
        self.position = (self.position + 1) % self.sequence.len() as i32;
        Some(&self.sequence[self.position as usize])
    }
}

fn parse(file_path: &str, part_two: bool) -> Result<Problem, ParsingError> {
    let file = File::open(file_path)?;
    let mut reader = BufReader::new(file);

    let mut buffer = String::new();

    let _ = reader.read_line(&mut buffer);

    let instructions: Vec<_> = buffer.chars().collect();
    let instructions = Instructions::new(instructions);

    let re = Regex::new(r"(?<source>\w\w\w) = \((?<left>\w\w\w), (?<right>\w\w\w)\)").unwrap();

    let mut dict = HashMap::new();

    reader.lines().for_each(|line| {
        let line = line.unwrap();

        if let Some(captures) = re.captures(&line) {
            let source = captures["source"].to_string().clone();
            let left = captures["left"].to_string();
            let right = captures["right"].to_string();

            dict.insert(source, (left, right));
        }
    });

    let start_label = "AAA";
    let root = get_node(start_label, &dict).unwrap();

    Ok(Problem { root, instructions })
}

fn process(file_path: &str, part_two: bool) -> io::Result<u64> {
    let problem = parse(file_path, part_two).unwrap();

    let mut node: &TreeNode = &problem.root;

    loop {
        if problem.instructions.next().unwrap() == 'R' {
            node = &node.right.unwrap();
        } else {
            node = &node.left.unwrap();
        }
        if node.label == "ZZZ" {
            break;
        }
    }

    Ok(1)
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    // println!("{}", process(test_file, false).unwrap());
    // println!("{}", process(input_file, false).unwrap());

    println!("{}", process(test_file, true).unwrap());
    // println!("{}", process(input_file, true).unwrap());
}
