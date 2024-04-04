use regex::Regex;
use std::cell::RefCell;
use std::fmt;
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

type NodeRef = Option<Rc<RefCell<TreeNode>>>;

struct TreeNode {
    label: String,
    left: NodeRef,
    right: NodeRef,
}

// #[derive(Debug)]
struct Problem {
    root: NodeRef,
    instructions: Instructions,
}

impl fmt::Debug for Problem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        TreeNode::print_node(&self.root, f)
    }
}

impl TreeNode {
    pub fn print_node(noderef: &NodeRef, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(node) = noderef {
            println!("Calling recursive print_node");
            let _ = TreeNode::print_node(&node.borrow().left, f);
            let res = write!(f, "{} ", node.borrow().label);
            let _ = TreeNode::print_node(&node.borrow().right, f);
            res
        } else {
            println!("Called print_node with an empty noderef");
            Ok(())
        }
    }

    pub fn find(noderef: &NodeRef, label: &str) -> NodeRef {
        println!("FIND: entering function for {}", label);

        if let Some(node) = noderef {

            let node = node.borrow();

            println!(
                "FIND: node.label={}, label param={}",
                &node.label,
                label
            );

            if node.label == label {
                println!("FIND: found match {}", &node.label);
                noderef.clone()
            } else if node.label == "ZZZ" {
                None
            } else {
                let left_find = TreeNode::find(&node.left, label);
                let right_find = TreeNode::find(&node.right, label);
                if left_find.is_some() {
                    left_find.clone()
                }else if right_find.is_some() {
                    right_find
                }else{
                    None
                }
            }

        } else {
            None
        }
    }

    pub fn find_or_create(root: &NodeRef, label: &str) -> NodeRef {
        println!("find_or_create called");
        match TreeNode::find(root, label) {
            Some(node) => Some(node),
            None => {
                println!("Creating new node {}", &label);
                Some(Rc::new(RefCell::new(
                    TreeNode {
                        label: label.to_owned(),
                        left: None,
                        right: None,
                    }
                )))
            }
        }
    }
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

fn parse(file_path: &str, part_two: bool) -> Result<Problem, ParsingError> {
    let file = File::open(file_path)?;
    let mut reader = BufReader::new(file);

    let mut buffer = String::new();

    let _ = reader.read_line(&mut buffer);

    let instructions: Vec<_> = buffer.chars().collect();
    let instructions = Instructions {
        sequence: instructions,
    };

    let re = Regex::new(r"(?<source>\w\w\w) = \((?<left>\w\w\w), (?<right>\w\w\w)\)").unwrap();

    // let mut dict = HashMap::new();

    let root: NodeRef = None;
    let root: NodeRef = TreeNode::find_or_create(&root, "AAA");

    reader.lines().for_each(|line| {
        let line = line.unwrap();

        if let Some(captures) = re.captures(&line) {
            let source = captures["source"].to_string().clone();
            let left = captures["left"].to_string();
            let right = captures["right"].to_string();

            println!("processing new line {} = {}, {}", &source, &left, &right);

            if source != "ZZZ" { 
            let node = TreeNode::find_or_create(&root, &source).unwrap();

            let left_child = TreeNode::find_or_create(&root, &left);
            let right_child = TreeNode::find_or_create(&root, &right);

            let mut node = node.borrow_mut();
            node.left = left_child;
            node.right = right_child;
            }
                
            // node.left = Some(Rc::new(RefCell::new(TreeNode {
            //     label: left.to_owned(),
            //     left: None,
            //     right: None,
            // })));
            //
            // node.right = Some(Rc::new(RefCell::new(TreeNode {
            //     label: right.to_owned(),
            //     left: None,
            //     right: None,
            // })));
        }
    });

    Ok(Problem { root, instructions })
}

fn process(file_path: &str, part_two: bool) -> io::Result<usize> {
    let problem = parse(file_path, part_two).unwrap();

    // dbg!(&problem);

    let mut noderef: NodeRef = problem.root;
    let mut instr_iter = problem.instructions.iter();
    let mut counter: usize = 0;

    // loop {
    //     let node = noderef.unwrap();
    //
    //     if node.borrow().label == "ZZZ" {
    //         break;
    //     }
    //
    //     dbg!(&node.borrow().label);
    //
    //     if *(instr_iter.next().unwrap()) == 'L' {
    //         noderef = node.borrow().left.clone();
    //     } else {
    //         noderef = node.borrow().right.clone();
    //     }
    //     counter += 1;
    // }

    Ok(counter)
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    // println!("{}", process(test_file, false).unwrap());
    // println!("{}", process(input_file, false).unwrap());

    println!("{}", process(test_file, true).unwrap());
    // println!("{}", process(input_file, true).unwrap());
}
