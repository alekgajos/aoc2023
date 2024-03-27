use regex::Regex;
use std::cell::OnceCell;
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

type NodeRef = Rc<OnceCell<TreeNode>>;

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

// fn get_node(label: &str, tree: &TreeNode) -> NodeRef {
//
//     let node = dict.get(label);
//     let node = TreeNode::find_or_create(root, label)
//
//     if let Some(node) = node {
//         let left = dict.get()
//
//         node.left.get_or_init(|| get_node(left, dict).);
//
//        Rc::from(TreeNode {
//                 label: label.to_owned(),
//                 left:
//                 right: get_node(right, dict),
//             })
//
//         if label != left && label != right {
//             Some()
//         } else {
//             None
//         }
//     } else {
//         None
//     }
// }

impl fmt::Debug for Problem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
       TreeNode::print_node(&self.root, f)
    }
}

impl TreeNode {
    pub fn print_node(noderef: &NodeRef, f: &mut fmt::Formatter) -> fmt::Result {

        if let Some(node) = noderef.get() {
            let _ = TreeNode::print_node(&node.left, f);
            let res = write!(f, "{} ", node.label);
            let _ = TreeNode::print_node(&node.right, f);
            res 
        }else{
            Ok(())
        }
    }

    pub fn find(noderef: &NodeRef, label: &str) -> Option<NodeRef> {
        if let Some(node) = noderef.get() {
            if node.label == label {
                Some(noderef.clone())
            } else {
                if let Some(node) = TreeNode::find(&node.left, label) {
                    Some(noderef.clone())
                } else {
                    TreeNode::find(&node.right, label)
                }
            }
        } else {
            None
        }
    }

    pub fn find_or_create(root: &NodeRef, label: &str) -> NodeRef {
        match TreeNode::find(root, label) {
            Some(node) => node,
            None => Rc::new(
                TreeNode {
                    label: label.to_owned(),
                    left: Rc::new(OnceCell::new()),
                    right: Rc::new(OnceCell::new()),
                }
                .into(),
            ),
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

    let root: NodeRef = Rc::new(OnceCell::new());
    let root = TreeNode::find_or_create(&root, "AAA");

    reader.lines().for_each(|line| {
        let line = line.unwrap();

        if let Some(captures) = re.captures(&line) {
            let source = captures["source"].to_string().clone();
            let left = captures["left"].to_string();
            let right = captures["right"].to_string();

            let node = TreeNode::find_or_create(&root, &source);
            let _ = node.set(TreeNode {
                label: source,
                left: TreeNode::find_or_create(&root, &left),
                right: TreeNode::find_or_create(&root, &right),
            });
        }
    });


    Ok(Problem { root, instructions })
}

fn process(file_path: &str, part_two: bool) -> io::Result<usize> {
    let problem = parse(file_path, part_two).unwrap();

    dbg!(&problem);

    let mut noderef: NodeRef = problem.root;
    let mut instr_iter = problem.instructions.iter();
    let mut counter: usize = 0;

    loop {
        let node = noderef.get().unwrap();

        if node.label == "ZZZ" {
            break;
        }

        dbg!(&node.label);

        if *(instr_iter.next().unwrap()) == 'R' {
            noderef = node.left.clone();
        } else {
            noderef = node.right.clone();
        }
        counter += 1;
    }

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
