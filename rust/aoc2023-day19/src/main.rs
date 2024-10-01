use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

struct Part {
    x: i64,
    m: i64,
    a: i64,
    s: i64,
}

impl Part {
    fn new(map: &HashMap<String, i64>) {
        let part = Part {
            x: 0,
            m: 0,
            a: 0,
            s: 0,
        };

        // macro_rules! assign {
        //     ($var:ident = $default:expr) => {
        //         if let Some(val) = map.get(stringify!($var)) {
        //            part.$var = val.parse()
        //         }
        //     };
        // }
    }
}

fn process(file_path: &str, part_two: bool) -> i64 {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);

    reader.lines().map(|line| {
        let mut line = line.unwrap().to_string();
        let first_char = &line.chars().next().unwrap().clone();
        match first_char {
            '{' => {
                line.remove(0);
                line.remove(line.len() - 1);
                line.split(',').map(|chunk| {
                    let parts = chunk.split('=');
                    let category = parts.next().unwrap();
                    let rating = parts.next().unwrap().parse::<i64>();
                })
            }
            _ => {}
        }
    });

    0
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false));
    println!("{}", process(input_file, false));
    //
    // println!("{}", process(test_file, true));
    // println!("{}", process(input_file, true));
}
