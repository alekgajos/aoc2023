use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Clone)]
struct Lens {
    power: i64,
    label: String,
}

fn parse(file_path: &str) -> Vec<String> {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);

    reader
        .lines()
        .flat_map(|line| {
            line.unwrap()
                .trim()
                .split(',')
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<String>>()
}

fn hash(word: String) -> i64 {
    let mut value: i64 = 0;

    for character in word.into_bytes() {
        value += character as i64;
        value *= 17;
        value %= 256;
    }
    value
}

fn process(file_path: &str, part_two: bool) -> i64 {
    let words = parse(file_path);
    if !part_two {
        let hashes: Vec<_> = words.into_iter().map(hash).collect();
        hashes.iter().sum()
    } else {
        let mut boxes: Vec<Vec<Lens>> = Vec::new();
        for _i in 0..256 {
            boxes.push(Vec::new());
        }

        for word in words {


            if word.contains('=') {
                let operands: Vec<&str> = word.split('=').collect();
                let label = operands.first().unwrap().to_string();
                let hash = hash(label.clone());
                let power: i64 = operands.last().unwrap().parse().unwrap();

                let onebox = &mut boxes[hash as usize];
                match onebox.iter_mut().find(|lens| lens.label == label) {
                    Some(ref mut lens) => {
                        lens.power = power;
                    }
                    None => {
                        onebox.push(Lens { power, label });
                    }
                }
            } else if word.contains('-') {
                let label = &word[0..&word.len() - 1].to_string();
                let hash = hash(label.clone());
                let onebox = &mut boxes[hash as usize];
                onebox.retain(|lens| lens.label != *label);
            }
        }

        let mut power: i64 = 0;
        for i in 0..256 {
           for k in 1..=boxes[i].len() {
                power += ((i+1) as i64)*(k as i64)*boxes[i][k-1].power;
            }
        }

        power
    }
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false));
    println!("{}", process(input_file, false));

    println!("{}", process(test_file, true));
    println!("{}", process(input_file, true));
}
