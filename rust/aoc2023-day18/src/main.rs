use core::panic;
use map2d::Vec2D;
use std::fs::File;
use std::io::{BufRead, BufReader};

struct Instruction {
    direction: String,
    steps: i64,
}

fn parse(file_path: &str, part_two: bool) -> (Vec<Vec2D>, i64) {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);

    let instr: Vec<Instruction> = reader
        .lines()
        .map(|line| {
            let line = line.unwrap();
            let parts: Vec<&str> = line.split(' ').collect();

            if !part_two {

            Instruction {
                direction: parts[0].to_owned(),
                steps: parts[1].parse::<i64>().unwrap(),
            }
            } else {
                let direction = match parts[2].strip_suffix(')').expect("").chars().last().unwrap() {
                    '0' => "R",
                    '1' => "D",
                    '2' => "L",
                    '3' => "U",
                    _ => panic!("Unexpeected direction char!"),
                }.to_string();
                let steps = i64::from_str_radix(&parts[2][2..=6], 16).unwrap();
                Instruction {direction, steps}

            }
        })
        .collect();

    let mut pos: Vec2D = Vec2D { x: 0, y: 0 };

    let vertices = instr
        .iter()
        .map(|ii| {
            let dir = match ii.direction.as_str() {
                "R" => Vec2D { x: 1, y: 0 },
                "L" => Vec2D { x: -1, y: 0 },
                "D" => Vec2D { x: 0, y: 1 },
                "U" => Vec2D { x: 0, y: -1 },
                _ => panic!("Unknown direction!"),
            };

            pos = pos + dir * ii.steps;
            pos
        })
        .collect();

    let circumference: i64 = instr.iter().map(|ii| ii.steps).sum();

    (vertices, circumference)
}

fn process(file_path: &str, part_two: bool) -> i64 {
    let (vertices, circumference) = parse(file_path, part_two);
    let mut vertices_circular = vertices.clone();
    vertices_circular.push(*vertices.first().unwrap());

    let double_area: i64 = vertices_circular
        .windows(2)
        .map(|pair| pair[0].x * pair[1].y - pair[1].x * pair[0].y)
        .sum();
    let area = double_area / 2;
    area + circumference / 2 + 1
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false));
    println!("{}", process(input_file, false));
    //
    println!("{}", process(test_file, true));
    println!("{}", process(input_file, true)); // 106920098354636
}
