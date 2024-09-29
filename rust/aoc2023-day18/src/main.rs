use core::panic;
use map2d::{Map, Vec2D};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::{char, vec};

struct Problem {
    map: Map<char>,
}

impl Problem {
    fn find_area(&self) -> i64 {
        let counts: Vec<usize> = self
            .map
            .rows()
            .map(|row| {
                row.iter()
                    .filter(|&c| String::from("UDLR").contains(*c))
                    .count()
            })
            .collect();

        // find last row with trench elements as it can be skipped in the area calculation
        let (last_row, _) = counts
            .iter()
            .enumerate()
            .rev()
            .find(|(_, &c)| c > 0)
            .unwrap();

        let mut area: i64 = counts.iter().sum::<usize>() as i64;

        let mut outside: bool = true;
        for y in 0..last_row {
            if counts[y] > 0 {
                if outside {
                    outside = false; // actual internal area can only start on the next row
                } else {
                    // actual logic
                    let mut inside = false;
                    let mut prev_trench = false;
                    let mut last_vert = 'X';
                    let mut prev_horiz = false;
                    let mut prev = 'X';
                    for x in 0..self.map.width {
                        match self
                            .map
                            .get(Vec2D {
                                x: x as i64,
                                y: y as i64,
                            })
                            .unwrap()
                        {
                            '.' => {
                                if inside {
                                    area += 1;
                                }
                                last_vert = 'X';
                            }
                            d @ 'U' | d @ 'D' => {
                                if last_vert == 'X' {
                                    inside = !inside;
                                } else if last_vert == *d {
                                    inside = inside; // no op
                                } else {
                                    inside = !inside;
                                }

                                last_vert = *d;
                            }
                            'L' | 'R' => {}
                            _ => {
                                prev_trench = true;
                            } // _ => panic!("Incorrect character in the map!"),
                        }
                    }
                }
            } else {
                if !outside {
                    outside = true; // internal area no longer possible
                    break;
                }
            }
            println!("row {} area {}", &y, &area);
        }

        area
    }
}

struct Instruction {
    direction: String,
    steps: i64,
    color: String,
}

// TODO: make a parser for a ready map for tests
fn parse_map(file_path: &str) -> Map<char> {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);

    let rows: Vec<_> = reader
        .lines()
        .map(|line| line.unwrap().chars().collect::<Vec<_>>())
        .collect();
    let n_rows = rows.len();
    let n_columns = rows.first().unwrap().len();

    Map::new(n_rows, n_columns, rows.into_iter().flatten().collect())
}

fn parse(file_path: &str) -> Map<char> {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);

    // let mut instr = Vec::new();

    let instr: Vec<Instruction> = reader
        .lines()
        .map(|line| {
            let line = line.unwrap();
            let parts: Vec<&str> = line.split(" ").collect();
            Instruction {
                direction: parts[0].to_owned(),
                steps: parts[1].parse::<i64>().unwrap(),
                color: parts[2].to_owned(),
            }
        })
        .collect();

    let n_rows: usize = instr
        .iter()
        .filter(|ii| ii.direction == "D")
        .map(|ii| ii.steps)
        .sum::<i64>() as usize
        + 1;
    let n_cols: usize = instr
        .iter()
        .filter(|ii| ii.direction == "R")
        .map(|ii| ii.steps)
        .sum::<i64>() as usize
        + 1;

    let mut map = Map::new(n_rows, n_cols, vec!['.'; n_rows * n_cols]);
    let mut pos: Vec2D = Vec2D { x: 0, y: 0 };

    instr.iter().for_each(|ii| {
        let dir = match ii.direction.as_str() {
            "R" => Vec2D { x: 1, y: 0 },
            "L" => Vec2D { x: -1, y: 0 },
            "D" => Vec2D { x: 0, y: 1 },
            "U" => Vec2D { x: 0, y: -1 },
            _ => panic!("Unknown direction!"),
        };

        let _ = match ii.direction.as_str() {
            "U" | "D" => {
                map.set(pos, *ii.direction.as_bytes().first().unwrap() as char);
            }
            _ => (),
        };
        for _ in 1..=ii.steps {
            let new_pos = pos + dir;
            map.set(new_pos, *ii.direction.as_bytes().first().unwrap() as char);
            pos = new_pos;
        }
    });

    map
}

fn process(file_path: &str, part_two: bool) -> i64 {
    let map = parse(file_path);

    dbg!(&map);
    let mut problem = Problem { map };

    if !part_two {
        problem.find_area()
    } else {
        todo!();
    }
}

fn dbgp(file_path: &str) {
    let map = parse_map(file_path);
    dbg!(&map);
    println!("dbg = {}", Problem { map }.find_area());
}

fn main() {
    let test_file = "test.txt";
    let dbg_file = "test2.txt";
    let input_file = "input.txt";

    dbgp(dbg_file);

    // println!("{}", process(test_file, false));
    // println!("{}", process(input_file, false));
    //
    // println!("{}", process(test_file, true));
    // println!("{}", process(input_file, true));
}
