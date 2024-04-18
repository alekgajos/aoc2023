#[macro_use]
extern crate lazy_static;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::num::ParseIntError;
use std::ops::Add;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParsingError {
    #[error("Regex error")]
    RegexError(#[from] ParseIntError),
    #[error("IO Error")]
    IOError(#[from] io::Error),
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Vec2D {
    x: i32,
    y: i32,
}

impl Add for Vec2D {
    type Output = Vec2D;
    fn add(self, rhs: Vec2D) -> Vec2D {
        Vec2D {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

#[derive(Debug, EnumIter, Clone, Copy)]
enum Direction {
    N,
    S,
    E,
    W,
}

impl Direction {
    pub fn vec(&self) -> Vec2D {
        match *self {
            Direction::N => Vec2D { x: 0, y: -1 },
            Direction::S => Vec2D { x: 0, y: 1 },
            Direction::E => Vec2D { x: 1, y: 0 },
            Direction::W => Vec2D { x: -1, y: 0 },
        }
    }
}
lazy_static! {
    static ref PIPES: HashSet<char> = HashSet::from(['J', 'F', 'L', '7', '-', '|']);
}

struct Map {
    height: usize,
    width: usize,
    data: Vec<char>,
}

struct Problem {
    map: Map,
}

impl Problem {
    pub fn find_start(&self) -> Vec2D {
        self.map
            .coords(self.map.data.iter().position(|c| *c == 'S').unwrap())
    }

    pub fn find_furthest(&mut self) -> (i64, Vec<Vec2D>) {
        let start = self.find_start();

        let mut starters: Vec<(Vec2D, Direction)> = Vec::new();
        let mut start_directions: Vec<Direction> = Vec::new();

        let mut path: Vec<Vec2D> = vec![start];

        for direction in Direction::iter() {
            let new_pos = start + direction.vec();
            if let Some(c) = self.map.get(new_pos) {
                if PIPES.contains(c) {
                    if let Some(new_direction) = get_direction(c, direction) {
                        start_directions.push(direction);
                        starters.push((new_pos, new_direction));
                    }
                }
            }
        }

        path.push(starters[0].0);
        path.push(starters[1].0);

        let start_variant = (start_directions[0], start_directions[1]);

        let start_symbol = match start_variant {
            (Direction::N, Direction::S) => '|',
            (Direction::S, Direction::N) => '|',

            (Direction::N, Direction::E) => 'L',
            (Direction::E, Direction::N) => 'L',

            (Direction::N, Direction::W) => 'J',
            (Direction::W, Direction::N) => 'J',

            (Direction::S, Direction::E) => 'F',
            (Direction::E, Direction::S) => 'F',

            (Direction::S, Direction::W) => '7',
            (Direction::W, Direction::S) => '7',

            (Direction::W, Direction::E) => '-',
            (Direction::E, Direction::W) => '-',

            _ => panic!("Incorrect start of the loop!"),
        };

        self.map.set(start, start_symbol);

        let mut right = starters.remove(1);
        let mut left = starters.remove(0);

        let mut counter: i64 = 1;
        loop {
            left = self.map.step(left);
            right = self.map.step(right);

            path.push(left.0);
            path.push(right.0);

            counter += 1;
            if left.0 == right.0 {
                break;
            }
        }
        (counter, path)
    }

    fn find_area(&self, path: Vec<Vec2D>) -> i64 {
        let mut clean_map = Map {
            height: self.map.height,
            width: self.map.width,
            data: vec!['.'; self.map.height * self.map.width],
        };

        for pos in path {
            clean_map.set(pos, *self.map.get(pos).unwrap());
        }

        // sweep
        let counts: Vec<usize> = clean_map
            .data
            .chunks(clean_map.width)
            .map(|row| row.iter().filter(|c| PIPES.contains(c)).count())
            .collect();

        let mut area: i64 = 0;

        let mut outside: bool = true;
        for y in 0..clean_map.height {
            if counts[y] > 0 {
                if outside {
                    outside = false; // actual internal area can only start on the next row
                } else {
                    // actual logic
                    let mut inside = false;
                    let mut prev_left = false;
                    for x in 0..clean_map.width {
                        match clean_map
                            .get(Vec2D {
                                x: x as i32,
                                y: y as i32,
                            })
                            .unwrap()
                        {
                            'F' => {
                                inside = !inside;
                                prev_left = true;
                            }
                            'L' => {
                                inside = !inside;
                                prev_left = false;
                            }
                            'J' => {
                                if !prev_left {
                                    inside = !inside;
                                }
                            }
                            '7' => {
                                if prev_left {
                                    inside = !inside
                                }
                            }
                            '|' => inside = !inside,
                            '-' => {}
                            '.' => {
                                if inside {
                                    area += 1;
                                }
                            }
                            _ => panic!("Incorrect character in the pruned map!"),
                        }
                    }
                }
            } else {
                if !outside {
                    outside = true; // internal area no longer possible
                    break;
                }
            }
        }

        area
    }
}

impl Map {
    pub fn new(height: usize, width: usize, data: Vec<char>) -> Map {
        Map {
            height,
            width,
            data,
        }
    }

    fn coords(&self, index: usize) -> Vec2D {
        let x = index % self.width;
        let y = index / self.height;
        Vec2D {
            x: x as i32,
            y: y as i32,
        }
    }

    pub fn get(&self, position: Vec2D) -> Option<&char> {
        if position.x < 0 || position.y < 0 {
            None
        } else {
            let index: usize = position.y as usize * self.width + position.x as usize;
            Some(self.data.get(index).unwrap())
        }
    }

    pub fn set(&mut self, position: Vec2D, c: char) {
        if position.x >= 0 && position.y >= 0 {
            let index: usize = position.y as usize * self.width + position.x as usize;
            self.data[index] = c;
        }
    }

    pub fn step(&self, prev: (Vec2D, Direction)) -> (Vec2D, Direction) {
        let new_pos = prev.0 + prev.1.vec();

        let c = self.get(new_pos).unwrap();
        if PIPES.contains(c) {
            (
                new_pos,
                get_direction(c, prev.1).expect("Inconsistent path!"),
            )
        } else {
            panic!("Pipe leading outside of the map!");
        }
    }
}

impl Debug for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // self.data.chunks(self.width).map(|row| row.fmt(f)).collect::<Vec<_>>();
        self.data
            .chunks(self.width)
            .map(|row| f.debug_list().entries(row.iter()).finish())
            .collect::<Vec<_>>();
        writeln!(f)
    }
}

fn get_direction(c: &char, prev: Direction) -> Option<Direction> {
    match prev {
        Direction::N => match c {
            '|' => Some(prev),
            'F' => Some(Direction::E),
            '7' => Some(Direction::W),
            _ => None,
        },
        Direction::S => match c {
            '|' => Some(prev),
            'L' => Some(Direction::E),
            'J' => Some(Direction::W),
            _ => None,
        },
        Direction::E => match c {
            '-' => Some(prev),
            'J' => Some(Direction::N),
            '7' => Some(Direction::S),
            _ => None,
        },
        Direction::W => match c {
            '-' => Some(prev),
            'F' => Some(Direction::S),
            'L' => Some(Direction::N),
            _ => None,
        },
    }
}

fn parse(file_path: &str) -> Map {
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

fn process(file_path: &str, part_two: bool) -> i64 {
    let map = parse(file_path);

    let mut problem = Problem { map };

    let (distance, path) = problem.find_furthest();

    if !part_two {
        distance
    } else {
        problem.find_area(path)
    }
}

fn main() {
    let test_file = "test.txt";
    let test_file2 = "test2.txt";
    let test_file3 = "test3.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false));
    println!("{}", process(input_file, false));

    println!("{}", process(test_file3, true));
    println!("{}", process(input_file, true));
}
