use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::{Debug, Write};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::ops::Add;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Vec2D {
    x: i64,
    y: i64,
}

struct Map {
    height: usize,
    width: usize,
    data: Vec<char>,
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

impl Map {
    pub fn new(height: usize, width: usize, data: Vec<char>) -> Map {
        Map {
            height,
            width,
            data,
        }
    }

    #[allow(dead_code)]
    fn column_iter(&self, column_index: usize) -> impl Iterator<Item = &char> {
        self.data.iter().skip(column_index).step_by(self.height)
    }

    #[allow(dead_code)]
    fn coords(&self, index: usize) -> Vec2D {
        let x = index % self.width;
        let y = index / self.height;
        Vec2D {
            x: x as i64,
            y: y as i64,
        }
    }

    #[allow(dead_code)]
    pub fn get(&self, position: Vec2D) -> Option<&char> {
        if position.x < 0
            || position.y < 0
            || position.x as usize >= self.width
            || position.y as usize >= self.height
        {
            None
        } else {
            let index: usize = position.y as usize * self.width + position.x as usize;
            Some(self.data.get(index).unwrap())
        }
    }

    pub fn set(&mut self, position: Vec2D, value: char) {
        if position.x >= 0 && position.y >= 0 {
            let index: usize = position.y as usize * self.width + position.x as usize;
            self.data[index] = value;
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
enum Direction {
    North,
    South,
    East,
    West,
}

static N: Direction = Direction::North;
static S: Direction = Direction::South;
static E: Direction = Direction::East;
static W: Direction = Direction::West;

fn shift(direction: &Direction) -> Vec2D {
    match direction {
        Direction::North => Vec2D { x: 0, y: -1 },
        Direction::South => Vec2D { x: 0, y: 1 },
        Direction::East => Vec2D { x: 1, y: 0 },
        Direction::West => Vec2D { x: -1, y: 0 },
    }
}

#[derive(Debug, Clone)]
struct Step {
    direction: Direction,
    position: Vec2D,
}

impl Debug for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.chunks(self.width).for_each(|row| {
            let _ = f.write_char('\n');
            row.iter().for_each(|x| {
                let _ = f.write_str(format!("{} ", x).as_str());
            });
        });
        writeln!(f)
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

fn redirect(element: char, direction: &Direction) -> Vec<&Direction> {
    match element {
        '/' => match direction {
            Direction::North => vec![&E],
            Direction::South => vec![&W],
            Direction::East => vec![&N],
            Direction::West => vec![&S],
        },
        '\\' => match direction {
            Direction::North => vec![&W],
            Direction::South => vec![&E],
            Direction::East => vec![&S],
            Direction::West => vec![&N],
        },
        '-' => match direction {
            Direction::North | Direction::South => vec![&E, &W],
            _ => vec![direction],
        },
        '|' => match direction {
            Direction::East | Direction::West => vec![&N, &S],
            _ => vec![direction],
        },
        _ => vec![direction],
    }
}

fn process(file_path: &str, part_two: bool) -> i64 {
    let map = parse(file_path);

    if !part_two {
        let start = Vec2D { x: 0, y: 0 };
        let start_direction = E;
        count_energized(&map, start, start_direction)
    } else {
        let mut max_count = 0;
        for x in 0..map.width {
            let start = Vec2D { x: x as i64, y: 0 };
            let c = count_energized(&map, start, S);
            if c > max_count {
                max_count = c;
            }

            let start = Vec2D {
                x: x as i64,
                y: (map.width - 1) as i64,
            };
            let c = count_energized(&map, start, N);
            if c > max_count {
                max_count = c;
            }
        }
        for y in 0..map.height {
            let start = Vec2D { x: 0, y: y as i64 };
            let c = count_energized(&map, start, E);
            if c > max_count {
                max_count = c;
            }

            let start = Vec2D {
                x: (map.height - 1) as i64,
                y: y as i64,
            };
            let c = count_energized(&map, start, W);
            if c > max_count {
                max_count = c;
            }
        }

        max_count
    }
}

fn count_energized(map: &Map, start: Vec2D, start_direction: Direction) -> i64 {
    let directions = [&N, &S, &E, &W];

    let mut cache = directions
        .into_iter()
        .map(|d| {
            (
                d,
                Map::new(map.height, map.width, vec!['0'; map.height * map.width]),
            )
        })
        .collect::<HashMap<_, _>>();

    let mut queue: VecDeque<Step> = VecDeque::new();
    queue.push_back(Step {
        direction: start_direction,
        position: start,
    });

    while !queue.is_empty() {
        let cs = queue.pop_front().unwrap();

        // check if already "visited"
        if let Some(visited) = cache.get(&cs.direction).unwrap().get(cs.position) {
            if *visited != '0' {
                // position was cached
                continue;
            }
        } else {
            // position outside of map
            continue;
        }

        cache.get_mut(&cs.direction).unwrap().set(cs.position, '1');

        let element = map.get(cs.position).unwrap();

        redirect(*element, &cs.direction)
            .into_iter()
            .for_each(|d: &Direction| {
                let shift = shift(d);
                let new_position = cs.position + shift;
                queue.push_back(Step {
                    direction: *d,
                    position: new_position,
                });
            });
    }

    // count energized fields
    let mut no_energized = 0;
    for x in 0..map.width {
        for y in 0..map.height {
            let mut energized = false;

            for cachemap in cache.values() {
                if *cachemap
                    .get(Vec2D {
                        x: x as i64,
                        y: y as i64,
                    })
                    .unwrap()
                    != '0'
                {
                    energized = true;
                }
            }

            if energized {
                no_energized += 1;
            }
        }
    }

    no_energized
}

#[allow(unused_variables)]
fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false));
    println!("{}", process(input_file, false));

    println!("{}", process(test_file, true));
    println!("{}", process(input_file, true));
}
