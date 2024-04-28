use itertools::iproduct;
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::ops::Add;

struct Map {
    height: usize,
    width: usize,
    data: Vec<char>,
}

struct Problem {
    map: Map,
    galaxy_rows: Vec<i64>,
    galaxy_cols: Vec<i64>,
    multiplier: i64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Vec2D {
    x: i64,
    y: i64,
}

impl Map {
    pub fn new(height: usize, width: usize, data: Vec<char>) -> Map {
        Map {
            height,
            width,
            data,
        }
    }

    fn column_iter(&self, column_index: usize) -> impl Iterator<Item = &char> {
        self.data.iter().skip(column_index).step_by(self.height)
    }

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
        if position.x < 0 || position.y < 0 {
            None
        } else {
            let index: usize = position.y as usize * self.width + position.x as usize;
            Some(self.data.get(index).unwrap())
        }
    }
}

impl Debug for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = self
            .data
            .chunks(self.width)
            .map(|row| f.debug_list().entries(row.iter()).finish())
            .collect::<Vec<_>>();
        writeln!(f)
    }
}

impl Problem {
    pub fn new(map: Map, multiplier: i64) -> Problem {
        let galaxy_rows: Vec<i64> = map
            .data
            .chunks(map.width)
            .map(|row| if row.contains(&'#') { 1 } else { 0 })
            .collect();

        let galaxy_cols: Vec<i64> = (0..map.width)
            .map(|column_index| {
                map.column_iter(column_index)
                    .find_map(|c| if *c == '#' { Some(1) } else { None })
                    .unwrap_or(0)
            })
            .collect();

        Problem {
            map,
            galaxy_rows,
            galaxy_cols,
            multiplier,
        }
    }

    fn get_empty_spaces(&self, a: i64, b: i64, by_columns: bool) -> i64 {
        let start = a.min(b) as usize;
        let end = a.max(b) as usize;
        if by_columns {
            self.galaxy_cols[start..end]
                .iter()
                .filter(|x| **x == 0i64)
                .count() as i64
        } else {
            self.galaxy_rows[start..end]
                .iter()
                .filter(|x| **x == 0)
                .count() as i64
        }
    }

    pub fn distance(&self, a: &Vec2D, b: &Vec2D) -> i64 {
        let mut dx = (a.x - b.x).abs();
        let mut dy = (a.y - b.y).abs();

        dx += self.get_empty_spaces(a.x, b.x, true) * (self.multiplier - 1);
        dy += self.get_empty_spaces(a.y, b.y, false) * (self.multiplier - 1);

        dx + dy
    }

    pub fn solve(&self) -> i64 {
        let galaxies: Vec<Vec2D> = self
            .map
            .data
            .iter()
            .enumerate()
            .flat_map(|x| {
                if *x.1 == '#' {
                    Some(self.map.coords(x.0))
                } else {
                    None
                }
            })
            .collect();

        let product: i64 = iproduct!(&galaxies, &galaxies)
            .map(|(a, b)| self.distance(a, b))
            .sum();

        product / 2
    }
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

    let multiplier = if part_two { 1000000 } else { 2 };

    let problem = Problem::new(map, multiplier);
    problem.solve()
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false));
    println!("{}", process(input_file, false));

    println!("{}", process(test_file, true));
    println!("{}", process(input_file, true));
}
