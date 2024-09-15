use map2d::{Map, Vec2D};
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct Vertex {
    position: Vec2D,
    direction: Vec2D,
    steps: i64,
}

struct Candidate {
    vertex: Vertex,
    distance: i64,
}

impl PartialEq for Candidate {
    fn eq(&self, other: &Self) -> bool {
        self.distance == other.distance
    }
}

impl Eq for Candidate {}

impl Ord for Candidate {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.distance.cmp(&other.distance).reverse()
    }
}

impl PartialOrd for Candidate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn parse(file_path: &str) -> Map<char> {
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

fn get_next(vertex: &Vertex) -> Vec<Vec2D> {
    let directions = vec![
        Vec2D { x: 0, y: -1 },
        Vec2D { x: 0, y: 1 },
        Vec2D { x: 1, y: 0 },
        Vec2D { x: -1, y: 0 },
    ];

    // handle the start vertex
    if vertex.position.x == 0 && vertex.position.y == 0 {
        return vec![directions[1], directions[2]];
    }

    let mut res = Vec::new();
    for dir in directions {
        if vertex.direction * dir == 0 {
            res.push(dir);
        }
    }

    res
}

fn process(file_path: &str, min_steps: i64, max_steps: i64) -> i64 {
    let map = parse(file_path);

    let target = Vec2D {
        x: (map.width - 1) as i64,
        y: (map.height - 1) as i64,
    };

    let mut queue = BinaryHeap::new();
    let mut visited = HashSet::new();
    let mut distances = HashMap::new();

    queue.push(Candidate {
        vertex: Vertex {
            position: Vec2D { x: 0, y: 0 },
            direction: Vec2D { x: 0, y: 0 },
            steps: 0,
        },
        distance: 0,
    });

    while !&queue.is_empty() {
        // dbg!(&queue);

        let Candidate { vertex, distance } = queue.pop().unwrap();
        if visited.contains(&vertex) {
            continue;
        }

        if vertex.position == target {
            return distance;
        }

        visited.insert(vertex.clone());

        let nexts = get_next(&vertex);

        for dir in nexts {
            let mut path_heat = 0;

            for step in 1..=max_steps {
                let pos = vertex.position + dir * step;

                if let Some(next_value) = map.get(pos) {
                    let new_vertex = Vertex {
                        position: pos,
                        direction: dir,
                        steps: step,
                    };
                    let next_value = next_value.to_digit(10).unwrap() as i64;
                    path_heat += next_value;
                    let new_distance = distance + path_heat;
                    if new_distance < *distances.get(&new_vertex).unwrap_or(&i64::MAX) {
                        distances.insert(new_vertex.clone(), new_distance);
                        if step >= min_steps {
                            queue.push(Candidate {
                                vertex: new_vertex.clone(),
                                distance: new_distance,
                            });
                        }
                    }
                }
            }
        }
    }

    dbg!(&map);

    10000
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("-- Test --");
    println!("{}", process(test_file, 1, 3));
    println!("{}", process(test_file, 4, 10));
    println!("-- Main --");
    // println!("{}", process(input_file, 1, 3));
    println!("{}", process(input_file, 4, 10));
}
