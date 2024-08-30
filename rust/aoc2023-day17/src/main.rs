use map2d::{Map, Vec2D};
use std::collections::BinaryHeap;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Clone, Debug)]
struct Vertex {
    distance: i64,
    predecessor: Vec2D,
    position: Vec2D,
    used: bool,
}

impl PartialEq for Vertex {
    fn eq(&self, other: &Self) -> bool {
        self.position == other.position
    }
}

impl Eq for Vertex {}

impl Ord for Vertex {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.distance.cmp(&other.distance).reverse()
    }
}

impl PartialOrd for Vertex {
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

fn get_prev_dir(vertex: Vec2D, graph: &Map<Vertex>) -> Option<(Vec2D, Vec2D)> {
    let this_vertex = graph.get(vertex).unwrap();
    let prev_vertex = this_vertex.predecessor;

    if prev_vertex.x < 0 || prev_vertex.y < 0 {
        // starting vertex
        None
    } else {
        let dir = vertex - prev_vertex;
        Some((dir, prev_vertex))
    }
}

fn get_next(vertex: Vec2D, graph: &Map<Vertex>) -> Vec<Vec2D> {
    let mut res = Vec::new();

    let directions = vec![
        Vec2D { x: 0, y: -1 },
        Vec2D { x: 0, y: 1 },
        Vec2D { x: 1, y: 0 },
        Vec2D { x: -1, y: 0 },
    ];

    for dir in directions {
        // do not consider the direction from which we arrived at this vertex
        if let Some((prev_dir, _)) = get_prev_dir(vertex, graph) {
            if prev_dir == dir * -1 {
                continue;
            }
        }

        let mut dir_ok = true;
        // let mut prev_dir: Vec2D;
        // let mut prev_vertex: Vec2D;
        let mut this_dir = dir;
        let mut this_vertex = vertex;

        let mut count: u64 = 0;

        let max_same = 3;

        // check if the direction was repeated 3 times
        while count < max_same {
            if let Some((prev_dir, prev_vertex)) = get_prev_dir(this_vertex, graph) {
                if this_dir == prev_dir {
                    count += 1;
                    this_vertex = prev_vertex;
                    this_dir = prev_dir;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        if count == max_same {
            dir_ok = false;
        }

        if dir_ok {
            if graph.get(vertex + dir).is_some() {
                res.push(vertex + dir);
            }
        }
    }

    res
}

fn process(file_path: &str) -> i64 {
    let map = parse(file_path);

    let mut graph: Map<Vertex> = Map::new(
        map.height,
        map.width,
        vec![
            Vertex {
                distance: 100000,
                predecessor: Vec2D { x: -1, y: -1 },
                position: Vec2D { x: 0, y: 0 },
                used: false,
            };
            map.height * map.width
        ],
    );

    for i in 0..map.width {
        for j in 0..map.height {
            let position = Vec2D {
                x: i as i64,
                y: j as i64,
            };
            let vertex = graph.get_mut(position).unwrap();
            vertex.position = position;
        }
    }

    dbg!(&map);

    // let start = graph.get(Vec2D { x: 0, y: 0 }).unwrap().clone();
    let start = Vertex {
        distance: 0,
        predecessor: Vec2D { x: -1, y: -1 },
        position: Vec2D { x: 0, y: 0 },
        used: true,
    };
    let target = Vec2D {
        x: (map.width - 1) as i64,
        y: (map.height - 1) as i64,
    };
    let mut queue = BinaryHeap::new();
    let mut visited = HashSet::new();
    queue.push(start.clone());

    while !&queue.is_empty() {
        // dbg!(&queue);

        let mut vertex = queue.pop().unwrap();
        while visited.contains(&vertex.position) {
            // TODO: check if we do not drain the queue here!
            vertex = queue.pop().unwrap();
        }

        println!("vertex chosen {:?}", vertex.distance);

        if vertex.position == target {
            break;
        }

        let nexts = get_next(vertex.position, &graph);

        if vertex.position.x == 3 && vertex.position.y == 1 {
            dbg!(&nexts);
        }

        for next in nexts {
            if visited.contains(&next) {
                continue;
            }
            let mut next_vertex = graph.get(next).unwrap().clone();
            let next_value = map.get(next).unwrap().to_digit(10).unwrap() as i64;

            if next_vertex.distance > vertex.distance + next_value {
                graph.modify(next, |v: &mut Vertex| {
                    v.distance = vertex.distance + next_value;
                    v.predecessor = vertex.position;
                });

                next_vertex.distance = vertex.distance + next_value;
                next_vertex.predecessor = vertex.position;

                queue.push(next_vertex);
            }
        }

        visited.insert(vertex.position);
    }

    let mut len = 0;
    let mut pos = graph.get(target).unwrap().predecessor;
    while pos != start.position {
        len += 1;
        graph.modify(pos, |v| v.used = true);
        pos = graph.get(pos).unwrap().predecessor;
    }

    dbg!(&map);

    // graph.print_with(|v| (v.used as i64).to_string());
    println!("------------------------");

    graph.print_with(|v| {
        if v.used {
            format!("|{}|", v.distance)
        } else {
            (v.distance).to_string()
        }
    });
    println!("------------------------");

    graph.print_with(|v| {
        let diff = v.position - v.predecessor;
        match diff {
            Vec2D { x, y } if x < 0 && y == 0 => "<",
            Vec2D { x, y } if x > 0 && y == 0 => ">",
            Vec2D { x, y } if x == 0 && y < 0 => "^",
            Vec2D { x, y } if x == 0 && y > 0 => "v",
            _ => "?",
        }
        .to_string()
    });

    graph.get(target).unwrap().distance
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("-- Test --");
    println!("{}", process(test_file));
    // println!("{}", process2(test_file, &re));
    // println!("-- Main --");
    // println!("{}", process(input_file, &re));
    // println!("{}", process2(input_file, &re));
}
