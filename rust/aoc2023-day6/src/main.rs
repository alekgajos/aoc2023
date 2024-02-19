use regex::Regex;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::num::ParseIntError;
use std::result::Result;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParsingError {
    #[error("Regex error")]
    RegexError(#[from] ParseIntError),
    #[error("IO Error")]
    IOError(#[from] io::Error),
}

#[derive(Debug)]
struct Game {
    time: u128,
    distance: u128,
}

fn parse(file_path: &str, concatenate: bool) -> Result<Vec<Game>, ParsingError> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);
    let re = Regex::new(r"(\w+)\s*:((?:\s+\d+)+)").unwrap();

    let mut times: Vec<u128> = vec![];
    let mut distances: Vec<u128> = vec![];

    reader.lines().for_each(|line| {
        let line: &str = &line.unwrap();
        if let Some((_, [header, values])) = re.captures_iter(line).map(|c| c.extract()).next() {
            let values_str = (values as &str).split_whitespace();
            match header {
                "Time" => {
                    if !concatenate {
                        times = values_str.map(|x| x.parse::<u128>().unwrap()).collect()
                    } else {
                        let times_str: Vec<_> = values_str.collect();
                        let time = times_str.join("").parse::<u128>().unwrap();
                        times.insert(0, time);
                    }
                }
                "Distance" => {
                    if !concatenate {
                        distances = values_str.map(|x| x.parse::<u128>().unwrap()).collect()
                    } else {
                        let distances_str: Vec<_> = values_str.collect();
                        let distance = distances_str.join("").parse::<u128>().unwrap();
                        distances.insert(0, distance);
                    }
                }
                &_ => (),
            }
        }
    });

    let games: Vec<_> = times
        .iter()
        .zip(distances.iter())
        .map(|(t, d)| Game {
            time: *t,
            distance: *d,
        })
        .collect();

    Ok(games)
}

fn process_game(game: &Game) -> u128 {
    let numbers = (0..=game.time)
        .map(|t| (game.time - t) * t)
        .filter(|d| d > &game.distance)
        .count();
    numbers.try_into().unwrap()
}

fn process(file_path: &str, concatenate: bool) -> io::Result<u128> {
    let games = parse(file_path, concatenate).unwrap();

    let result: u128 = games.iter().map(process_game).product();

    Ok(result)
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false).unwrap());
    println!("{}", process(input_file, false).unwrap());

    println!("{}", process(test_file, true).unwrap());
    println!("{}", process(input_file, true).unwrap());
}
