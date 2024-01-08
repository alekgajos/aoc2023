use atoi::atoi;
use regex::Regex;
use std::collections::HashSet;
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

/// Returns (number of points, number of matching numbers)
fn extract_points(line: &str, re: &Regex) -> std::result::Result<(u32, u32), ParsingError> {
    if let Some((_, [_, winners, numbers])) = re.captures_iter(line).map(|c| c.extract()).next() {
        // let card: u32 = atoi::<u32>(card.as_bytes()).unwrap();
        let winners: Vec<u32> = winners
            .trim()
            .split(' ')
            .filter_map(|s| atoi::<u32>(s.as_bytes()))
            .collect();
        let numbers: Vec<u32> = numbers
            .trim()
            .split(' ')
            .filter_map(|s| atoi::<u32>(s.as_bytes()))
            .collect();
        // println!("Winners: {:?}", winners);
        // println!("Numbers: {:?}", numbers);

        let winners_set: HashSet<&u32> = HashSet::from_iter(winners.iter());
        let wins: u32 = numbers
            .iter()
            .map(|x| if winners_set.contains(x) { 1 } else { 0 })
            .sum();
        return Ok((if wins >= 1 { 2_u32.pow(wins - 1) } else { 0 }, wins));
    }
    Ok((0, 0))
}

fn process(file_path: &str, re: &Regex) -> Result<u32, ParsingError> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);

    let mut points: u32 = 0;

    for line in reader.lines() {
        points += extract_points(&line?, re).unwrap().0;
    }

    Ok(points)
}

fn process2(file_path: &str, re: &Regex) -> Result<u32, ParsingError> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);

    let points: Vec<u32> = reader
        .lines()
        .map(|line| extract_points(&line.unwrap(), re).unwrap().1)
        .collect();
    let mut counts: Vec<u32> = vec![1;points.len()];

    counts[0] = 1;

    for index in 0..points.len() {
        for _j in 0..counts[index] {
            let mut k = index + 1;
            while k < points.len() && k <= index + points[index] as usize {
                counts[k] += 1;
                k += 1;
            }
        }
    }

    let card_count = counts.iter().sum::<u32>();
    Ok(card_count)
}

fn main() {
    let input_file = "input.txt";
    let test_file = "test.txt";

    let re = Regex::new(r"Card\s+(\d+):\s+((?:\d+\s+)+)\|((?:\s+\d+)+)").unwrap();

    println!("-- Test --");
    println!("{}", process(test_file, &re).unwrap());
    println!("{}", process2(test_file, &re).unwrap());
    println!("-- Main --");
    println!("{}", process(input_file, &re).unwrap());
    println!("{}", process2(input_file, &re).unwrap());
}
