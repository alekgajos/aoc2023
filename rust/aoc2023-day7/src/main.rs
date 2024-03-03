use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::mem::discriminant;
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

#[derive(Copy, Clone)]
enum HandOrdering {
    Single = 1,
    Pair,
    TwoPair,
    Triplet,
    FullHouse,
    Quad,
    Quint,
}

struct Hand {
    ordering: HandOrdering,
    hand_numeric: i64,
    bid: u64,
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        ((self.ordering as isize), self.hand_numeric)
            .cmp(&((other.ordering as isize), other.hand_numeric))
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        (discriminant(&self.ordering), self.hand_numeric)
            == (discriminant(&other.ordering), other.hand_numeric)
    }
}

impl Eq for Hand {}

impl Hand {
    fn new(hand_string: &str, bid_string: &str, jokers: bool) -> Self {
        let mut hand_hex = hand_string
            .replace("A", "E")
            .replace("T", "A")
            .replace("J", "B")
            .replace("Q", "C")
            .replace("K", "D");

        if jokers {
            hand_hex = hand_hex.replace("B", "1");
        }

        let hand_numeric = i64::from_str_radix(&hand_hex, 16).unwrap();

        let mut cards_sorted: Vec<_> = hand_hex.chars().collect();
        // let mut cards_sorted = numeric_cards.clone();
        cards_sorted.sort();

        let mut counts = HashMap::new();
        for card in cards_sorted {
            counts
                .entry(card)
                .and_modify(|count| *count += 1u32)
                .or_insert(1u32);
        }

        let mut counts_vec: Vec<u32> = counts.values().copied().collect();
        let joker_count: u32 = *(counts.get(&'1').unwrap_or(&0));

        counts_vec.sort();
        counts_vec.reverse();

        let mut ordering = match counts_vec[0] {
            5 => HandOrdering::Quint,
            4 => HandOrdering::Quad,
            3 => {
                if counts_vec[1] == 2 {
                    HandOrdering::FullHouse
                } else {
                    HandOrdering::Triplet
                }
            }
            2 => {
                if counts_vec[1] == 2 {
                    HandOrdering::TwoPair
                } else {
                    HandOrdering::Pair
                }
            }
            _ => HandOrdering::Single,
        };

        if jokers && joker_count > 0 {
            ordering = match ordering {
                HandOrdering::Quint => HandOrdering::Quint,
                HandOrdering::Quad => HandOrdering::Quint,
                HandOrdering::FullHouse => HandOrdering::Quint,
                HandOrdering::TwoPair => {
                    if joker_count == 1 {
                        HandOrdering::FullHouse
                    } else {
                        HandOrdering::Quad
                    }
                }
                HandOrdering::Triplet => HandOrdering::Quad,
                HandOrdering::Pair => HandOrdering::Triplet,
                HandOrdering::Single => HandOrdering::Pair,
            }
        }

        Self {
            bid: bid_string.parse().unwrap(),
            ordering,
            hand_numeric,
        }
    }
}

fn parse(file_path: &str, jokers: bool) -> Result<Vec<Hand>, ParsingError> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);

    Ok(reader
        .lines()
        .map(|line| {
            let line = line.unwrap();
            let tokens: Vec<_> = line.split_whitespace().collect();
            Hand::new(tokens[0], tokens[1], jokers)
        })
        .collect())
}

fn process(file_path: &str, jokers: bool) -> io::Result<u64> {
    let mut hands = parse(file_path, jokers).unwrap();

    hands.sort();

    let result: u64 = hands
        .iter()
        .enumerate()
        .map(|(rank, hand)| (rank as u64 + 1) * hand.bid)
        .sum();

    Ok(result)
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    // println!("{}", process(test_file, false).unwrap());
    // println!("{}", process(input_file, false).unwrap());

    println!("{}", process(test_file, true).unwrap());
    println!("{}", process(input_file, true).unwrap());
}
