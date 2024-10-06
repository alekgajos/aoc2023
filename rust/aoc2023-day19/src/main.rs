use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::ops::Index;

#[derive(Debug)]
struct Part {
    map: HashMap<String, i64>,
}

impl Part {
    fn new(map: HashMap<String, i64>) -> Part {
        Part { map }
    }
}

impl Index<&str> for Part {
    type Output = i64;
    fn index(&self, index: &str) -> &Self::Output {
        self.map.get(index).expect("Unexpected part key")
    }
}

#[derive(Debug)]
enum Rule {
    Mid(String, i64, bool, String),
    Final(String),
}

#[derive(Debug)]
struct Workflow {
    rules: Vec<Rule>,
}

fn process(file_path: &str, part_two: bool) -> i64 {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);

    let mut workflows = HashMap::new();
    let mut items = Vec::new();

    reader.lines().for_each(|line| {
        let mut line = line.unwrap().to_string();
        if let Some(first_char) = &line.chars().next() {
            match first_char {
                '{' => {
                    line.remove(0);
                    line.remove(line.len() - 1);
                    let mut map = HashMap::new();
                    line.split(',').for_each(|chunk| {
                        let mut parts = chunk.split('=');
                        let category = parts.next().unwrap();
                        let rating = parts.next().unwrap().parse::<i64>().unwrap();
                        map.insert(category.to_string(), rating);
                    });
                    items.push(Part::new(map));
                }
                _ => {
                    let (name, rest) = line.split_once('{').unwrap();
                    let mut rest = rest.to_string();
                    rest.remove(rest.len() - 1);
                    let rules: Vec<Rule> = rest.split(',').map(parse_rule).collect();

                    workflows.insert(name.to_string(), Workflow { rules });
                }
            }
        }
    });

    items
        .iter()
        .map(|item| process_item(&workflows, item))
        .sum()
}

fn process_item(workflows: &HashMap<String, Workflow>, item: &Part) -> i64 {
    let mut cw: &Workflow = workflows.get("in").unwrap();
    loop {
        let mut rule_iter = cw.rules.iter();
        let mut cr: &Rule = rule_iter.next().unwrap();

        loop {
            match cr {
                Rule::Mid(what, thr, less_than_thr, target) => {
                    if (*less_than_thr && item[what] < *thr)
                        || (!*less_than_thr && item[what] > *thr)
                    {
                        // rule accepts the part
                        if target == "A" {
                            return item.map.values().sum();
                        } else if target == "R" {
                            return 0;
                        } else {
                            cw = workflows.get(target).unwrap();
                            break;
                        }
                    } else {
                        // move to the next rule in the same workflow
                        cr = rule_iter.next().unwrap();
                    }
                }
                Rule::Final(target) => {
                    if target == "A" {
                        return item.map.values().sum();
                    } else if target == "R" {
                        return 0;
                    } else {
                        cw = workflows.get(target).unwrap();
                        break;
                    }
                }
            }
        }
    }
}

fn parse_rule(rule: &str) -> Rule {
    if let Some((cond, target)) = rule.split_once(':') {
        if let Some((what, thr)) = cond.split_once('<') {
            Rule::Mid(
                what.to_string(),
                thr.parse::<i64>().unwrap(),
                true,
                target.to_string(),
            )
        } else if let Some((what, thr)) = cond.split_once('>') {
            Rule::Mid(
                what.to_string(),
                thr.parse::<i64>().unwrap(),
                false,
                target.to_string(),
            )
        } else {
            panic!("Incorrect input");
        }
    } else {
        Rule::Final(rule.to_string())
    }
}

fn main() {
    let test_file = "test.txt";
    let input_file = "input.txt";

    println!("{}", process(test_file, false));
    println!("{}", process(input_file, false));
    //
    // println!("{}", process(test_file, true));
    // println!("{}", process(input_file, true));
}
