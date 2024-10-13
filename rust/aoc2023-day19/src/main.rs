use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::ops::{self, Index};

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

#[derive(Debug, Clone)]
struct Range {
    low: i64,
    high: i64,
    label: String,
    is_empty: bool,
}

impl ops::MulAssign<Range> for Range {
    fn mul_assign(&mut self, rhs: Range) {
        if self.high < rhs.low || self.low > rhs.high {
            self.is_empty = true;
        } else if self.high >= rhs.low {
            self.low = self.low.max(rhs.low);
            self.high = self.high.min(rhs.high);
        }
    }
}

impl Range {
    fn new(label: &str) -> Range {
        Range {
            low: Range::LOW_LIMIT,
            high: Range::HIGH_LIMIT,
            label: label.to_string(),
            is_empty: false,
        }
    }

    fn count(&self) -> i64 {
        if !self.is_empty {
            self.high - self.low + 1
        } else {
            0
        }
    }

    const LOW_LIMIT: i64 = 1;
    const HIGH_LIMIT: i64 = 4000;
}

type Ranges = HashMap<String, Range>;

struct Processor<'a> {
    workflows: &'a HashMap<String, Workflow>,
}

impl<'a> Processor<'a> {
    /// First range in the tuple matches the rule, the second one passes on

    fn dispatch(&self, target: &str, ranges: Ranges) -> i64 {
        match target {
            "A" => {
                ranges.values().map(|r| r.count()).product()
            },
            "R" => {
                0
            },
            _ => {
                let new_workflows = self.workflows.get(target).unwrap().rules.as_slice();
                self.step(ranges, new_workflows)
            }
        }
        
    }

    fn step(&self, ranges: Ranges, rules: &[Rule]) -> i64 {
        match rules {
            [head, tail @ ..] => {
                if let Rule::Final(target) = head {
                    self.dispatch(target, ranges)
                } else {
                    let (matches, passes, target) = rule_to_ranges(head);
                    let mut ranges_left = ranges.clone();
                    *ranges_left.get_mut(&matches.label).unwrap() *= matches;

                    let mut ranges_right = ranges.clone();
                    *ranges_right.get_mut(&passes.label).unwrap() *= passes;

                    self.dispatch(target.as_str(), ranges_left) + self.step(ranges_right, tail)
                }
            }
            _ => panic!(),
        }
    }
}

fn rule_to_ranges(rule: &Rule) -> (Range, Range, String) {
    match rule {
        Rule::Mid(what, thr, is_less_than_thr, target) => {
            if *is_less_than_thr {
                (
                    Range {
                        is_empty: false,
                        low: Range::LOW_LIMIT,
                        high: thr - 1,
                        label: what.to_string(),
                    },
                    Range {
                        is_empty: false,
                        low: *thr,
                        high: Range::HIGH_LIMIT,
                        label: what.to_string(),
                    },
                    target.to_string(),
                )
            } else {
                (
                    Range {
                        is_empty: false,
                        low: thr + 1,
                        high: Range::HIGH_LIMIT,
                        label: what.to_string(),
                    },
                    Range {
                        is_empty: false,
                        low: Range::LOW_LIMIT,
                        high: *thr,
                        label: what.to_string(),
                    },
                    target.to_string(),
                )
            }
        }
        Rule::Final(_) => panic!("Final rule not expected here."),
    }
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

    if !part_two {
        items
            .iter()
            .map(|item| process_item(&workflows, item))
            .sum()
    } else {
        let proc = Processor {
            workflows: &workflows,
        };

        let mut full_ranges = HashMap::new();
        for name in ["x", "m", "a", "s"] {
            full_ranges.insert(name.to_string(), Range::new(name));
        }

        proc.step(full_ranges, workflows.get("in").unwrap().rules.as_slice())
    }
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
    println!("{}", process(test_file, true));
    println!("{}", process(input_file, true));
}
