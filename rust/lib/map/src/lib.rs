use core::ops::{Add, Mul, Sub};
use std::fmt::Debug;
use std::fmt::Write;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Vec2D {
    pub x: i64,
    pub y: i64,
}

pub struct Map<T> {
    pub height: usize,
    pub width: usize,
    data: Vec<T>,
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

impl Mul<i64> for Vec2D {
    type Output = Vec2D;
    fn mul(self, rhs: i64) -> Vec2D {
        Vec2D {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}

impl Sub for Vec2D {
    type Output = Vec2D;
    fn sub(self, rhs: Vec2D) -> Vec2D {
        Vec2D {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl<T> Map<T> {
    pub fn new(height: usize, width: usize, data: Vec<T>) -> Map<T> {
        Map {
            height,
            width,
            data,
        }
    }

    pub fn column_iter(&self, column_index: usize) -> impl Iterator<Item = &T> {
        self.data.iter().skip(column_index).step_by(self.height)
    }

    pub fn coords(&self, index: usize) -> Vec2D {
        let x = index % self.width;
        let y = index / self.height;
        Vec2D {
            x: x as i64,
            y: y as i64,
        }
    }

    pub fn get_mut(&mut self, position: Vec2D) -> Option<&mut T> {
        if position.x < 0
            || position.y < 0
            || position.x as usize >= self.width
            || position.y as usize >= self.height
        {
            None
        } else {
            let index: usize = position.y as usize * self.width + position.x as usize;
            Some(self.data.get_mut(index).unwrap())
        }
    }

    pub fn get(&self, position: Vec2D) -> Option<&T> {
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

    pub fn set(&mut self, position: Vec2D, value: T) {
        if position.x >= 0 && position.y >= 0 {
            let index: usize = position.y as usize * self.width + position.x as usize;
            self.data[index] = value;
        }
    }

    pub fn modify(&mut self, position: Vec2D, f: impl Fn(&mut T)) {
        if position.x >= 0 && position.y >= 0 {
            let index: usize = position.y as usize * self.width + position.x as usize;
            f(self.data.get_mut(index).unwrap());
        }
    }

    pub fn print_with(&self, f: impl Fn(&T) -> String) {
        self.data.chunks(self.width).for_each(|row| {
            println!("");
            row.iter().for_each(|x| {
                print!("{}\t", f(x));
            });
        });
        println!("");
    }
}

impl<T: std::fmt::Display> Debug for Map<T> {
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
