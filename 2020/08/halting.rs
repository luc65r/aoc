use std::{
    io::{self, BufRead},
    str::FromStr,
    error::Error,
    iter::repeat,
    convert::TryInto,
    fmt,
};

#[derive(Debug)]
enum Instruction {
    Acc(i64),
    Jmp(i64),
    Nop(i64),
}

use Instruction::*;

#[derive(Debug)]
struct ParseError;

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error!")
    }
}

impl Error for ParseError {}

impl FromStr for Instruction {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut words = s.split(' ');
        let op = match words.next() {
            Some("acc") => |n| Acc(n),
            Some("jmp") => |n| Jmp(n),
            Some("nop") => |n| Nop(n),
            Some(_) => return Err(Box::new(ParseError)),
            None => return Err(Box::new(ParseError)),
        };

        return match words.next() {
            Some(x) => match x.parse::<i64>() {
                Ok(n) => Ok(op(n)),
                Err(err) => Err(Box::new(err)),
            },

            None => Err(Box::new(ParseError)),
        };
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Status {
    Running,
    Loop,
    Terminated,
}

use Status::*;

#[derive(Debug, Clone, Copy)]
struct State {
    position: usize,
    accumulator: i64,
    status: Status,
}

#[derive(Debug)]
struct Program {
    instructions: Vec<(Instruction, bool)>,
    state: State,
}

impl Program {
    fn new<I>(ins: I) -> Self
    where
        I: Iterator<Item = Instruction>,
    {
        return Program {
            instructions: ins.zip(repeat(false)).collect(),
            state: State {
                position: 0,
                accumulator: 0,
                status: Running,
            },
        }
    }
}

impl Iterator for Program {
    type Item = State;

    fn next(&mut self) -> Option<Self::Item> {
        if self.state.status != Running {
            return None;
        }

        if self.state.position >= self.instructions.len() {
            self.state.status = Terminated;
            return Some(self.state);
        }

        match self.instructions[self.state.position] {
            (_, true) => {
                self.state.status = Loop;
            }
            (Acc(n), _) => {
                self.state.accumulator += n;
                self.instructions[self.state.position].1 = true;
                self.state.position += 1;
            }
            (Jmp(n), _) => {
                self.instructions[self.state.position].1 = true;
                self.state.position = (self.state.position as i64 + n).try_into().unwrap();
            }
            (Nop(_), _) => {
                self.instructions[self.state.position].1 = true;
                self.state.position += 1;
            }
        }

        return Some(self.state);
    }
}

fn main() -> io::Result<()> {
    let stdin = io::stdin();

    let prog = Program::new(stdin.lock().lines().map(|x| x.unwrap().parse().unwrap()));
    if let Some(state) = prog.last() {
        println!("{}", state.accumulator);
    } else {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "Couldn't run instructions"));
    }

    return Ok(());
}
