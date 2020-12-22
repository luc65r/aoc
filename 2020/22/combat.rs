#![feature(deque_range)]

use std::{
    collections::{VecDeque, HashSet},
    io::{self, BufRead},
};

struct NormalGame {
    decks: [VecDeque<u8>; 2],
}

struct RecursiveGame {
    decks: [VecDeque<u8>; 2],
    already_played: HashSet<[VecDeque<u8>; 2]>,
}

impl NormalGame {
    fn new() -> Self {
        NormalGame {
            decks: [VecDeque::new(), VecDeque::new()],
        }
    }
}

impl Iterator for NormalGame {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.decks[0].is_empty() || self.decks[1].is_empty() {
            return None;
        }

        let c = [
            self.decks[0].pop_front().unwrap(),
            self.decks[1].pop_front().unwrap(),
        ];
        let winner = (c[0] < c[1]) as u8;
        self.decks[winner as usize].push_back(c[winner as usize]);
        self.decks[winner as usize].push_back(c[1 - winner as usize]);
        return Some(winner);
    }
}

impl Iterator for RecursiveGame {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.decks[0].is_empty() || self.decks[1].is_empty() {
            return None;
        }

        if !self.already_played.insert(self.decks.clone()) {
            self.decks[0].clear();
            self.decks[1].clear();
            return Some(0);
        }

        let c = [
            self.decks[0].pop_front().unwrap(),
            self.decks[1].pop_front().unwrap(),
        ];
        let winner = if c[0] as usize <= self.decks[0].len()
                && c[1] as usize <= self.decks[1].len() {
            let new_game = RecursiveGame {
                decks: [
                    self.decks[0].range(0 .. c[0] as usize).copied().collect(),
                    self.decks[1].range(0 .. c[1] as usize).copied().collect(),
                ],
                already_played: HashSet::new(),
            };
            new_game.last().unwrap()
        } else {
            (c[0] < c[1]) as u8
        };

        self.decks[winner as usize].push_back(c[winner as usize]);
        self.decks[winner as usize].push_back(c[1 - winner as usize]);
        return Some(winner);
    }
}

fn get_score(deck: &VecDeque<u8>) -> usize {
    let mut sum = 0;
    let mut m = deck.len();
    for c in deck {
        sum += *c as usize * m;
        m -= 1;
    }

    return sum;
}

fn main() {
    let stdin = io::stdin();

    let normal_game = &mut NormalGame::new();

    let mut current_deck = &mut normal_game.decks[0];

    for linew in stdin.lock().lines() {
        let line = linew.unwrap();
        if line == "Player 2:" {
            current_deck = &mut normal_game.decks[1];
        }
        if let Ok(n) = line.parse() {
            current_deck.push_back(n);
        }
    }

    let recursive_game = &mut RecursiveGame {
        decks: normal_game.decks.clone(),
        already_played: HashSet::new(),
    };
    
    println!("{}\n{}", get_score(
            &normal_game.decks[normal_game.last().unwrap() as usize]
        ), get_score(
            &recursive_game.decks[recursive_game.last().unwrap() as usize]
        ));
}
