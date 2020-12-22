#![feature(min_const_generics)]

use std::{
    collections::{HashMap, HashSet},
    io::{self, BufRead},
};

type Pos<const N: usize> = [isize; N];

#[derive(Debug, PartialEq)]
struct World<const N: usize> {
    cells: HashSet<Pos<N>>,
}

impl<const N: usize> World<N> {
    fn new(s: &Vec<String>) -> Self {
        let mut w = World {
            cells: HashSet::new(),
        };

        for (y, l) in s.iter().enumerate() {
            for (x, c) in l.bytes().enumerate() {
                if c == b'#' {
                    w.cells.insert({
                        let mut cell = [0; N];
                        cell[0] = x as isize;
                        cell[1] = y as isize;
                        cell
                    });
                }
            }
        }

        return w;
    }

    fn count_neighbors(&self, p: Pos<N>) -> usize {
        neighbors(p).iter().filter(|x| self.cells.get(*x).is_some()).count()
    }
}

impl<const N: usize> Iterator for World<N> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let mut diff: HashMap<Pos<N>, bool> = HashMap::new();
        for pos in self.cells.iter() {
            if diff.get(pos) == None {
                diff.insert(*pos, activated(true, self.count_neighbors(*pos)));
            }
            for neigh in neighbors(*pos) {
                if diff.get(&neigh) == None {
                    diff.insert(neigh, activated(self.cells.get(&neigh) != None,
                                                 self.count_neighbors(neigh)));
                }
            }
        }

        for (pos, activate) in diff {
            if activate {
                self.cells.insert(pos);
            } else {
                self.cells.remove(&pos);
            }
        }

        return Some(self.cells.len());
    }
}

fn main() {
    let stdin = io::stdin();
    let lines: Vec<String> = stdin.lock().lines().map(|l| l.unwrap()).collect();
    let mut world3d: World<3> = World::new(&lines);
    let mut world4d: World<4> = World::new(&lines);

    println!("{}\n{}", world3d.nth(5).unwrap(), world4d.nth(5).unwrap()); 
}

fn activated(a: bool, n: usize) -> bool {
    if a {
        n == 2 || n == 3
    } else {
        n == 3
    }
}

fn neighbors<const N: usize>(p: Pos<N>) -> Vec<Pos<N>> {
    let nb = 3_usize.pow(N as u32);
    let mut neigh = Vec::with_capacity(nb - 1);

    for mut i in 0..nb {
        if i == (nb - 1) / 2 {
            continue; // a == p
        }

        let mut a = p;
        for j in 0..N {
            a[j] += (i % 3) as isize - 1;
            i /= 3;
        }

        neigh.push(a);
    }

    return neigh;
}

#[test]
fn test_neighbors() {
    assert!({
        let n = neighbors([-5]);
        vec![[-4], [-6]].iter().all(|x| n.contains(x))
    });
    assert!({
        let n = neighbors([0, 3]);
        vec![
            [-1, 2], [-1, 3], [-1, 4],
            [ 0, 2],          [ 0, 4],
            [ 1, 2], [ 1, 3], [ 1, 4],
        ].iter().all(|x| n.contains(x))
    });
}
