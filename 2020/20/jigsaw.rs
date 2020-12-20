use std::{
    collections::HashMap,
    io::{self, BufRead},
    convert::TryInto,
    cmp::PartialEq,
    mem,
};

#[derive(Debug)]
struct Tile {
    nb: usize,
    borders: [[bool; 10]; 4],
}

impl Tile {
    fn new<I: Iterator<Item = String>>(lines: &mut I) -> Option<Self> {
        let nb: usize = lines
            .next()?
            .get(5..9)?
            .parse().ok()?;
        let image: Vec<Vec<bool>> = lines
            .map(|s| s.chars().map(|c| c == '#').collect())
            .collect();
        return Some(Tile {
            nb: nb,
            borders: [
                image[0].clone().try_into().ok()?,
                image[9].clone().try_into().ok()?,
                image.iter().map(|x| x[0]).collect::<Vec<bool>>().try_into().ok()?,
                image.iter().map(|x| x[9]).collect::<Vec<bool>>().try_into().ok()?,
            ],
        });
    }
}

fn rev<T>(mut border: [T; 10]) -> [T; 10] {
    let (fst, snd) = border.split_at_mut(5);
    for i in 0..5 {
        mem::swap(&mut fst[i], &mut snd[4 - i]);
    }

    return border;
}

fn border_eq<T: PartialEq>(b1: [T; 10], b2: [T; 10]) -> bool {
    b1 == b2 || b1 == rev(b2)
}

fn main() {
    let stdin = io::stdin();
    let lines = &mut stdin.lock().lines().map(|x| x.unwrap());

    //let mut tiles = Vec::new();
    let mut borders: HashMap<[bool; 10], Vec<usize>> = HashMap::new();
    loop {
        let tile = &mut lines.take(11);
        if let Some(t) = Tile::new(tile) {
            for b in &t.borders {
                match borders.get_mut(b) {
                    Some(ids) => ids.push(t.nb),
                    None => match borders.get_mut(&rev(*b)) {
                        Some(ids) => ids.push(t.nb),
                        None => {
                            borders.insert(*b, vec![t.nb]);
                        }
                    }
                }
            }
            //tiles.push(t);
        } else {
            break;
        }
        lines.next();
    }

    let corner_tiles: Vec<usize> = {
        let mut c: HashMap<usize, usize> = HashMap::new();
        borders.values()
            .filter(|ids| ids.len() == 1)
            .map(|ids| ids[0])
            .for_each(|t| match c.get_mut(&t) {
                Some(n) => {
                    *n += 1;
                }
                None => {
                    c.insert(t, 1);
                }
            });
        c.drain().filter(|(_, v)| *v == 2).map(|(k, _)| k).collect()
    };

    println!("{:?}", corner_tiles.iter().product::<usize>());
}
