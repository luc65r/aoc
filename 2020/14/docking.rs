use std::{
    collections::HashMap,
    io::{self, BufRead},
};

type Mem = HashMap<u64, u64>;

fn main() {
    let mut mem1: Mem = HashMap::new();
    let mut mem2: Mem = HashMap::new();
    let mut mask: (u64, u64) = (0, 0);
    let mut masks: Vec<(u64, u64)> = Vec::new();

    for line in io::stdin().lock().lines() {
        let str_line = line.unwrap();
        if str_line.as_bytes()[1] == b'a' {
            mask = to_mask(&str_line[7..43]);
            masks = to_masks(&str_line[7..43]);
        } else {
            let bracket = str_line.find(']').unwrap();
            let place: u64 = str_line[4 .. bracket].parse().unwrap();
            let value: u64 = str_line[bracket + 4 ..].parse().unwrap();

            mem1.insert(place, value & mask.0 | mask.1);
            for m in &masks {
                mem2.insert(place & m.0 | m.1, value);
            }
        }
    }

    println!("{}\n{}", mem1.values().sum::<u64>(),
                       mem2.values().sum::<u64>());
}

fn to_mask(s: &str) -> (u64, u64) {
    let mut mask = (u64::MAX, 0);
    for (i, c) in s.as_bytes().iter().enumerate() {
        match c {
            b'0' => mask.0 &= !(1 << (35 - i)),
            b'1' => mask.1 |= 1 << (35 - i),
            b'X' => {},
            _ => panic!()
        }
    }
    return mask;
}

fn to_masks(s: &str) -> Vec<(u64, u64)> {
    let mut masks = vec![(u64::MAX, 0)];
    for (i, c) in s.as_bytes().iter().enumerate() {
        match c {
            b'0' => {}
            b'1' => for mask in &mut masks {
                mask.1 |= 1 << (35 - i);
            }
            b'X' => {
                let mut m = Vec::new();
                for mask in &mut masks {
                    mask.0 &= !(1 << (35 - i));
                    m.push((mask.0, mask.1 | (1 << (35 - i))));
                }
                masks.append(&mut m);
            }
            _ => panic!()
        }
    }
    return masks;
}
