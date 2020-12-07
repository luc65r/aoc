use std::{
    collections::HashMap,
    io::{self, BufRead, Error, ErrorKind},
};

type Color = String;
type Inside = (Color, u8);
type Bags = HashMap<Color, Vec<Inside>>;

fn main() -> io::Result<()> {
    let stdin = io::stdin();

    let mut bags: Bags = HashMap::new();
    for line in stdin.lock().lines() {
        match parse_line(line?) {
            Ok((bag, in_bag)) => {
                bags.insert(bag, in_bag);
            }
            Err(err) => return Err(Error::new(
                ErrorKind::InvalidInput,
                format!("While parsing stdin: {}", err)
            ))
        }
    }

    let mut cache = HashMap::new();
    let mut nb = 0;
    for bag in bags.keys() {
        if contains_gold(bag, &bags, &mut cache) {
            nb += 1;
        }
    }

    println!("{}\n{}", nb, bags_in(&String::from("shiny gold"), &bags));

    return Ok(());
}

fn bags_in(color: &Color, bags: &Bags) -> u64 {
    return bags.get(color).unwrap()
        .iter()
        .map(|x| x.1 as u64 * (bags_in(&x.0, bags) + 1))
        .sum();
}

fn contains_gold(color: &Color, bags: &Bags, cache: &mut HashMap<Color, bool>) -> bool {
    if let Some(b) = cache.get(color) {
        return *b;
    }

    let inside = bags.get(color).unwrap();
    if let Some(_) = inside.iter().find(|x| x.0 == "shiny gold") {
        cache.insert(color.clone(), true);
        return true;
    } else {
        for bag in inside {
            if contains_gold(&bag.0, bags, cache) {
                return true;
            }
        }

        cache.insert(color.clone(), false);
        return false;
    }
}

fn get_two<'a, I>(words: &mut I) -> Option<String>
where
    I: Iterator<Item = &'a str>
{
    return words.next()
        .and_then(|x| words.next()
                  .map(|y| x.to_owned() + " " + y));
}

fn parse_line(line: String) -> Result<(Color, Vec<Inside>), String> {
    let mut words = line.split(' ');
    let mut res = match get_two(&mut words) {
        Some(s) => (s, vec![]),
        None => return Err(String::from("Unexpected end of line"))
    };

    match words.next() {
        Some("bags") => {}
        Some(s) => return Err(format!("Unexpected word: {}", s)),
        None => return Err(String::from("Unexpected end of line"))
    }

    match words.next() {
        Some("contain") => {}
        Some(s) => return Err(format!("Unexpected word: {}", s)),
        None => return Err(String::from("Unexpected end of line"))
    }

    loop {
        match words.next() {
            Some("no") => break,
            Some(num) => match num.parse::<u8>() {
                Ok(n) => if let Some(s) = get_two(&mut words) {
                    res.1.push((s, n));
                } else {
                    return Err(String::from("Unexpected end of line"));
                }
                Err(_) => return Err(format!("Wanted digit, got {}", num))
            }
            None => break
        }

        match words.next() {
            Some("bag,") => {}
            Some("bags,") => {}
            Some("bag.") => break,
            Some("bags.") => break,
            Some(s) => return Err(format!("Unexpected word: {}", s)),
            None => return Err(String::from("Unexpected end of line"))
        }
    }

    return Ok(res);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsing() {
        assert!(parse_line(String::from(
            "light red bags contain 1 bright white bag, 2 muted yellow bags."
        )) == Ok((String::from("light red"), vec![
            (String::from("bright white"), 1),
            (String::from("muted yellow"), 2),
        ])));

        assert!(parse_line(String::from(
            "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
        )) == Ok((String::from("dark orange"), vec![
            (String::from("bright white"), 3),
            (String::from("muted yellow"), 4),
        ])));

        assert!(parse_line(String::from(
            "faded blue bags contain no other bags."
        )) == Ok((String::from("faded blue"), vec![])));
    }
}
