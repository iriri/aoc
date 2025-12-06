use std::cmp::Ordering;
use std::{env, fs};

use aoc::*;

#[allow(clippy::type_complexity)]
fn parse_input(input: &str) -> Result<(Vec<(u64, u64)>, Vec<u64>)> {
   let (mut ranges, xs, _) = input.split_terminator('\n').try_fold(
      (vec![], vec![], false),
      |(mut ranges, mut xs, state), ln| -> Result<(Vec<(u64, u64)>, Vec<u64>, bool)> {
         if ln.is_empty() {
            return Ok((ranges, xs, true));
         }
         if !state {
            let (l, h) = ln.split_once('-').ok_or_else(|| error("invalid_input"))?;
            ranges.push((l.parse()?, h.parse()?));
            return Ok((ranges, xs, state));
         }
         xs.push(ln.parse()?);
         Ok((ranges, xs, state))
      },
   )?;
   ranges.sort_by(|(l, h), (l1, h1)| match l.cmp(l1) {
      Ordering::Equal => h.cmp(h1),
      o => o,
   });
   Ok((ranges, xs))
}

fn one(ranges: &[(u64, u64)], xs: &[u64]) {
   let n = xs.iter().fold(0, |acc, &x| match ranges.binary_search_by(|(l, _)| x.cmp(l)) {
      Ok(_) => acc + 1,
      Err(i) => {
         for &(l, h) in &ranges[i..] {
            if l > x {
               break;
            }
            if x <= h {
               return acc + 1;
            }
         }
         if ranges[..i].iter().rev().any(|&(l, h)| x >= l && x <= h) {
            return acc + 1;
         }
         acc
      },
   });
   println!("{n}");
}

fn two(ranges: &[(u64, u64)]) {
   let (n, _) = ranges.iter().fold((0, 0), |(n, max), &(l, h)| {
      let l = if l <= max { max + 1 } else { l };
      if l > h {
         return (n, max);
      }
      (n + (h - l) + 1, h)
   });
   println!("{n}");
}

fn main() -> Result<()> {
   let arg = env::args().nth(1).ok_or_else(|| error("missing argument"))?;
   let (ranges, xs) = parse_input(&fs::read_to_string(arg)?)?;
   one(&ranges, &xs);
   two(&ranges);
   Ok(())
}
