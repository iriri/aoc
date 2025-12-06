use std::ops::Add;
use std::ops::Mul;
use std::{env, fs};

use aoc::*;

#[derive(Clone, Copy)]
enum Op {
   Add,
   Mul,
}

fn parse_one(input: &str) -> Result<(Vec<Vec<u64>>, Vec<Op>)> {
   input.split_terminator('\n').try_fold(
      (vec![], vec![]),
      |(mut rows, ops): (Vec<Vec<_>>, _), ln| {
         let xs = ln
            .split_terminator(' ')
            .filter(|s| !s.is_empty())
            .map(str::parse)
            .collect::<Result<Vec<u64>, _>>();
         if let Ok(xs) = xs {
            if let Some(row) = rows.first()
               && row.len() != xs.len()
            {
               return Err(error("invalid input"));
            }
            rows.push(xs);
            return Ok((rows, ops));
         }
         let ops = ln
            .split_terminator(' ')
            .filter(|s| !s.is_empty())
            .map(|s| match s.len() {
               1 => match s.as_bytes()[0] {
                  b'+' => Ok(Op::Add),
                  b'*' => Ok(Op::Mul),
                  _ => Err(error("invalid input")),
               },
               _ => Err(error("invalid input")),
            })
            .collect::<Result<Vec<_>>>()?;
         match rows.first() {
            Some(row) if row.len() == ops.len() => Ok((rows, ops)),
            _ => Err(error("invalid input")),
         }
      },
   )
}

fn one(rows: &[Vec<u64>], ops: &[Op]) {
   let n = (0..rows[0].len()).fold(0, |acc, i| {
      let (f, zero): (fn(_, _) -> _, _) = match ops[i] {
         Op::Add => (Add::<u64>::add, 0),
         Op::Mul => (Mul::<u64>::mul, 1),
      };
      acc + rows.iter().fold(zero, |acc, row| f(acc, row[i]))
   });
   println!("{n}");
}

#[allow(clippy::type_complexity)]
fn parse_two(input: &str) -> Result<(Vec<Vec<Option<u8>>>, Vec<(Op, usize)>)> {
   input.split_terminator('\n').try_fold(
      (vec![], vec![]),
      |(mut rows, ops): (Vec<Vec<_>>, _), ln| {
         let ln = ln.as_bytes();
         match ln.first() {
            Some(&b) if b.is_ascii_digit() || b == b' ' => {
               if let Some(row) = rows.first()
                  && row.len() != ln.len()
               {
                  return Err(error("invalid input"));
               }
               let row = ln
                  .iter()
                  .map(|b| match b {
                     b'0'..=b'9' => Ok(Some(b - b'0')),
                     b' ' => Ok(None),
                     _ => Err(error("invalid input")),
                  })
                  .collect::<Result<Vec<_>>>()?;
               rows.push(row);
               Ok((rows, ops))
            },
            Some(_) => {
               let ops = ln
                  .iter()
                  .enumerate()
                  .filter_map(|(i, b)| match b {
                     b'+' => Some(Ok((Op::Add, i))),
                     b'*' => Some(Ok((Op::Mul, i))),
                     b' ' => None,
                     _ => Some(Err(error("invalid input"))),
                  })
                  .collect::<Result<Vec<_>>>()?;
               match (ops.last(), rows.first()) {
                  (Some(&(_, i)), Some(row)) if i < row.len() => Ok((rows, ops)),
                  _ => Err(error("invalid input")),
               }
            },
            _ => Err(error("invalid input")),
         }
      },
   )
}

fn two(rows: &[Vec<Option<u8>>], ops: &[(Op, usize)]) {
   let n = ops.iter().fold(0, |acc, &(op, i)| {
      let (f, mut x): (fn(_, _) -> _, _) = match op {
         Op::Add => (Add::<u64>::add, 0),
         Op::Mul => (Mul::<u64>::mul, 1),
      };
      for i in i..rows[0].len() {
         let mut a = 0;
         for row in rows {
            if let Some(n) = row[i] {
               a = (a * 10) + (n as u64);
            }
         }
         if a == 0 {
            break;
         }
         x = f(x, a);
      }
      acc + x
   });
   println!("{n}");
}

fn main() -> Result<()> {
   let arg = env::args().nth(1).ok_or_else(|| error("missing argument"))?;
   let (nums, ops) = parse_one(&fs::read_to_string(&arg)?)?;
   one(&nums, &ops);
   let (nums, ops) = parse_two(&fs::read_to_string(&arg)?)?;
   two(&nums, &ops);
   Ok(())
}
