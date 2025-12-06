use std::error::Error;
use std::mem::replace;

pub type Result<T, E = Box<dyn Error>> = std::result::Result<T, E>;

pub fn error<T: Into<Box<dyn Error>>>(x: T) -> Box<dyn Error> {
   x.into()
}

pub fn xchg<T>(x: T, y: &mut T) -> T {
   replace(y, x)
}

pub trait Blit: Iterator + Sized {
   fn blit(mut self, buf: &mut [Self::Item]) -> (&mut [Self::Item], Self) {
      let mut i = 0;
      while let Some(elt) = self.next() {
         if i >= buf.len() {
            return (buf, self);
         }
         buf[xchg(i + 1, &mut i)] = elt;
      }
      (&mut buf[..i], self)
   }
}

impl<T: Iterator + Sized> Blit for T {}
