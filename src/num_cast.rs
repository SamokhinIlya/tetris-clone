use std::convert::TryFrom;
use std::fmt::Debug;

pub trait NumCast: Num {
    /// Short hand for `self.try_into().unwrap()` for numbers.
    ///
    /// # Panics
    /// 
    /// Panics if conversion failed.
    fn num_cast<T>(self) -> T
        where T: Num + TryFrom<Self>,
              <T as TryFrom<Self>>::Error: Debug;
}

impl<U: Num> NumCast for U {
    fn num_cast<T>(self) -> T
        where T: Num + TryFrom<Self>,
              <T as TryFrom<Self>>::Error: Debug
    {
        self.try_into().unwrap()
    }
}

pub trait Num: Sized + Clone {}

macro_rules! impl_num {
    ($type:ty) => {
        impl Num for $type {}
    };
}

impl_num!(isize);
impl_num!(i8);
impl_num!(i16);
impl_num!(i32);
impl_num!(i64);
impl_num!(i128);
impl_num!(usize);
impl_num!(u8);
impl_num!(u16);
impl_num!(u32);
impl_num!(u64);
impl_num!(u128);