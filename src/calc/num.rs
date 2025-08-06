use std::ops::{Add, AddAssign, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign, Div};

/// Represents a mathematical object such that `null_id()` + `x` == `x` + `null_id()` == `x`.
pub trait NullIdentity : Sized {
    fn null_id() -> Self;
}
/// Represents a mathematical object such that `unit_id()` * `x` == `x` * `unit_id()` == `x`.
pub trait UnitIdentity: Sized {
    fn unit_id() -> Self;
}
/// Represents a type that has a `sqrt` function.
pub trait SqrtComputable: Sized {
    fn sqrt_comp(&self) -> Self;
}

impl SqrtComputable for f64 {
    fn sqrt_comp(&self) -> Self {
        self.sqrt()
    }
}
impl SqrtComputable for f32 {
    fn sqrt_comp(&self) -> Self {
        self.sqrt()
    }
}

macro_rules! create_null_unit {
    ($item: ty, $v: expr, $n: expr) => {
        impl NullIdentity for $item {
            fn null_id() -> $item {
                $v
            }
        }

        impl UnitIdentity for $item {
            fn unit_id() -> $item {
                $n
            }
        }
    }
}

create_null_unit!(i8,   0i8  , 1i8  );
create_null_unit!(i16,  0i16 , 1i16 );
create_null_unit!(i32,  0i32 , 1i32 );
create_null_unit!(i64,  0i64 , 1i64 );
create_null_unit!(i128, 0i128, 1i128);

create_null_unit!(u8,   0u8  , 1u8  );
create_null_unit!(u16,  0u16 , 1u16 );
create_null_unit!(u32,  0u32 , 1u32 );
create_null_unit!(u64,  0u64 , 1u64 );
create_null_unit!(u128, 0u128, 1u128);

create_null_unit!(isize, 0isize, 1isize);
create_null_unit!(usize, 0usize, 1usize);

create_null_unit!(f32, 0.0f32, 1.0f32);
create_null_unit!(f64, 0.0f64, 1.0f64);

/// Represents a data type that can be used to compute the determinant of a Matrix. 
pub trait DeterminantComputable: 
    Sized + 
    NullIdentity + 
    Clone + 
    Mul<Self, Output=Self> + 
    Add<Self, Output=Self> + 
    Sub<Self, Output=Self> + 
    Neg<Output=Self> { }

impl<T> DeterminantComputable for T where T: Sized + NullIdentity + Clone + Mul<Output=T> + Add<Output=T> + Sub<Output=T> + Neg<Output=T> {

}

pub trait AddClosure : Sized + Add<Output=Self> + AddAssign { }
pub trait SubClosure : Sized + Sub<Output=Self> + SubAssign { }
pub trait MulClosure : Sized + Mul<Output=Self> + MulAssign { }
pub trait DivClosure : Sized + Div<Output=Self> + DivAssign { }
pub trait NegClosure : Sized + Neg<Output = Self> { }

impl<T> AddClosure for T where T: Add<Output = T> + AddAssign { }
impl<T> SubClosure for T where T: Sub<Output = T> + SubAssign { }
impl<T> MulClosure for T where T: Mul<Output = T> + MulAssign { }
impl<T> DivClosure for T where T: Div<Output = T> + DivAssign { }
impl<T> NegClosure for T where T: Neg<Output=T> { }

pub trait RangedType : PartialEq + PartialOrd + Sized {
    fn min_value() -> Self;
    fn max_value() -> Self;

    fn is_min(&self) -> bool where Self: PartialEq {
        self == &Self::min_value()
    }
    fn is_max(&self) -> bool where Self: PartialEq {
        self == &Self::max_value()
    }
}

macro_rules! make_ranged_type {
    ($on: ty) => {
        impl RangedType for $on {
            #[inline]
            fn min_value() -> $on {
                <$on>::MIN
            }
            #[inline]
            fn max_value() -> $on {
                <$on>::MAX
            }
        }
    }
}

make_ranged_type!(i8  );
make_ranged_type!(i16 );
make_ranged_type!(i32 );
make_ranged_type!(i64 );
make_ranged_type!(i128);

make_ranged_type!(u8  );
make_ranged_type!(u16 );
make_ranged_type!(u32 );
make_ranged_type!(u64 );
make_ranged_type!(u128);

make_ranged_type!(isize);
make_ranged_type!(usize);

make_ranged_type!(f32);
make_ranged_type!(f64);

pub trait Incrementable {
    fn increment(&mut self);
}

macro_rules! make_increment {
    ($on: ty, $v: expr) => {
        impl Incrementable for $on {
            fn increment(&mut self) {
                self.add_assign($v);
            }
        }
    }
}

make_increment!(i8,   1i8  );
make_increment!(i16,  1i16 );
make_increment!(i32,  1i32 );
make_increment!(i64,  1i64 );
make_increment!(i128, 1i128);

make_increment!(u8,   1u8  );
make_increment!(u16,  1u16 );
make_increment!(u32,  1u32 );
make_increment!(u64,  1u64 );
make_increment!(u128, 1u128);

make_increment!(isize, 1isize);
make_increment!(usize, 1usize);

make_increment!(f32, 1.0f32);
make_increment!(f64, 1.0f64);