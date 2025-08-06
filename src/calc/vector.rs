use super::num::NullIdentity;
use super::calc_error::DimensionError;
use std::ops::{Add, Deref, DerefMut, Div, Mul, Neg, Sub};
use std::fmt::{Display, Debug, Formatter};
use std::iter::zip;
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
pub struct MathVector<T> {
    data: Vec<T>
}

impl<T> Default for MathVector<T> {
    fn default() -> Self {
        Self {
            data: vec![]
        }
    }
}
impl<T1, T2> PartialEq<MathVector<T2>> for MathVector<T1> where T1: PartialEq<T2> {
    fn eq(&self, other: &MathVector<T2>) -> bool {
        if self.dim() != other.dim() { return false; }

        zip(self.data.iter(), other.data.iter())
            .all(|(a, b)| a == b)
    }
}
impl<T> Display for MathVector<T> where T: Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let joined: Vec<String> = self.data.iter().map(|x| x.to_string()).collect();
        write!(f, "[ {} ]", joined.join(", "))
    }
}
impl<T> Debug for MathVector<T> where T: Debug {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let joined: Vec<String> = self.data.iter().map(|x| format!("{x:?}")).collect();
        write!(f, "[ {} ]", joined.join(", "))
    }
}
impl<T> Deref for MathVector<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl<T> DerefMut for MathVector<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T, S> From<S> for MathVector<T> where S: IntoIterator<Item = T> {
    fn from(value: S) -> Self {
        Self {
            data: value.into_iter().collect()
        }
    }
}
impl<T> MathVector<T> where T: Clone {
    pub fn allocate(size: usize, v: T) -> Self {
        Self {
            data: vec![v; size]
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum CrossProductError {
    Dim(usize, usize),
    Undef(usize)
}
impl Display for CrossProductError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dim(a, b) => write!(f, "the dimensions {a} and {b} do not match, and cross product cannot be performed"),
            Self::Undef(x) => write!(f, "cross product is not supported on vectors of size {x}")
        }
    }
}
impl std::error::Error for CrossProductError { }

impl<T> MathVector<T> where T: Clone + Sub<Output=T> + Mul<Output=T> + NullIdentity {
     pub fn cross_product(&self, rhs: &Self) -> Result<Self, CrossProductError> {
        if self.dim() != rhs.dim() {
            return Err(CrossProductError::Dim(self.dim(), rhs.dim()))
        }

        let a: (T, T, T);
        let b: (T, T, T);

        match self.dim() {
            2 => {
                a = (self.data[0].clone(), self.data[1].clone(), T::null_id());
                b = (rhs.data[0].clone(), rhs.data[1].clone(), T::null_id());
            }
            3 => {
                a = (self.data[0].clone(), self.data[1].clone(), self.data[2].clone());
                b = (rhs.data[0].clone(), rhs.data[1].clone(), rhs.data[2].clone());
            }
            x => {
                return Err(CrossProductError::Undef(x));
            }
        }

        /*
             i   j   k 
            a.0 a.1 a.2 
            b.0 b.1 b.2

            i(a.1 * b.2 - a.2 * b.1) - j(a.0 * b.2 - a.2 * b.0) + k(a.0 * b.1 - a.1 * b.0)
         */

        Ok(
            MathVector::from(
                vec![
                    a.1.clone() * b.2.clone() - a.2.clone() * b.1.clone(),
                    a.0.clone() * b.2.clone() - a.2.clone() * b.0.clone(),
                    a.0.clone() * b.1.clone() - a.1.clone() * b.0.clone()
                ]
            )
        )
    }
}
impl<T> MathVector<T> where T: Mul + Clone, <T as Mul>::Output: Add<Output = <T as Mul>::Output> + NullIdentity{
    pub fn dot_product(&self, rhs: &Self) -> Result<<T as Mul>::Output, DimensionError<usize>> {
        if self.dim() != rhs.dim() {
            return Err(DimensionError::new(self.dim(), rhs.dim()));
        }

        let mut result= <T as Mul>::Output::null_id();
        for (a, b) in zip(self.data.iter(), rhs.data.iter()) {
            result = result + a.clone() * b.clone();
        }

        Ok(result)
    }

     pub fn magnitude(&self) -> f64 where <T as Mul>::Output: Into<f64> {
        let result: f64 = self.dot_product(self).unwrap().into();
        result.sqrt() //note that the dot product returns error if the dims do not match, and when dot product with self, the dims cannot mismatch
    }
}
impl<T> MathVector<T> where T: Into<f64> + Clone {
    pub fn angle(&self) -> Option<f64> where T: Clone {
        if self.dim() == 2 {
            let b: f64 = self.data[1].clone().into();
            let a: f64 = self.data[0].clone().into();
            Some( b.atan2(a) )
        }
        else {
            None
        }
    }
}
impl<T> MathVector<T> {
    pub fn dim(&self) -> usize {
        self.data.len()
    }
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
    pub fn shape(&self) -> String {
        format!("(Vector:{})", self.dim())
    }
}

// VEC ONLY OPERATORS
// PURE
impl<T> Neg for MathVector<T> where T: Neg {
    type Output = MathVector<<T as Neg>::Output>;
    fn neg(self) -> Self::Output {
        let data = self.data.into_iter().map(|x| x.neg()).collect();

        MathVector {
            data
        }
    }
}
impl<T> Add for MathVector<T> where T: Add<Output=T> + Clone {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let mut a: Self;
        let b: Self;

        if self.dim() < rhs.dim() {
            a = rhs;
            b = self;
        }
        else {
            a = self;
            b = rhs;
        }

        for (i, elem) in b.data.into_iter().enumerate() {
            a.data[i] = a.data[i].clone() + elem;
        }

        a
    }
}
impl<T> Sub for MathVector<T> where T: Neg<Output=T> + Add<Output=T> + Clone {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        self + rhs.neg()
    }
}

//REF
impl<T> Neg for &MathVector<T> where T: Clone + Neg {
    type Output = MathVector<<T as Neg>::Output>;
    fn neg(self) -> Self::Output {
        let contents = self.data.iter().map(|x| x.clone().neg()).collect();
        MathVector {
            data: contents 
        }
    }
}
impl<'a, T> Add<&'a MathVector<T>> for &MathVector<T> where T: Add<Output=T> + Clone {
    type Output = MathVector<T>;
    fn add(self, rhs: &'a MathVector<T>) -> Self::Output {
        let mut result: MathVector<T>;
        let a: &MathVector<T>;
        let b: &MathVector<T>;

        if self.dim() < rhs.dim() {
            a = rhs;
            b = self;
        }
        else {
            a = self;
            b = rhs;
        }

        result = a.clone();
        for (i, elem) in b.data.iter().enumerate() {
            result.data[i] = result.data[i].clone() + elem.clone();
        }

        result

    }
}
impl<'a, T> Sub<&'a MathVector<T>> for &MathVector<T> where T: Neg<Output=T> + Add<Output=T> + Clone {
    type Output = MathVector<T>;
    fn sub(self, rhs: &'a MathVector<T>) -> Self::Output {
        self.clone() + rhs.clone().neg()
    }
}

// VEC - SCA OPERATORS
// PURE
// A B
impl<T> Mul<T> for MathVector<T> where T: Mul + Clone {
    type Output = MathVector<<T as Mul>::Output>;

    fn mul(self, rhs: T) -> Self::Output {
        self.data.into_iter().map(|x| x * rhs.clone()).collect::<Vec<<T as Mul>::Output>>().into()
    }
}
impl<T> Div<T> for MathVector<T> where T: Div + Clone {
    type Output = MathVector<<T as Div>::Output>;

    fn div(self, rhs: T) -> Self::Output {
        self.data.into_iter().map(|x| x / rhs.clone()).collect::<Vec<<T as Div>::Output>>().into()
    }
}

// REFERENCE
// A B
impl<T> Mul<T> for &MathVector<T> where T: Mul<Output=T> + Clone {
    type Output = MathVector<T>;
    fn mul(self, rhs: T) -> Self::Output {
        let result = self.data.iter().map(|x| x.clone() * rhs.clone()).collect();
        MathVector {
            data: result
        }
    }
}
impl<T> Div<T> for &MathVector<T> where T: Div<Output=T> + Clone {
    type Output = MathVector<T>;
    fn div(self, rhs: T) -> Self::Output {
        let result = self.data.iter().map(|x| x.clone() / rhs.clone()).collect();
        MathVector {
            data: result
        }
    }
}

/// A common data type used to store `f64` (doubles) in a vector.
pub type FloatVector = MathVector<f64>;
/// A common data type used to store `f64` (doubles) in a vector.
pub type ComplexVector = MathVector<crate::calc::Complex>;

#[test]
fn test_vector() {
    let a = FloatVector::from(vec![1.0, 2.0, 3.0]);
    let b = FloatVector::from(vec![2.0, 3.0]);
    let c = 4.0;

    //Negation
    assert_eq!(-a.clone(), MathVector::from(vec![-1.0, -2.0, -3.0]));

    //Index
    assert_eq!(a[0], 1.0);

    //Other operations
    assert!(a.dot_product(&b).is_err());
    assert_eq!(a.dot_product(&a).unwrap(), (1 * 1 + 2 * 2 + 3 * 3) as f64);
    assert_eq!(a.cross_product(&MathVector::from(vec![3.0, 2.0, 1.0])).unwrap(), MathVector::from(vec![-4.0, -8.0, -4.0]));
    assert_eq!(b.cross_product(&MathVector::from(vec![3.0, 2.0])).unwrap(), MathVector::from(vec![0.0, 0.0, -5.0]));

    //Properties
    assert_eq!(a.magnitude(), ((1 * 1 + 2 * 2 + 3 * 3) as f64).sqrt());
    assert!(a.angle().is_none());
    //Due to floating point error, this may fail. 
    assert_eq!(b.angle().unwrap(), 3f64.atan2(2.0));

    // a + b == b + a
    assert_eq!(a.clone() + b.clone(), MathVector::from(vec![3.0, 5.0, 3.0]));
    assert_eq!(b.clone() + a.clone(), a.clone() + b.clone());

    // a - b != b - a
    assert_eq!(a.clone() - b.clone(), MathVector::from(vec![-1.0, -1.0, 3.0]));
    assert_eq!(b.clone() - a.clone(), MathVector::from(vec![1.0, 1.0, -3.0]));

    // a * b == b * a
    assert_eq!(a.clone() * c, MathVector::from(vec![4.0, 8.0, 12.0]));

    // b / a DNE
    assert_eq!(a.clone() / c, MathVector::from(vec![1.0/4.0, 2.0/4.0, 3.0/4.0]));

    //Assert that &a + &b == a + b, and other operations
    assert_eq!(&a + &b, a.clone() + b.clone());
    assert_eq!(&a + &b, &b + &a);
    assert_eq!(&a - &b, a.clone() - b.clone());
    assert_eq!(&a * c, a.clone() * c);
    assert_eq!(&a / c, a.clone() / c);
}