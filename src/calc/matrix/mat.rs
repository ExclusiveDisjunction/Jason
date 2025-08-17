use serde::de::{self, MapAccess, SeqAccess, Visitor};
use serde::{Serialize, Deserialize};

use std::fmt::{Display, Debug, Formatter};
use std::ops::{Add, Deref, DerefMut, Div, Index, IndexMut, Mul, Neg};
use std::marker::PhantomData;

use crate::calc::err::{DimensionError, OutOfRangeError};
use crate::calc::num::{DeterminantComputable, Incrementable, NullIdentity, UnitIdentity};
//use crate::calc::{CalcError, OperationError, VariableData, VariableType};
use super::base::{matrix_determinant, matrix_eq, print_matrix, MatrixLike, MatrixRowStorage};
use super::extract::MatrixRef;

/// Constructs and stores a 2d grid of numbers, with a specified number of rows and columns.
/// This handles the creation, maintenance, access, and memory for storing the values.
/// This structure is written very generically, and more functionality is provided given the abilities of `T`. 
/// Although not explicity required, almost all operations require that `T` is `Clone`. 
/// For arethmatic operations, it is assumed that `T.clone()` is inexpensive, and having complex cloning can slow down this structure.
/// Each use of `Clone` is described.
#[derive(Serialize, Debug)] //Note that deserialize also does a check to verify that the data is grid like, so it should be kept.
pub struct Matrix<T> {
    data: Vec<Vec<T>>
}
impl<T> Default for Matrix<T> {
    fn default() -> Self {
        Self {
            data: vec![]
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct MatrixConversionError {
    pub expected: usize,
    pub found: usize
}
impl Display for MatrixConversionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "unable to construct a matrix out of the list provided. all rows were expected to be {} item(s) long, but one row was {} item(s) long", self.expected, self.found)
    }
}
impl std::error::Error for MatrixConversionError { }
impl MatrixConversionError {
    pub fn new(expected: usize, found: usize) -> Self {
        Self {
            expected,
            found
        }
    }
}

impl<T> TryFrom<Vec<Vec<T>>> for Matrix<T> {
    type Error = MatrixConversionError;
    fn try_from(value: Vec<Vec<T>>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Ok( Self::default() )
        }
        else {
            let row_size: usize = value[0].len();
            for row in &value {
                if row.len() != row_size {
                    return Err(MatrixConversionError::new(row_size, row.len()));
                }
            }

            Ok(
                Self {
                    data: value
                }
            )
        }
    }
}
impl<T> From<MatrixRef<'_, T>> for Matrix<T> where T: Clone {
    fn from(value: MatrixRef<'_, T>) -> Self {
        if value.is_empty() {
            return Self::default()
        }

        Self {
            data: value.iter().map(|x| x.iter().cloned().collect() ).collect()
        }
    }
}

impl<T> Index<usize> for Matrix<T> {
    type Output = [T];
    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}
impl<T> IndexMut<usize> for Matrix<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index]
    }
}

impl<T> Display for Matrix<T> where T: Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        print_matrix(f, self)
    }
}
impl<T> Clone for Matrix<T> where T: Clone {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone()
        }
    }
}
impl<T> PartialEq for Matrix<T> where T: PartialEq {
    fn eq(&self, other: &Self) -> bool {
       matrix_eq(self, other)
    }
} 

impl<'a, T> MatrixLike<'a> for Matrix<T> where T: 'a {
    type Storage = T;
    type RowStorage = [T];
    type Extraction = MatrixRef<'a, T>;

    type Iter = MatrixIter<'a, T>;
    
    fn iter(&'a self) -> Self::Iter {
        MatrixIter {
            iter: self.data.iter()
        }
    }

    fn rows(&self) -> usize {
        self.data.len()
    }
    fn cols(&self) -> usize {
        if self.data.is_empty() {
            0
        }
        else {
            self.data[0].len()
        }
    }

    fn extract<A, B>(&'a self, rows: A, cols: B) -> MatrixRef<'a, T> where A: IntoIterator<Item = usize>, B: IntoIterator<Item = usize> {
        MatrixRef::new(self, rows, cols)
    }

    fn determinant(&self) -> Option<Self::Storage> where Self::Storage: NullIdentity + DeterminantComputable + Clone {
        matrix_determinant(self)
    }
}

/// The fields that `Matrix<T>` stores.
#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
enum MatrixFields {
    Data
}

/// The `Visitor` used by `Matrix<T>` to deserialize.
struct MatrixVisitor<T> {
    _mark: PhantomData<T>
}
impl<T> Default for MatrixVisitor<T> {
    fn default() -> Self {
        Self {
            _mark: PhantomData
        }
    }
}
impl<'de, T> Visitor<'de> for MatrixVisitor<T> where T: Deserialize<'de> {
    type Value = Matrix<T>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("struct Matrix<T>")
    }

    fn visit_seq<V: SeqAccess<'de>>(self, mut seq: V) -> Result<Matrix<T>, V::Error> {
        //Data

        let data: Vec<Vec<T>> = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(0, &self))?;

        let result: Matrix<T> = data.try_into().map_err(|_| de::Error::custom("Expecting a grid of values, got irregularly sized data"))?;
        Ok(
            result 
        )
    }
    fn visit_map<V: MapAccess<'de>>(self, mut map: V) -> Result<Matrix<T>, V::Error> {
        let mut data = None;

        while let Some(key) = map.next_key()? {
            match key {
                MatrixFields::Data => {
                    if data.is_some() {
                        return Err(de::Error::duplicate_field("data"))
                    }

                    data = Some(map.next_value()?);
                }
            }
        }

        let data: Vec<Vec<T>> = data.ok_or_else(|| de::Error::missing_field("data"))?;

        let result = data.try_into().map_err(|_| de::Error::custom("Expecting a grid of values, got irregularly sized data"))?;
        Ok(
            result
        )
    }
}

impl<'de, T> Deserialize<'de> for Matrix<T> where T: Deserialize<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de> {
        
        const FIELDS: &[&str] = &["data"];
        deserializer.deserialize_struct("Matrix<T>", FIELDS, MatrixVisitor::default())
    }
}

impl<T> IntoIterator for Matrix<T> {
    type IntoIter = std::vec::IntoIter<Vec<T>>;
    type Item = Vec<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

impl<T> Matrix<T> {
    /// Directly stores the value of a `Vec<Vec<T>>` into the matrix. For safety reasons, this is a module only function. 
    /// To have a safe alternative, use `Matrix::<T>::try_from(val: Vec<Vec<T>>)`. 
    pub(super) fn direct(data: Vec<Vec<T>>) -> Self {
        Self {
            data
        }
    }

    /// Determines if either row value is out of bounds, returning `IndexOutOfRangeError` if it is.
    fn rows_oob(&self, a: usize, b: usize) -> Result<(), OutOfRangeError<usize>> {
        if a >= self.rows() {
            return Err(OutOfRangeError::new(a))
        }
        else if b >= self.rows() {
            return Err(OutOfRangeError::new(b))
        }

        Ok(())
    }
    /// Swaps the values of two rows. 
    pub fn row_swap(&mut self, orig: usize, dest: usize) -> Result<(), OutOfRangeError<usize>> {
        self.rows_oob(orig, dest)?;

        if orig != dest {
            self.data.swap(orig, dest);
        }

        Ok(())
    }

    /// Safely retrievs a row reference.
    pub fn get_row(&self, index: usize) -> Option<&[T]> {
        if index >= self.rows() {
            None
        }
        else {
            Some(&self[index])
        }
    }
    /// Safely retrievs a mutable row reference.
    pub fn get_row_mut(&mut self, index: usize) -> Option<&mut [T]> {
        if index >= self.rows() {
            None
        }
        else {
            Some(&mut self[index])
        }
    }
    /// Returns an iterator over all rows that allows mutation of stored data. 
    /// Note this does not allow for resizing the matrix.
    pub fn iter_mut(&mut self) -> MatrixIterMut<'_, T> {
        MatrixIterMut {
            iter: self.data.iter_mut()
        }
    }
}
impl<T> Matrix<T> where T: Clone {
    /// Creates the matrix, assigns `rows` x `cols` to be the size, and fills it with `v`. 
    /// In this function, `v` is cloned for each element.
    pub fn allocate(rows: usize, cols: usize, v: T) -> Self {
        let mut result = Self::default();
        result.resize(rows, cols, v);

        result
    }
    /// Resizes the matrix to a new size. 
    /// If either `rows` or `cols` is zero, then it will make the matrix empty.
    /// If the size is unchanged, it sets all elements to be `v`. 
    /// Otherwise, it resizes the matrix and sets all elements to be `v`.
    /// In this function, `v` is cloed for each element.
    pub fn resize(&mut self, rows: usize, cols: usize, v: T) where T: Clone {
        let currs = self.dimension();

        if currs == (rows, cols) {
            for row in &mut self.data {
                for item in row {
                    *item = v.clone();
                }
            }
        }
        else if rows == 0 || cols == 0 {
            self.data = vec![];
        }
        else {
            self.data = vec![vec![v; cols]; rows];
        }
    }

    /// Transposes the matrix. Esentially, the matrix's rows will become columns, and vice versa. 
    /// In practice the elements are flipped around the matrix' diagonal. 
    pub fn transpose(&self) -> Self {
        if self.is_empty() {
            return self.clone()
        }
         
        let mut result = Self::allocate(self.cols(), self.rows(), self[0][0].clone());

        for i in 0..self.rows() {
            for j in 0..self.cols() {
                result[j][i] = self[i][j].clone();
            }
        }

        result
    }
    /// Combines two same-row matricies (but not same column) together into one matrix. The resultant matrix will have:
    /// 1) the same rows as both `self` and `rhs`.
    /// 2) The sum of `self.cols() + rhs.cols()` for its cols.
    /// ```
    /// let lhs = Matrix::try_from(vec![vec![1, 2], vec![3, 4]]).unwrap();
    /// let rhs = Matrix::try_from(vec![vec![5], vec![6]]).unwrap();
    /// 
    /// let aug = lhs.agument(&rhs).unwrap();
    /// assert_eq!(aug, Matrix::try_from(vec![vec![1, 2, 5], vec![3, 4, 6]]).unwrap());
    /// assert_eq!(aug.rows(), lhs.rows());
    /// assert_eq!(aug.cols(), lhs.cols() + rhs.cols());
    /// ```
    /// This will fail if and only if:
    /// 1) `self` or `rhs` is empty
    /// 2) `self.rows() != rhs.rows()`
    /// 
    /// Since the elements are stored behind a reference, the elements of both `self` and `rhs` are cloned into the new matrix. 
    /// If cloning is not cheap, do not use this function.
    pub fn augment(&self, rhs: &Self) -> Result<Self, DimensionError<usize>> {
        let lhs = self;
        if lhs.rows() != rhs.rows() || lhs.is_empty() || rhs.is_empty() {
            return Err(DimensionError::new(lhs.rows(), rhs.rows()))
        }

        let one_rows = lhs.rows();
        let one_cols = lhs.cols();
        let two_cols = rhs.cols();

        let mut result = Self::allocate(one_rows, one_cols + two_cols, lhs[0][0].clone());
        assert!(!result.is_empty());

        for i in 0..one_rows {
            let mut j: usize = 0;
            for sources_col in &lhs[i] {
                result[i][j] = sources_col.clone();
                j += 1;
            }

            for sources_col in &rhs[i] {
                result[i][j] = sources_col.clone();
                j += 1;
            }
        }
        
        Ok(result)
    }
}

impl<T> Matrix<T> where T: Add<Output=T> + Clone {
    /// Adds the elements of two rows.
    /// Clones the elements of the source and destination rows. 
    pub fn row_add(&mut self, orig: usize, dest: usize) -> Result<(), OutOfRangeError<usize>> {
        self.rows_oob(orig, dest)?;

        for j in 0..self.cols() {
            let amount = self[orig][j].clone() + self[dest][j].clone();
            self[dest][j] = amount;
        }

        Ok(())
    }

    /// Computes the sum of two rows, where row 2 is multiplied by `fac`, and stores the result in `orig`. 
    /// Clones the elements of the source and destination rows, as well as the factor for each multiplication.
    pub fn row_fac_add<V>(&mut self, orig: usize, fac: V, dest: usize) -> Result<(), OutOfRangeError<usize>> where V: Mul<T, Output=T> + Clone {
        self.rows_oob(orig, dest)?;

        for j in 0..self.cols() {
            let amount = fac.clone() * self[orig][j].clone() + self[dest][j].clone();
            self[dest][j] = amount;
        }

        Ok(())
    }
}

impl<T> Matrix<T> where T: UnitIdentity + NullIdentity + Clone {
    /// Creates a matrix, with all elements set to `T::null_id()`, but fills the diagonal to `T::unit_id()`.
    /// This will clone `T::null_id()` and `T::unit_id()` a total of `dim x dim` times.
    pub fn identity(dim: usize) -> Self {
        let mut result = Self::allocate(dim, dim, T::null_id());
        for i in 0..dim {
            result[i][i] = T::unit_id();
        }

        result
    }
}

impl<T> Matrix<T> where T: Clone + PartialEq + UnitIdentity + NullIdentity + Neg<Output = T> + Add<Output=T> + Mul<Output=T> + Div<Output=T> {
    /// Puts the matrix into reduced row echelon form. This should not return Err. If it does, please report this issue.
    pub fn row_echelon_form(&mut self) -> Result<(), OutOfRangeError<usize>> {
        if self.is_empty() {
            return Ok(());
        }

        let rows = self.rows();
        let cols = self.cols();

        for i in 0..rows {
            let mut pivot_col = None;
            for col in 0..cols {
                if self.data[i][col] != T::null_id() {
                    pivot_col = Some(col);
                    break;
                }
            }

            if let Some(p) = pivot_col {
                if self.data[i][p] != T::unit_id() {
                    let fac = self.data[i][p].clone(); //We need to reduce the pivot to one.
                    for k in 0..cols {
                        let result = self.data[i][k].clone() / fac.clone();
                        self.data[i][k] = result;
                    }
                }

                for below_row in (i+1)..rows { //Every row under our row needs to have the 'p' value eliminated.
                    let fac = -self.data[below_row][p].clone();
                    if fac == T::null_id() || fac == -T::null_id(){
                        continue;
                    }

                    self.row_fac_add(p, fac, below_row)?;
                }
            }
        }

        let mut rows: usize = rows;

        let mut i: usize = 0;
        loop {
            if i >= rows || i >= cols {
                break;
            }

            let mut pivot = None;
            for j in 0..cols {
                if self.data[i][j] != T::null_id() {
                    pivot = Some(j);
                    break;
                }
            }

            let switch_with_last: bool;

            if let Some(p) = pivot {
                if p < rows && p > i {
                    self.row_swap(p, i)?;
                    i += 1;
                    switch_with_last = false;
                }
                else if p >= rows && p < cols {
                    switch_with_last = true;
                }
                else {
                    switch_with_last = false;
                    i += 1;
                }
                
            }
            else {
                switch_with_last = true;
            }

            if switch_with_last {
                let with = rows - 1;
                if with != i && with < rows {
                    self.row_swap(with, i)?;
                    rows -= 1; //This shrinks the rows, so that we dont double count that row.
                }
                else {
                    i += 1; //We increment when we dont swap with a zero row. 
                }
            }
        }

        Ok(())
    }
    /// Puts the matrix into reduced row echelon form. This should not return an error, and if it does, please report it.
    pub fn reduced_row_echelon_form(&mut self) -> Result<(), OutOfRangeError<usize>>{
        if self.is_empty() {
            return Ok(());
        }

        self.row_echelon_form()?;

        /*

            The row echelon form provides the following functionalities:
                1. All pivots are one.
                2. All values under a pivot are zero.
                3. All zeroes rows are at the bottom.
                4. All pivots are in decending order. 

            Therefore, to convert this into reduced row echelon form:
                1. All values above a pivot must be zero.
         */

        let rows = self.rows();
        let cols = self.cols();

        for i in 0..rows {
            //Pivot is a column position
            let mut pivot = None;
            for j in 0..cols {
                if self.data[i][j] != T::null_id() {
                    pivot = Some(j);
                    break;
                }
            }

            if let Some(p) = pivot {
                //We need to go above this pivot
                for row in 0..p {
                    let fac = -self.data[row][p].clone();
                    self.row_fac_add(i, fac, row)?;
                }
            }
        }

        Ok(())
    }

    /// Using the reduced row echelon form, computes the inverse of this matrix. 
    /// If the current matrix is `A`, then this finds `A^-1`, such that `A * A^-1 == A^-1 * A == I_n`, where `n` is the dimension of the matrix.
    /// This will return `None` if:
    /// 1. The matrix is not square
    /// 2. The determinant is zero
    /// 
    /// Note that this does not compute the determinant. It uses the reduced row echelon form for computation.
    /// To compute this value, the augment of `self` and `I_n` must be created. This will clone all elements of the current matrix,
    /// and will clone `T::null_id()`, `T::unit_id()` `n x n` times.
    pub fn inverse(&self) -> Option<Self> where T: PartialEq {
        if !self.is_square() { return None; }
        if self.is_empty() { return Some(Self::default()); }

        let identity = Self::identity(self.rows());
        let mut augmented = match Self::augment(&identity, self) {
            Ok(m) => m,
            Err(_) => return None
        };

        augmented.reduced_row_echelon_form().ok()?;

        let cols_index = 0..self.rows();
        let left_index = 0..self.rows();
        let right_index = self.rows()..(2 * self.rows());

        let left = augmented.extract(left_index, cols_index.clone());
        let right = augmented.extract(right_index, cols_index);

        if !matrix_eq(&right, &identity) {
            None
        }
        else {
            Some( left.into() )
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct NonSquareError;
impl Display for NonSquareError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("the matrix is not square")
    }
}
impl std::error::Error for NonSquareError { }

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum MatrixPowError {
    NonSquare,
    NonInvertable
}
impl Display for MatrixPowError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            match self {
                Self::NonInvertable => "the matrix is non-invertable, but the power provided is negative",
                Self::NonSquare => "the matrix is not square"
            }
        )
    }
}
impl std::error::Error for MatrixPowError { }

impl<T> Matrix<T> where T: Clone + NullIdentity + UnitIdentity + Add<Output=T> + Mul<Output=T> {
    /// Applies the pow operation on the specified matrix until a counter (starting at `P::default()`) reaches `amount`.
    fn pow_base<P>(mut on: Matrix<T>, amount: P) -> Result<Self, NonSquareError> where P : Default + Incrementable + PartialOrd {
        if !on.is_square() {
            return Err(NonSquareError)
        }
        if on.is_empty() {
            return Ok(on)
        }

        let mut countdown = P::default();

        if amount == countdown {
            return Ok( Self::identity(on.rows() ) )
        }

        countdown.increment(); //If we want to raise to the 2nd power, we apply this loop once. 
        //So, for any given `amount`, we run this `amount - 1` times.
        while countdown < amount {
            on = (&on * &on).unwrap(); //Since matrix multiplcation can only fail IF the matrix is not of the right dimensions, but square matricies can always be multiplied... this is redundant. 
            countdown.increment();
        }

        Ok( on )
    }

    /// Computes the result of multiplying `self` `amount` times.
    pub fn powi<P>(&self, amount: P) -> Result<Self, NonSquareError> where P : Default + Incrementable + PartialOrd {
        Self::pow_base(self.clone(), amount)
    }

    /// Computes the result of multiplying `self` `amount` times, considering if `amount == P::default()`, `amount == P::unit_id()` or if `amount < P::default()`.
    /// If `amount < P::default()`, then the inverse of the matrix will be computed instead, and the operation placed on it. 
    /// Due to this, the value of `T` must support inverse operations. Please see `Matrix<T>::inverse()` for more info.
    pub fn powf<P>(&self, mut amount: P) -> Result<Self, MatrixPowError> 
    where P : Default + Incrementable + PartialOrd + UnitIdentity + Neg<Output=P> + Debug,
    T: Div<Output=T> + Neg<Output=T> + PartialEq {
        if !self.is_square() {
            return Err(MatrixPowError::NonSquare)
        }

        let acting: Self;

        if amount == P::default() {
            return Ok( Matrix::identity(self.rows()) );
        }
        else if amount == P::unit_id() {
            return Ok( self.clone() );
        }
        else if amount < P::default() {
            amount = amount.neg();
            acting = self.inverse().ok_or(MatrixPowError::NonInvertable)?;
        }
        else {
            acting = self.clone();
        }

        Ok( Self::pow_base(acting, amount).unwrap() ) //Since pow_base will return NonSquareError, and we already checked if our matrix is square, this will never fail.
    }
}

/// A data type that abstracts the iteration over a Matrix's rows. 
pub struct MatrixIter<'a, T> where T: 'a {
    iter: std::slice::Iter<'a, Vec<T>>
}
impl<'a, T> Iterator for MatrixIter<'a, T> where T: 'a {
    type Item = &'a [T];

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| x.deref())
    }
}

/// A data type that abstracts the iteration over a Matrix's rows, providing mutable access.
pub struct MatrixIterMut<'a, T> where T: 'a {
    iter: std::slice::IterMut<'a, Vec<T>>
}
impl<'a, T> Iterator for MatrixIterMut<'a, T> where T: 'a {
    type Item = &'a mut [T];

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| x.deref_mut())
    }
}

#[test]
fn matrix_serde_test() {
    use serde_json::{to_string, from_str};

    let mat = Matrix::<i32>::identity(2);
    let ser = to_string(&mat).expect("unable to serialize");
    let der: Result<Matrix<i32>, _>  = from_str(&ser);

    assert_eq!(der.ok(), Some( mat ));
}