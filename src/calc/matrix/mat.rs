//! The `mat` module provides the [`Matrix`] struct, a utility for storing a 2d grid. 
//! The structure provides many functionalities, such as serde `Serialize` and `Deserialize`, as well as many mathematical operations.
//! To see the list of provided operators, see the [`super::ops`] module.
//! 
//! Note on deserialization: The matrix will check the internal buffer to ensure it is in proper format. 
//! Deserialization will fail if the check does not pass. 

/*
    Copyright 2025 Hollan Sellars, Dr. Dipali Swain

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/

use serde::de::{self, MapAccess, SeqAccess, Visitor};
use serde::{Serialize, Deserialize};

use std::fmt::{Display, Debug, Formatter};
use std::ops::{Add, Deref, DerefMut, Index, IndexMut, Mul};
use std::marker::PhantomData;
use std::str::FromStr;

use super::prelude::{matrix_determinant, print_matrix, MatrixLike, MatrixRowStorage, DeterminantComputable};
use super::super::num::{NullIdentity, UnitIdentity};
use super::extract::MatrixRef;
use super::err::{OutOfRangeError, MatrixConversionError, MatrixParseError};

/// Constructs and stores a 2d grid of numbers, with a specified number of rows and columns.
/// This handles the creation, maintenance, access, and memory for storing the values.
/// This structure is written very generically, and more functionality is provided given the abilities of `T`. 
/// Although not explicity required, almost all operations require that `T` is `Clone`. 
/// For arethmatic operations, it is assumed that `T.clone()` is inexpensive, and having complex cloning can slow down this structure.
/// Each use of `Clone` is described.
#[derive(Serialize, Debug, PartialEq, Hash, Eq)] //Note that deserialize also does a check to verify that the data is grid like, so it should be kept.
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

impl<T> TryFrom<Vec<Vec<T>>> for Matrix<T> {
    type Error = MatrixConversionError;
    /// Performs the conversion from `Vec<Vec<T>>` to [`Matrix`]. If the input data is invalid, it will return [`MatrixConversionError`].
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
impl<T> FromStr for Matrix<T> where T: FromStr {
    type Err = MatrixParseError<<T as FromStr>::Err>;
    fn from_str(raw: &str) -> Result<Self, Self::Err> {
        /*
            The table is in the form
            [[...], [...], ...]

            So we do not know the order or the elements count for each row. We must assume this each time.
        */
        let mut raw_table = raw.trim();
        if !raw_table.starts_with('[') || !raw_table.ends_with(']') {
            return Err( MatrixParseError::NonOpenedMatrix );
        }

        raw_table = &raw_table[1..(raw_table.len() - 1)]; //Remove the [ ] surrounding the data.
        if raw_table.is_empty() {
            return Err( MatrixParseError::EmptyMatrix );
        }

        let mut table: Vec<Vec<T>> = vec![];
        let mut current: Vec<T> = vec![];
        let mut in_a_set: bool = false;
        let mut current_value = String::new();

        for char in raw_table.chars() {
            match char {
                '[' => {
                    if in_a_set {
                        return Err( MatrixParseError::OpeningSetInSet );
                    }
                    if !current.is_empty() {
                        return Err( MatrixParseError::ClosingSetWithoutOpeningSet );
                    }

                    in_a_set = true;
                }
                ',' => {
                    if in_a_set {
                        let parsed: T = current_value.parse()
                                .map_err(MatrixParseError::Inner)?;

                        current_value.clear();
                        current.push(parsed);
                    }
                    else {
                        let mut new_current = vec![];
                        std::mem::swap(&mut new_current, &mut current);
                        table.push(new_current);
                    }
                }
                ']' => {
                    if !in_a_set {
                        return Err( MatrixParseError::ClosingSetWithoutOpeningSet );
                    }

                    if !current_value.is_empty() {
                        let parsed: T = current_value.parse()
                                .map_err(MatrixParseError::Inner)?;

                        current_value.clear();
                        current.push(parsed);
                    }

                    in_a_set = false;
                },
                x if x.is_whitespace() => continue,
                x=> {
                    if !in_a_set {
                        return Err( MatrixParseError::ValueOutsideOfRow );
                    }

                    current_value.push(x)
                },
            }
        }

        table.push(current);
        
        Self::try_from(table)
            .map_err(MatrixParseError::Conv)
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

impl<T> IntoIterator for Matrix<T> {
    type IntoIter = std::vec::IntoIter<Vec<T>>;
    type Item = Vec<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}
impl<'a, T> IntoIterator for &'a Matrix<T> {
    type IntoIter = MatrixIter<'a, T>;
    type Item = &'a [T];

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl<'a, T> IntoIterator for &'a mut Matrix<T> {
    type IntoIter = MatrixIterMut<'a, T>;
    type Item = &'a mut [T];
    
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
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
#[test]
fn matrix_tester() {
    let a = Matrix::try_from(vec![vec![1.0, 3.0, 6.0], vec![-1.0, 4.0, 1.0], vec![6.0, 2.0, 4.0]]).unwrap();
    let b = Matrix::try_from(vec![vec![0.0, 4.0, 6.0], vec![-1.0, -2.0, -1.0], vec![1.0, -3.0, 6.0]]).unwrap();
    let c = Matrix::identity(2);
    let d = Matrix::try_from(vec![vec![1.0, 3.0], vec![-4.0, 1.0], vec![7.0, 6.0]]).unwrap();
    let e = Matrix::try_from(vec![vec![4.0, 1.0, 9.0], vec![7.0, -2.0, 1.0]]).unwrap();
    let f = Matrix::try_from(vec![vec![2.0, 6.0], vec![1.0, 4.0]]).unwrap();
    let s = 4.0;

    assert!(a.is_square() && !a.is_empty());
    assert!(Matrix::<u8>::default().is_empty());

    match (a.clone() + b, Matrix::try_from(vec![vec![1.0, 7.0, 12.0], vec![-2.0, 2.0, 0.0], vec![7.0, -1.0, 10.0]])) {
        (Ok(m1), Ok(m2)) => assert_eq!(m1, m2),
        (a, b) => panic!("Expected (ok, ok), got ({:?}, {:?})", a, b)
    }

    assert_eq!(d * e, Ok( Matrix::try_from(vec![vec![25.0, -5.0, 12.0], vec![-9.0, -6.0, -35.0], vec![70.0, -5.0, 69.0]]).unwrap() ));
    assert_eq!(c.clone() * f.clone(), Ok(f));
    assert_eq!(c * s, Matrix::try_from(vec![vec![4.0, 0.0], vec![0.0, 4.0]]).unwrap());
}

#[test]
fn text_matrix_parse() {
    let cases = [
        ("[]", None),
        ("", None),
        ("[[][]]", Some(Matrix::try_from(vec![vec![]]).unwrap())),
        ("[[],[]]", Some(Matrix::try_from(vec![vec![], vec![]]).unwrap())),
        ("[[1, 1], [2, 2]]", Some(Matrix::try_from(vec![vec![1, 1], vec![2, 2]]).unwrap())),
        ("[[1, 1], [], [2, 2]]", None),
        ("[[a, b]]", None)
    ];

    for (raw, result) in cases {
        let parsed = Matrix::from_str(raw).ok();

        assert_eq!(parsed, result, "Raw: {raw}");
    }
}

#[test]
fn rref_tester() {
    let mut l = Matrix::try_from(vec![vec![1.0, 4.0, 9.0], vec![-2.0, 1.0, 0.0], vec![0.0, -3.0, -6.0]]).unwrap();
    l.row_echelon_form();
    let as_ref = Matrix::try_from(vec![vec![1.0, 4.0, 9.0], vec![0.0, 1.0, 2.0], vec![0.0, 0.0, 0.0]]).unwrap();
    assert_eq!(l, as_ref);

    let mut m = Matrix::try_from(vec![vec![1.0, 4.0, 9.0], vec![0.0, 0.0, 0.0], vec![0.0, 1.0, 2.0]]).unwrap();
    m.row_echelon_form();
    assert_eq!(m, as_ref);

    let mut r = Matrix::try_from(vec![vec![0.0, 0.0, 0.0], vec![4.0, 8.0, 16.0], vec![0.0, 0.0, 0.0]]).unwrap();
    r.row_echelon_form();
    assert_eq!(r, Matrix::try_from(vec![vec![1.0, 2.0, 4.0], vec![0.0, 0.0, 0.0], vec![0.0, 0.0, 0.0]]).unwrap());

    let mut t = Matrix::try_from(vec![vec![1.0, 0.0, 0.0, 0.0, 0.0], vec![0.0, 0.0, 0.0, 0.0, 1.0], vec![0.0, 1.0, 0.0, 0.0, 0.0]]).unwrap();
    t.row_echelon_form();
    assert_eq!(t, Matrix::try_from(vec![vec![1.0, 0.0, 0.0, 0.0, 0.0], vec![0.0, 1.0, 0.0, 0.0, 0.0], vec![0.0, 0.0, 0.0, 0.0, 1.0]]).unwrap());

    l.reduced_row_echelon_form();
    let rref = Matrix::try_from(vec![vec![1.0, 0.0, 1.0], vec![0.0, 1.0, 2.0], vec![0.0, 0.0, 0.0]]).unwrap();
    assert_eq!(l, rref);
}