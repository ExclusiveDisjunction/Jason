//! A collection of structures, functions, and traits to encode 2d data structures.
//! The core of this module is the structure [`Matrix`], which supports the storage of generic elements.
//! Additionally, the structures of this module gain functionality as the generic they are built on gains them.

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

pub mod err;
pub mod extract;
pub mod mat;
pub mod ops;
pub mod prelude;

use std::ops::DerefMut;

pub use err::*;
pub use extract::*;
pub use mat::*;
pub use prelude::*;

/// A common data type used to store `f64` (doubles) in a matrix.
pub type FloatMatrix = Matrix<f64>;
/// A common data type used to store `f64` (doubles) in a matrix extraction.
pub type FloatMatrixRef<'a> = MatrixRef<'a, f64>;

pub fn flatten_copy<'a, T, A>(matrix: &'a T, output: &mut A)
where
    T: MatrixLike<'a>,
    T::Storage: Clone,
    A: DerefMut<Target = [<T as MatrixLike<'a>>::Storage]>,
{
    let total_len = matrix.rows() * matrix.cols();
    let output = output.deref_mut();
    assert_eq!(total_len, output.len(), "The sizes must be equal for the copy to work.");

    let mut k = 0;
    for row in matrix.iter() {
        for elem in row.iter() {
            output[k] = elem.clone();
            k += 1;
        }
    }
}
pub fn flatten_map_copy<'a, T, V, F, A>(matrix: &'a T, output: &mut A, mut f: F)
where
    T: MatrixLike<'a>,
    F: FnMut(&T::Storage) -> V,
    A: DerefMut<Target = [V]>,
{
    let total_len = matrix.rows() * matrix.cols();
    let output = output.deref_mut();
    assert_eq!(total_len, output.len(), "The sizes must be equal for the copy to work.");

    let mut k = 0;
    for row in matrix.iter() {
        for elem in row.iter() {
            output[k] = f(elem);
            k += 1;
        }
    }
}
