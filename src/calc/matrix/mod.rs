pub mod base;
pub mod extract;
pub mod mat;
pub mod ops;

pub use base::{MatrixDimension, MatrixLike};
pub use extract::MatrixRef;
pub use mat::{Matrix, MatrixConversionError};

use super::Complex;

/// A common data type used to store `f64` (doubles) in a matrix.
pub type FloatMatrix = Matrix<f64>;
/// A common data type used to store `f64` (doubles) in a matrix extraction.
pub type FloatMatrixRef<'a> = MatrixRef<'a, f64>;

/// A common data type used to store `Complex` numbers in a matrix.
pub type ComplexMatrix = Matrix<Complex>;
/// A common data type used to store `Complex` numbers in a matrix extraction.
pub type ComplexMatrixRef<'a> = MatrixRef<'a, Complex>;

#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn dimension_tester() {
        let a = MatrixDimension::new(1, 4);
        let b = MatrixDimension::new(3, 1);
        assert_ne!(a, b);
        assert_eq!(format!("{}", a), String::from("1x4"));
        assert_eq!(b, (3, 1));
        assert!(!a.is_empty());
        assert!(!a.is_square());
        assert!(MatrixDimension::new(1, 1).is_square());
    }

    #[test]
    fn extraction_tester() {
        let mat = FloatMatrix::identity(3);
        let extr = mat.as_extraction();
        assert_eq!(extr[0][0], mat[0][0]);
        assert_eq!(extr, mat);

        let sub_extr = extr.extract(0..1, 0..1);
        assert_eq!(sub_extr, mat.extract(0..1, 0..1));

        let sub_mat: FloatMatrix = sub_extr.clone().into();
        assert_eq!(sub_mat, sub_extr);
        assert_ne!(sub_extr, extr);

        let det = extr.determinant();
        assert_eq!(det, Some(1.0));
    }

    #[test]
    fn determinant_tester() {
        let a = Matrix::<i32>::try_from(vec![vec![1, 2], vec![2, 1]]).unwrap();
        let b = Matrix::<i32>::try_from(vec![vec![1, 0], vec![1, 0]]).unwrap();

        assert_eq!(a.determinant(), Some(1 * 1 - 2 * 2));
        assert_eq!(b.determinant(), Some(0));

        let c = Matrix::<i32>::try_from(vec![vec![1, 4, 6], vec![-1, 2, 4], vec![3, 1, 2]]).unwrap();

        assert_eq!(c.determinant(), Some(14))

    }

    #[test]
    fn matrix_tester() {
        use super::super::vector::MathVector;

        let a = FloatMatrix::try_from(vec![vec![1.0, 3.0, 6.0], vec![-1.0, 4.0, 1.0], vec![6.0, 2.0, 4.0]]).unwrap();
        let b = FloatMatrix::try_from(vec![vec![0.0, 4.0, 6.0], vec![-1.0, -2.0, -1.0], vec![1.0, -3.0, 6.0]]).unwrap();
        let c = FloatMatrix::identity(2);
        let d = FloatMatrix::try_from(vec![vec![1.0, 3.0], vec![-4.0, 1.0], vec![7.0, 6.0]]).unwrap();
        let e = FloatMatrix::try_from(vec![vec![4.0, 1.0, 9.0], vec![7.0, -2.0, 1.0]]).unwrap();
        let f = FloatMatrix::try_from(vec![vec![2.0, 6.0], vec![1.0, 4.0]]).unwrap();
        let v = MathVector::from(vec![1.0, 2.0, 3.0]);
        let s = 4.0;

        assert!(a.is_square() && !a.is_empty());
        assert!(FloatMatrix::default().is_empty());

        match (a.clone() + b, FloatMatrix::try_from(vec![vec![1.0, 7.0, 12.0], vec![-2.0, 2.0, 0.0], vec![7.0, -1.0, 10.0]])) {
            (Ok(m1), Ok(m2)) => assert_eq!(m1, m2),
            (a, b) => panic!("Expected (ok, ok), got ({:?}, {:?})", a, b)
        }

        assert_eq!(d * e, Ok( FloatMatrix::try_from(vec![vec![25.0, -5.0, 12.0], vec![-9.0, -6.0, -35.0], vec![70.0, -5.0, 69.0]]).unwrap() ));
        assert_eq!(c.clone() * f.clone(), Ok(f));
        assert_eq!(a * v, Ok(MathVector::from(vec![25.0, 10.0, 22.0])));
        assert_eq!(c * s, FloatMatrix::try_from(vec![vec![4.0, 0.0], vec![0.0, 4.0]]).unwrap());
    }

    #[test]
    fn rref_tester() {
        let mut l = FloatMatrix::try_from(vec![vec![1.0, 4.0, 9.0], vec![-2.0, 1.0, 0.0], vec![0.0, -3.0, -6.0]]).unwrap();
        l.row_echelon_form().unwrap();
        let as_ref =FloatMatrix::try_from(vec![vec![1.0, 4.0, 9.0], vec![0.0, 1.0, 2.0], vec![0.0, 0.0, 0.0]]).unwrap();
        assert_eq!(l, as_ref);

        let mut m = FloatMatrix::try_from(vec![vec![1.0, 4.0, 9.0], vec![0.0, 0.0, 0.0], vec![0.0, 1.0, 2.0]]).unwrap();
        m.row_echelon_form().unwrap();
        assert_eq!(m, as_ref);

        let mut r = FloatMatrix::try_from(vec![vec![0.0, 0.0, 0.0], vec![4.0, 8.0, 16.0], vec![0.0, 0.0, 0.0]]).unwrap();
        r.row_echelon_form().unwrap();
        assert_eq!(r, FloatMatrix::try_from(vec![vec![1.0, 2.0, 4.0], vec![0.0, 0.0, 0.0], vec![0.0, 0.0, 0.0]]).unwrap());

        let mut t = FloatMatrix::try_from(vec![vec![1.0, 0.0, 0.0, 0.0, 0.0], vec![0.0, 0.0, 0.0, 0.0, 1.0], vec![0.0, 1.0, 0.0, 0.0, 0.0]]).unwrap();
        t.row_echelon_form().unwrap();
        assert_eq!(t, FloatMatrix::try_from(vec![vec![1.0, 0.0, 0.0, 0.0, 0.0], vec![0.0, 1.0, 0.0, 0.0, 0.0], vec![0.0, 0.0, 0.0, 0.0, 1.0]]).unwrap());

        l.reduced_row_echelon_form().unwrap();
        let rref = FloatMatrix::try_from(vec![vec![1.0, 0.0, 1.0], vec![0.0, 1.0, 2.0], vec![0.0, 0.0, 0.0]]).unwrap();
        assert_eq!(l, rref);
    }

    #[test]
    fn complex_matrix_test() {
        let a = ComplexMatrix::try_from(vec![vec![(0.0, 1.0).into(), (1.0, 2.0).into()], vec![(1.0, 2.0).into(), (2.0, 2.0).into() ] ] ).unwrap();
        let b = ComplexMatrix::try_from(vec![vec![(1.0, 2.0).into(), (4.0, 2.0).into()], vec![(1.0, 2.0).into(), (-2.0, 2.0).into() ] ] ).unwrap();
        let c = ComplexMatrix::identity(2);
        let d = ComplexMatrix::try_from(vec![ vec![(1, 1).into(), (0, 0).into()], vec![(0, 0).into(), (1, 1).into()] ] ).unwrap();
        let scale = Complex::new(2.0, 0.0);

        assert_eq!(c.clone() * b.clone(), Ok(b.clone()));
        assert_eq!(a.clone() + b.clone(), Ok( ComplexMatrix::try_from(vec![vec![(1, 3).into(), (5, 4).into()], vec![(2, 4).into(), (0, 4).into() ] ] ).unwrap() ));
        assert_eq!(a.clone() - b.clone(), Ok( ComplexMatrix::try_from(vec![vec![(-1, -1).into(), (-3, 0).into()], vec![(0, 0).into(), (4, 0).into() ] ] ).unwrap() ));
        assert_eq!(-a.clone(), ComplexMatrix::try_from(vec![vec![(0, -1).into(), (-1, -2).into()], vec![(-1, -2).into(), (-2, -2).into() ] ] ).unwrap() );
        assert_eq!(d * scale, ComplexMatrix::try_from(vec![ vec![(2, 2).into(), (0, 0).into()], vec![(0, 0).into(), (2, 2).into()] ] ).unwrap());
    }
}