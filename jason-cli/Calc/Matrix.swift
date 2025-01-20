//
//  Matrix.swift
//  jason-cli
//
//  Created by Hollan on 1/17/25.
//

import Foundation

public struct MatrixRow {
    internal init(target: Matrix, offset: Int) {
        self.target = target
        self.offset = offset
    }
    
    private let target: Matrix
    private let offset: Int
    
    public var isValidRow: Bool {
        offset >= 0 && offset < target.rows;
    }
    
    public subscript(_ index: Int) -> Double {
        get {
            return target[offset, index]
        }
        set(v) {
            target[offset, index] = v
        }
    }
}

public struct MatrixExtraction<T: Collection> : Equatable where T.Element == Matrix.IndexType, T.Index == Int {
    internal init(target: Matrix, rows: T, cols: T) {
        self.target = target
        self.rows = rows
        self.cols = cols
    }
    
    public var intoMatrix: Matrix {
        get {
            var result = Matrix(rows: rows.count, cols: cols.count)
            var ourI: Int = 0;
            var ourJ: Int;
            
            for i in rows {
                ourJ = 0;
                for j in cols {
                    result.data[ourI][ourJ] = target.data[i][j];
                    ourJ += 1;
                }
                ourI += 1;
            }
            
            return result;
        }
    }
    
    private let target: Matrix;
    public let rows: T;
    public let cols: T;
    public var dims: MatrixDimension {
        get {
            MatrixDimension(rows: rows.count, cols: cols.count)
        }
    }
    
    public var determinant: Double? {
        get {
            guard dims.isSquare else { return nil; }
            
            if dims == (2, 2) {
                return self[0, 0] * self[1, 1] - self[0, 1] * self[1, 0];
            }
            else {
                var result: Double = 0;
                let firstRowsRemoved = [Int](rows[1..<rows.count]); //Since each minor will have the same first row removed, its better to get it here instead of computing it for each minor.
                for i in cols.indices {
                    // We exclude our current row and column.
                    let minor = MinorMatrixExtraction(target: self.target, rows: firstRowsRemoved, cols: cols.filter { $0 != cols[i] } )
                    guard let minorsDeterminant = minor.determinant else { return nil; }
                    
                    result += (i % 2 == 0 ? -1 : 1) * self[0, i] * minorsDeterminant;
                }
                
                return result;
            }
        }
    }
    
    public subscript(_ row: T.Index) -> MatrixRow {
        get {
            let offset = rows[row]
            return target[offset];
        }
    }
    public subscript(_ row: T.Index, _ col: T.Index) -> Double {
        get {
            let row_offset = rows[row];
            let col_offset = cols[col];
            
            return target[row_offset, col_offset];
        }
        set(v) {
            let row_offset = rows[row];
            let col_offset = cols[col];
            
            target[row_offset, col_offset] = v;
        }
    }
    public subscript(_ rows: [T.Index], _ cols: [T.Index]) -> MinorMatrixExtraction {
        get {
            let extracted_rows = rows.map { self.rows[$0] }
            let extracted_cols = cols.map { self.cols[$0] }
            
            return MatrixExtraction<[Int]>(target: self.target, rows: extracted_rows, cols: extracted_cols)
        }
    }
    
    public static func ==(lhs: MatrixExtraction<T>, rhs: MatrixExtraction<T>) -> Bool {
        guard lhs.dims == rhs.dims else { return false; }
        
        for i in lhs.rows.indices {
            for j in lhs.cols.indices {
                if lhs[i, j] != rhs[i, j] {
                    return false;
                }
            }
        }
        
        return true;
    }
    public static func ==(lhs: MatrixExtraction<T>, rhs: Matrix) -> Bool {
        guard lhs.dims == rhs.dims else { return false; }
        
        for i in lhs.rows.indices {
            for j in lhs.cols.indices {
                if lhs[i, j] != rhs[i, j] {
                    return false;
                }
            }
        }
        
        return true;
    }
    public static func ==(lhs: Matrix, rhs: MatrixExtraction<T>) -> Bool {
        return rhs == lhs;
    }
    public static func !=(lhs: MatrixExtraction<T>, rhs: Matrix) -> Bool {
        return !(lhs == rhs);
    }
    public static func !=(lhs: Matrix, rhs: MatrixExtraction<T>) -> Bool {
        return rhs != lhs;
    }
}

public typealias ContinuousMatrixExtraction = MatrixExtraction<Range<Int>>;
public typealias MinorMatrixExtraction = MatrixExtraction<[Int]>;

public struct MatrixDimension : Sendable, Equatable {
    public init(rows: Int, cols: Int) {
        self.rows = rows;
        self.cols = cols;
    }
    
    public let rows: Int;
    public let cols: Int;
    
    public var isSquare: Bool {
        get {
            rows == cols
        }
    }
    
    public static func ==(lhs: MatrixDimension, rhs: MatrixDimension) -> Bool {
        lhs.rows == rhs.rows && lhs.cols == rhs.cols
    }
    public static func ==(lhs: MatrixDimension, rhs: (Int, Int)) -> Bool {
        lhs.rows == rhs.0 && lhs.cols == rhs.1;
    }
}

public final class Matrix : VariableData {
    public init() {
        self.data = [];
    }
    public init(rows: Int, cols: Int, value: Double = 0.0) {
        self.data = [];
        self.allocate(rows: rows, cols: cols, value: value);
    }
    public init(dims: MatrixDimension, value: Double = 0.0) {
        self.data = [];
        self.allocate(rows: dims.rows, cols: dims.cols, value: value);
    }
    public init(from: Vector, asColMatrix: Bool) {
        if asColMatrix {
            let data = from.exposeData;
            self.data = [[Double]](repeating: [Double](repeating: 0, count: 1), count: data.count);
            for i in 0..<data.count {
                self.data[i][0] = data[i];
            }
        }
        else {
            self.data = [from.exposeData];
        }
    }
    public init<T: ScalarLike>(rows: Int, values: T...) throws {
        if values.count % rows != 0 {
            throw DimensionError(dimA: rows, dimB: values.count)
        }
        self.data = [[Double]](repeating: [], count: rows);

    
        var j: Int = 0;
        let jump: Int = values.count / rows;
        for i in 0..<rows {
            let slice = values[j..<(j+jump)].map(\.asDouble);
            self.data[i] = Array<Double>(slice)
            j += jump
        }
    }
    private init(inner: [[Double]]) {
        self.data = inner
    }
    
    // Supporting Types
    public typealias IndexType = [[Double]].Index;
    
    // VariableData implementations
    public static var type: VariableType { .matrix }
    public var displayString: String { "(Matrix:\(rows)x\(columns))" }
    public func clone() -> any VariableData {
        Matrix(inner: self.data)
    }
    
    // General Static Functions
    public static func Identity(dim: Int) -> Matrix {
        let result = Matrix(rows: dim, cols: dim);
        for i in 0..<dim {
            result.data[i][i] = 1;
        }
        
        return result;
    }
    
    // Management
    private func allocate(rows: Int, cols: Int, value: Double = 0) {
        let currs: (Int, Int) = (self.rows, self.columns);
        
        if (currs == (rows, cols)) { //We will just set the value for each element
            for i in self.data.indices {
                for j in self.data[i].indices {
                    self.data[i][j] = value
                }
            }
        }
        else {
            if rows == 0 || cols == 0 {
                self.data = [[Double]]();
            } else {
                self.data = [[Double]](repeating: [Double](repeating: value, count: cols), count: rows);
            }
        }
        
    }
    
    // Outputting
    private func getColSchematic() -> [(Bool, Int)] {
        return [];
    }
    private func getRowString(schema: [(Bool, Int)], row: Int, open: Character, close: Character) -> String? {
        return nil;
    }
    public var singleLinePrint: String {
        get {
            
        }
    }
    public var multiLinePrint: String {
        get {
            
        }
    }
    
    fileprivate var data: [[Double]]
    
    public subscript(_ row: Matrix.IndexType) -> MatrixRow {
        get {
            precondition(row >= 0 && row <= self.rows, "invalid row index")
            
            return MatrixRow(target: self, offset: row)
        }
    }
    public subscript(_ row: Matrix.IndexType, _ col: Matrix.IndexType) -> Double {
        get {
            precondition(row >= 0 && row < self.rows, "invalid row index")
            precondition(col >= 0 && col < self.columns, "invalid column index")
            
            return data[row][col];
        }
        set(v) {
            precondition(row >= 0 && row < self.rows, "invalid row index")
            precondition(col >= 0 && col < self.columns, "invalid column index")
            
            data[row][col] = v;
        }
    }
    
    // Matrix Information
    public func extract(rows: Range<Matrix.IndexType>, cols: Range<Matrix.IndexType>) -> ContinuousMatrixExtraction {
        return ContinuousMatrixExtraction(target: self, rows: rows, cols: cols)
    }
    public func extract(rows: [Matrix.IndexType], cols: [Matrix.IndexType]) -> MinorMatrixExtraction {
        return MinorMatrixExtraction(target: self, rows: rows, cols: cols)
    }
    
    // Computed Properties
    public var rows: Int {
        data.count
    }
    public var columns: Int {
        data.isEmpty ? 0 : data[0].count
    }
    public var dims: MatrixDimension {
        get {
            MatrixDimension(rows: self.rows, cols: self.columns);
        }
    }
    public var isSquare: Bool {
        rows == columns
    }
    public var isValid: Bool {
        rows != 0
    }
    public var asExtraction: ContinuousMatrixExtraction {
        get {
            ContinuousMatrixExtraction(target: self, rows: 0..<self.rows, cols: 0..<self.columns)
        }
    }
    public var determinant: Double? {
        get {
            self.asExtraction.determinant
        }
    }
    public var inverse: Matrix? {
        get {
            guard self.isSquare else { return nil }
            guard self.isValid else { return Matrix() }
            
            let identity = Matrix.Identity(dim: self.rows)
            let augmented = try! identity | self; //Augment only throws if the matrix is not square, but that was already handled.
            augmented.reducedRowEchelonForm();
            
            let cols_index = 0..<rows;
            let left_index = 0..<rows;
            let right_index = rows..<(2 * rows);
            
            let left = augmented.extract(rows: left_index, cols: right_index);
            let right = augmented.extract(rows: right_index, cols: cols_index);
            
            if right != identity {
                return nil;
            } else {
                return left.intoMatrix;
            }
        }
    }
    public var transpose: Matrix {
        get {
            let result = Matrix(rows: self.columns, cols: self.rows);
            
            for i in 0..<self.rows {
                for j in 0..<self.columns {
                    result.data[j][i] = self.data[i][j];
                }
            }
            
            return result;
        }
    }
    public func transposeInplace() {
        let rows = self.rows;
        let cols = self.columns;
        let old = self.data;
        
        self.allocate(rows: cols, cols: rows);
        
        for i in 0..<rows {
            for j in 0..<cols {
                self.data[j][i] = old[i][j];
            }
        }
    }
    
    // Echelon Operations
    public func rowSwap(orig: Matrix.IndexType, dest: Matrix.IndexType) throws(IndexOutOfRangeError<Matrix.IndexType>) {
        guard orig >= 0 && orig < rows else {
            throw IndexOutOfRangeError(index: orig)
        }
        guard  dest >= 0 && dest < rows else {
            throw IndexOutOfRangeError(index: dest)
        }
        
        if orig == dest { return } //Nothing to do
        
        data.swapAt(orig, dest);
    }
    public func rowAdd<T: ScalarLike>(orig: Matrix.IndexType, fac: T, dest: Matrix.IndexType) throws(IndexOutOfRangeError<Matrix.IndexType>) {
        guard orig >= 0 && orig < rows else {
            throw IndexOutOfRangeError(index: orig)
        }
        guard  dest >= 0 && dest < rows else {
            throw IndexOutOfRangeError(index: dest)
        }
        
        let fac = fac.asDouble;
        for j in 0..<self.columns {
            self.data[dest][j] += fac * self.data[orig][j];
        }
    }
    public func rowEchelonForm() {
        
    }
    public func reducedRowEchelonForm() {
        
    }
    
    
    
    public static func == (lhs: Matrix, rhs: Matrix) -> Bool {
        lhs.rows == rhs.rows && lhs.columns == rhs.columns && lhs.data == rhs.data
    }
    
    public static func |(lhs: Matrix, rhs: Matrix) throws(OperationError) -> Matrix {
        guard lhs.isValid && rhs.isValid else { throw OperationError(operation: "|", a: lhs.displayString, b: rhs.displayString, reason: "no matrix can be the empty matrix") }
        guard lhs.rows == rhs.rows else { throw OperationError(operation: "|", a: lhs.displayString, b: rhs.displayString, reason: "Dimension mismatch, rows must be equal") }
        
        let oneRows = lhs.rows, oneCols = lhs.columns;
        let twoCols = rhs.columns;
        
        let result = Matrix(rows: oneRows, cols: oneCols + twoCols);
        precondition(result.isValid, "invalid matrix result");
        
        for row in result.data.indices {
            var col: Int = 0;
            //First we take from the lhs matrix
            for sourcesCol in lhs.data[row] {
                result.data[row][col] = sourcesCol;
                col += 1;
            }
            
            //Now we take from the rhs matrix
            for sourcesCol in rhs.data[row] {
                result.data[row][col] = sourcesCol;
                col += 1;
            }
        }
        
        return result;
    }
    
    public static func +(lhs: Matrix, rhs: Matrix) throws(OperationError) -> Matrix {
        guard lhs.dims == rhs.dims else { throw OperationError(operation: "", a: lhs.displayString, b: rhs.displayString, reason: "dimensions for this operation require the same dimensions")}
        
        let result = Matrix(dims: lhs.dims)
        for i in 0..<result.rows {
            for j in 0..<result.columns {
                result.data[i][j] = lhs.data[i][j] + rhs.data[i][j];
            }
        }
        
        return result;
    }
    public static func -(lhs: Matrix, rhs: Matrix) throws(OperationError) -> Matrix {
        guard lhs.dims == rhs.dims else { throw OperationError(operation: "", a: lhs.displayString, b: rhs.displayString, reason: "dimensions for this operation require the same dimensions")}
        
        let result = Matrix(dims: lhs.dims)
        for i in 0..<result.rows {
            for j in 0..<result.columns {
                result.data[i][j] = lhs.data[i][j] - rhs.data[i][j];
            }
        }
        
        return result;
    }
    public static func *(lhs: Matrix, rhs: Matrix) throws(OperationError) -> Matrix {
        
    }
    
    public static prefix func -(lhs: Matrix) -> Matrix {
        let result = Matrix(inner: lhs.data);
        
        for i in 0..<result.rows {
            for j in 0..<result.columns {
                result.data[i][j] *= -1;
            }
        }
        
        return result;
    }
    
    public static func +=(lhs: inout Matrix, rhs: Matrix) throws(OperationError) {
        
    }
    public static func -=(lhs: inout Matrix, rhs: Matrix) throws(OperationError) {
        
    }
    public static func *=(lhs: inout Matrix, rhs: Matrix) throws(OperationError) {
        
    }
    
    public static func *<T: ScalarLike>(lhs: Matrix, rhs: T) -> Matrix {
        let rhs = rhs.asDouble
        let result = Matrix(inner: lhs.data)
        
        for i in 0..<result.rows {
            for j in 0..<result.columns {
                result.data[i][j] *= rhs;
            }
        }
        
        return result;
    }
    public static func *<T: ScalarLike>(lhs: T, rhs: Matrix) -> Matrix {
        return rhs * lhs;
    }
    public static func /<T: ScalarLike>(lhs: Matrix, rhs: T) -> Matrix {
        let rhs = rhs.asDouble
        let result = Matrix(inner: lhs.data)
        
        for i in 0..<result.rows {
            for j in 0..<result.columns {
                result.data[i][j] /= rhs;
            }
        }
        
        return result;
    }
    public static func /<T: ScalarLike>(lhs: T, rhs: Matrix) -> Matrix {
        return rhs / lhs
    }
    
    public static func *(lhs: Vector, rhs: Matrix) throws(OperationError) -> Vector {
        guard lhs.dim == rhs.columns else { throw OperationError(operation: "*", a: lhs.displayString, b: rhs.displayString, reason: "the dimension of the vector must match the dimension of the columns of the matrix") }
        
        //The result will be the same dimension of rhs rows, and is a weighted sum of the columns of rhs.
        let result = Vector(size: rhs.rows);
        for i in 0..<rhs.rows {
            for j in 0..<rhs.columns {
                result.data[i] += lhs.data[j] * rhs.data[i][j];
            }
        }
        
        return result;
    }
    public static func *(lhs: Matrix, rhs: Vector) throws(OperationError) -> Vector {
        try rhs * lhs;
    }
}
