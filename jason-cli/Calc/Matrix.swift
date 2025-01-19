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

public struct MatrixExtraction<T: Collection> where T.Element == Matrix.IndexType {
    internal init(target: Matrix, rows: T, cols: T) {
        self.target = target
        self.rows = rows
        self.cols = cols
    }
    
    private let target: Matrix;
    public let rows: T;
    public let cols: T;
    
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
}

public typealias ContinuousMatrixExtraction = MatrixExtraction<Range<Int>>;
public typealias MinorMatrixExtraction = MatrixExtraction<[Int]>;

public final class Matrix : VariableData {
    public init() {
        self.data = [];
    }
    public init(rows: Int, cols: Int, value: Double = 0.0) {
        self.data = [];
        self.allocate(rows: rows, cols: cols, value: value);
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
    
    public typealias IndexType = [[Double]].Index;
    
    public static func Identity(dim: Int) -> Matrix {
        let result = Matrix(rows: dim, cols: dim);
        for i in 0..<dim {
            result.data[i][i] = 1;
        }
        
        return result;
    }
    
    private func allocate(rows: Int, cols: Int, value: Double = 0) {
        
    }
    
    private func getColSchematic() -> [(Bool, Int)] {
        return [];
    }
    private func getRowString(schema: [(Bool, Int)], row: Int, open: Character, close: Character) -> String? {
        return nil;
    }
    
    public static var type: VariableType { .matrix }
    public var displayString: String { "(Matrix:\(rows)x\(columns))" }
    
    public func clone() -> any VariableData {
        Matrix(inner: self.data)
    }
    
    private var data: [[Double]]
    
    public subscript(_ row: Int) -> MatrixRow {
        get {
            precondition(row >= 0 && row <= self.rows, "invalid row index")
            
            return MatrixRow(target: self, offset: row)
        }
    }
    public subscript(_ row: Int, _ col: Int) -> Double {
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
    
    public func extract(rows: Range<Int>, cols: Range<Int>) -> ContinuousMatrixExtraction {
        return ContinuousMatrixExtraction(target: self, rows: rows, cols: cols)
    }
    public func extract(rows: [Int], cols: [Int]) -> MinorMatrixExtraction {
        return MinorMatrixExtraction(target: self, rows: rows, cols: cols)
    }
    
    public func determinant() -> Double? {
        
    }
    public func invert() -> Matrix? {
        
    }
    public func transpose() -> Matrix {
        
    }
    public func transposeInplace() {
        
    }
    
    public func rowEchelonForm() -> Matrix {
        
    }
    public func reducedRowEchelonForm() -> Matrix {
        
    }
    
    public var rows: Int {
        data.count
    }
    public var columns: Int {
        data.isEmpty ? 0 : data[0].count
    }
    public var isSquare: Bool {
        rows == columns
    }
    public var isValid: Bool {
        rows != 0
    }
    
    public static func == (lhs: Matrix, rhs: Matrix) -> Bool {
        lhs.rows == rhs.rows && lhs.columns == rhs.columns && lhs.data == rhs.data
    }
    
    public static func |(lhs: Matrix, rhs: Matrix) throws(OperationError) -> Matrix {
        
    }
    
    public static func +(lhs: Matrix, rhs: Matrix) throws(OperationError) -> Matrix {
        
    }
    public static func -(lhs: Matrix, rhs: Matrix) throws(OperationError) -> Matrix {
        
    }
    public static func *(lhs: Matrix, rhs: Matrix) throws(OperationError) -> Matrix {
        
    }
    
    public static prefix func -(lhs: Matrix) -> Matrix {
        
    }
    
    public static func +=(lhs: inout Matrix, rhs: Matrix) throws(OperationError) {
        
    }
    public static func -=(lhs: inout Matrix, rhs: Matrix) throws(OperationError) {
        
    }
    public static func *=(lhs: inout Matrix, rhs: Matrix) throws(OperationError) {
        
    }
    
    public static func *<T: ScalarLike>(lhs: Matrix, rhs: T) -> Matrix {
        
    }
    public static func *<T: ScalarLike>(lhs: T, rhs: Matrix) -> Matrix {
        return rhs * lhs;
    }
    public static func /<T: ScalarLike>(lhs: Matrix, rhs: T) -> Matrix {
        
    }
    public static func /<T: ScalarLike>(lhs: T, rhs: Matrix) -> Matrix {
        return rhs / lhs
    }
    
    public static func *(lhs: Vector, rhs: Matrix) throws(OperationError) -> Matrix {
        
    }
    public static func *(lhs: Matrix, rhs: Vector) throws(OperationError) -> Matrix {
        try rhs * lhs;
    }
}
