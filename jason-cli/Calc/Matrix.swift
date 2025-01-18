//
//  Matrix.swift
//  jason-cli
//
//  Created by Hollan on 1/17/25.
//

import Foundation

public final class MatrixRow {
    internal init(target: Matrix, offset: Int) {
        self.target = target
        self._offset = offset
    }
    
    private var target: Matrix
    private var _offset: Int
    
    public var isValidRow: Bool {
        _offset >= 0 && _offset < target.rows;
    }
    public var offset: Int {
        self._offset
    }
    
    public subscript(_ index: Int) -> Double {
        get {
            return target.getAtIndex(_offset, index)
        }
        set(v) {
            target.setAtIndex(_offset, index, val: v)
        }
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
    
    public static func Identity(dim: Int) -> Matrix {
        var result = Matrix(rows: dim, cols: dim);
        for i in 0..<dim {
            result.data[i][i] = 1;
        }
        
        return result;
    }
    
    private func allocate(rows: Int, cols: Int, value: Double = 0) {
        
    }
    
    private func getColSchematic() -> [(Bool, Int)] {
        
    }
    private func getRowString(schema: [(Bool, Int)], row: Int, open: Character, close: Character) -> String? {
        
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
    public func getAtIndex(_ row: Int, _ col: Int) -> Double {
        precondition(row >= 0 && row < self.rows, "invalid row index")
        precondition(col >= 0 && col < self.columns, "invalid column index")
        
        return data[row][col];
    }
    public func setAtIndex(_ row: Int, _ col: Int, val: Double) {
        precondition(row >= 0 && row < self.rows, "invalid row index")
        precondition(col >= 0 && col < self.columns, "invalid column index")
        
        data[row][col] = val;
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
}
