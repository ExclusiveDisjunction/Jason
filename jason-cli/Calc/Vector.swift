//
//  Vector.swift
//  jason-cli
//
//  Created by Hollan on 1/17/25.
//

import Foundation

public protocol DimensionKind: Equatable {
    var stringDescription: String { get }
}

extension Int: DimensionKind {
    public var stringDescription: String {
        String(self)
    }
}

public final class DimensionError<T: DimensionKind> : Error {
    init(dimA: T, dimB: T) {
        self.a = dimA
        self.b = dimB
    }
    
    var a: T
    var b: T
    
    var localizedDescription: String {
        "Dimension mismatch between \(a.stringDescription) and \(b.stringDescription)"
    }
}

public final class OperationError : Error {
    init(operation: String, on: String, reason: String) {
        self.operation = operation
        self.on = on
        self.reason = reason
    }
    
    public var operation: String;
    public var on: String;
    public var reason: String;
    
    public var localizedDescription: String {
        "Cannot apply \(operation) on \(on) because '\(reason)'"
    }
}

public final class Vector : VariableData {
    
    public init() {
        self.data = []
    }
    public init<T: ScalarLike>(elements: T...) {
        self.data = elements.map(\.asDouble)
    }
    public init(size: Int) {
        data = [Double](repeating: 0, count: size)
    }
    public init<T: ScalarLike>(list: [T]) {
        self.data = list.map(\.asDouble)
    }
    
    private var data: [Double];
    
    public var dim: Int {
        self.data.count
    }
    public var isValid: Bool {
        !self.data.isEmpty
    }
    
    public var magnitude: Double {
        //We can mark this with try! because dotProduct only fails if the dimensions are off
        sqrt(try! Vector.dotProduct(lhs: self, rhs: self).asDouble)
    }
    public var angle: Double? {
        get {
            guard dim == 2 else { return nil }
            
            return atan2(data[1], data[2])
        }
    }
    
    public static func dotProduct(lhs: Vector, rhs: Vector) throws(DimensionError<Int>) -> Scalar {
        guard lhs.dim != rhs.dim else { throw DimensionError(dimA: lhs.dim, dimB: rhs.dim) }
        
        var result: Double = 0.0
        for i in 0..<lhs.dim {
            result += lhs.data[i] * rhs.data[i]
        }
        
        return Scalar(result)
    }
    public static func crossProduct(lhs: Vector, rhs: Vector) throws-> Vector {
        
    }
    
    public subscript(index: Int) -> Double {
        get {
            precondition(index >= 0 && index < self.dim, "Index out of boudns");
            
            return self.data[index]
        }
        set(v) {
            precondition(index >= 0 && index < self.dim, "Index out of boudns");
            
            self.data[index] = v
        }
    }
    
    public static var type: VariableType { .vector }
    public var displayString: String { "(Vector:\(self.data.count))"}
    
    public func clone() -> any VariableData {
        Vector(list: self.data)
    }
    
    public static func ==(lhs: Vector, rhs: Vector) -> Bool {
        lhs.data == rhs.data
    }
}
