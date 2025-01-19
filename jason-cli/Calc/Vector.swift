//
//  Vector.swift
//  jason-cli
//
//  Created by Hollan on 1/17/25.
//

import Foundation

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
    
    var data: [Double];
    
    /// Returns a copy of the internal data for use otherwise. 
    public var exposeData: [Double] { data }
    
    public var dim: Int {
        self.data.count
    }
    public var isValid: Bool {
        !self.data.isEmpty
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
    public static func crossProduct(lhs: Vector, rhs: Vector) throws -> Vector {
        guard lhs.dim == rhs.dim else { throw DimensionError(dimA: lhs.dim, dimB: rhs.dim) }
        
        let a: (Double, Double, Double);
        let b: (Double, Double, Double);
        
        switch (lhs.dim) {
        case 2:
            a = (lhs.data[0], lhs.data[1], 0)
            b = (rhs.data[0], rhs.data[1], 0)
        case 3:
            a = (lhs.data[0], lhs.data[1], lhs.data[2])
            b = (rhs.data[0], rhs.data[1], rhs.data[2])
        default:
            throw OperationError(operation: "X", a: lhs.displayString, b: rhs.displayString, reason: "cross product is only offered for 2D and 3D vectors")
        }
        
        return Vector(
            elements:
                a.1 * b.2 - a.2 * b.1,
                a.2 * b.0 - a.0 * b.2,
                a.0 * b.1 - a.1 * b.0
        );
    }
    
    public static var type: VariableType { .vector }
    public var displayString: String { "(Vector:\(self.data.count))"}
    
    public func clone() -> any VariableData {
        Vector(list: self.data)
    }
    
    public static func ==(lhs: Vector, rhs: Vector) -> Bool {
        lhs.data == rhs.data
    }
    
    /*
        a, b -> Vector
            a + b = b + a
            a - b != b - a
            a * b DNE
            a / b DNE
     
        a -> Vector, b -> Scalar
            a + b DNE
            a - b DNE
            a * b = b * a
            a / b
            b / a DNE 
     */
    public static func +(lhs: Vector, rhs: Vector) -> Vector {
        var a: Vector; //Larger by dimension, and our result
        let b: Vector; //Smaller by dimension
        
        if lhs.dim < rhs.dim {
            a = rhs
            b = lhs
        } else {
            a = rhs
            b = lhs
        }
        
        for i in 0..<b.dim {
            a.data[i] += b.data[i]
        }
        
        return a
    }
    public static func -(lhs: Vector, rhs: Vector) -> Vector {
        var a: Vector; //Larger by dimension, and our result
        let b: Vector; //Smaller by dimension
        
        if lhs.dim < rhs.dim {
            a = rhs
            b = lhs
        } else {
            a = rhs
            b = lhs
        }
        
        for i in 0..<b.dim {
            a.data[i] += b.data[i]
        }
        
        return a
    }
    public static func *<T: ScalarLike>(lhs: Vector, rhs: T) -> Vector {
        let b = rhs.asDouble
        return Vector(list: lhs.data.map { $0 * b })
    }
    public static func *<T: ScalarLike>(lhs: T, rhs: Vector) -> Vector {
        return rhs * lhs
    }
    public static func /<T: ScalarLike>(lhs: Vector, rhs: T) -> Vector {
        let b = rhs.asDouble
        return Vector(list: lhs.data.map { $0 / b })
    }
    
    public static prefix func -(lhs: Vector) -> Vector {
        let arr = lhs.data.map { -$0 };
        return Vector(list: arr);
    }
    
    public static func +=(lhs: inout Vector, rhs: Vector) throws {
        guard lhs.dim >= rhs.dim else { throw OperationError(operation: "+=", a: lhs.displayString, b: rhs.displayString, reason: "cannot add in place if lhs dim is less than rhs dim") }
        
        for i in rhs.data.indices {
            lhs.data[i] += rhs.data[i]
        }
    }
    public static func -=(lhs: inout Vector, rhs: Vector) throws {
        guard lhs.dim >= rhs.dim else { throw OperationError(operation: "-=", a: lhs.displayString, b: rhs.displayString, reason: "cannot subtract in place if lhs dim is less than rhs dim") }
        
        for i in rhs.data.indices {
            lhs.data[i] -= rhs.data[i]
        }
    }
    public static func *=<T: ScalarLike>(lhs: inout Vector, rhs: T) {
        let b = rhs.asDouble
        for i in lhs.data.indices {
            lhs.data[i] *= b
        }
    }
    public static func /=<T: ScalarLike>(lhs: inout Vector, rhs: T) {
        let b = rhs.asDouble
        for i in lhs.data.indices {
            lhs.data[i] /= b
        }
    }
}
