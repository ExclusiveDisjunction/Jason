//
//  Scalar.swift
//  jason-cli
//
//  Created by Hollan on 1/17/25.
//

import Foundation

/// Used to simply details with Scalar and other types inputting.
public protocol ScalarLike {
    var asDouble: Double { get }
}

extension Double: ScalarLike {
    public var asDouble: Double { self }
}

public func pow(_ base: ScalarLike, _ power: ScalarLike) -> Scalar {
    Scalar(pow(base.asDouble, power.asDouble))
}

public final class Scalar : VariableData, ScalarLike, Comparable {
    public init() {
        self.a = 0;
    }
    public init<T : ScalarLike>(_ value: T) {
        self.a = value.asDouble
    }
    
    public var a: Double;
    public var asDouble: Double { a }
    
    public static var type: VariableType { .scalar }
    public var displayString: String { "(Scalar)" }
    
    public func clone() -> any VariableData { Scalar(self) }
    
    public static func ==(lhs: Scalar, rhs: Scalar) -> Bool {
        lhs.a == rhs.a
    }
    public static func<(lhs: Scalar, rhs: Scalar) -> Bool {
        lhs.a < rhs.a
    }
    
    public static func +<T: ScalarLike>(lhs: Scalar, rhs: T) -> Scalar {
        Scalar(lhs.a + rhs.asDouble)
    }
    public static func +<T: ScalarLike>(lhs: T, rhs: Scalar) -> Scalar {
        Scalar(rhs.a + lhs.asDouble)
    }
    public static func -<T: ScalarLike>(lhs: Scalar, rhs: T) -> Scalar {
        Scalar(lhs.a - rhs.asDouble)
    }
    public static func -<T: ScalarLike>(lhs: T, rhs: Scalar) -> Scalar {
        Scalar(rhs.a - lhs.asDouble)
    }
    public static func *<T: ScalarLike>(lhs: Scalar, rhs: T) -> Scalar {
        Scalar(lhs.a * rhs.asDouble)
    }
    public static func *<T: ScalarLike>(lhs: T, rhs: Scalar) -> Scalar {
        Scalar(rhs.a * lhs.asDouble)
    }
    public static func /<T: ScalarLike>(lhs: Scalar, rhs: T) -> Scalar {
        Scalar(lhs.a / rhs.asDouble)
    }
    public static func /<T: ScalarLike>(lhs: T, rhs: Scalar) -> Scalar {
        Scalar(rhs.a / lhs.asDouble)
    }
    
    public static func +=<T: ScalarLike>(lhs: inout Scalar, rhs: T) {
        lhs.a += rhs.asDouble
    }
    public static func -=<T: ScalarLike>(lhs: inout Scalar, rhs: T) {
        lhs.a -= rhs.asDouble
    }
    public static func *=<T: ScalarLike>(lhs: inout Scalar, rhs: T) {
        lhs.a *= rhs.asDouble
    }
    public static func /=<T: ScalarLike>(lhs: inout Scalar, rhs: T) {
        lhs.a /= rhs.asDouble
    }
}
