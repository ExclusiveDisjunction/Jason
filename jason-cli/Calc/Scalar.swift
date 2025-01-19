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
extension Float: ScalarLike {
    public var asDouble: Double { Double(self) }
}
extension Int: ScalarLike {
    public var asDouble: Double { Double(self) }
}

extension ScalarLike {
    /// Either truncates the decimal to make an integer, or attempts to see if the number is `roundingThreshold` away from being an integer. If `round` == true, then this will always return a value.
    public func toInt(round: Bool = true, roundingThreshold: Double = 0.05) -> Int? {
        let val = self.asDouble;
        if round {
            return Int(val);
        } else {
            let variance = val - Double(Int(val));
            if abs(variance) >= roundingThreshold {
                return nil
            }
            else {
                return Int(val);
            }
        }
    }
}

public func pow<T: ScalarLike>(_ base: T, _ power: T) -> Scalar {
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
    
    public static prefix func -(lhs: Scalar) -> Scalar {
        return Scalar(-lhs.a)
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
