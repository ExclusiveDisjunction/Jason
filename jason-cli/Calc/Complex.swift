//
//  Complex.swift
//  jason-cli
//
//  Created by Hollan on 1/17/25.
//

import Foundation

public final class Complex: VariableData {
    public init() {
        self.a = 0
        self.b = 0
    }
    public init<T: ScalarLike>(_ a: T, _ b: T) {
        self.a = a.asDouble
        self.b = b.asDouble
    }
    
    public var a: Double;
    public var b: Double;
    
    /// Equivalent to `Z^*`
    public var conjugate: Complex {
        return Complex(a, -b)
    }
    /// Equivalent to `Z * (Z^*)`
    public var mulConjugate: Scalar {
        // (a-bi) (a+bi) = a^2 - abi + abi -bi^2 = a^2 + b^2
        return Scalar((a * a) + (b * b))
    }
    
    public static var type: VariableType { .complex }
    public var displayString: String { "(Complex)" }
    
    public func clone() -> any VariableData {
        Complex(a, b)
    }
    
    public static func ==(lhs: Complex, rhs: Complex) -> Bool {
        return lhs.a == rhs.a && lhs.b == rhs.b
    }
    
    public static func +(lhs: Complex, rhs: Complex) -> Complex {
        Complex(lhs.a + rhs.a, lhs.b + rhs.b)
    }
    public static func -(lhs: Complex, rhs: Complex) -> Complex {
        Complex(lhs.a - rhs.a, lhs.b - rhs.b)
    }
    public static func *(lhs: Complex, rhs: Complex) -> Complex {
        // (a + bi)(c + di)
        // ( ac + adi + bci + bdi^2 )
        // ( (ac - bd) + (ad + bc)i
        
        Complex(lhs.a * rhs.a - lhs.b * rhs.b, lhs.a * rhs.b + lhs.b * rhs.a)
    }
    public static func /(lhs: Complex, rhs: Complex) -> Complex {
        /*
          (a + bi) / (c + di)
          (a + bi) * (c - di) / (c + di)(c - di)
          lhs * conj / mul_conj
         */
        
        return lhs * rhs.conjugate / rhs.mulConjugate
    }
    
    public static func +=(lhs: inout Complex, rhs: Complex){
        lhs.a += rhs.a
        lhs.b += rhs.b
    }
    public static func -=(lhs: inout Complex, rhs: Complex) {
        lhs.a -= rhs.a
        lhs.b -= rhs.b
    }
    public static func *=(lhs: inout Complex, rhs: Complex){
        // (a + bi)(c + di)
        // ( ac + adi + bci + bdi^2 )
        // ( (ac - bd) + (ad + bc)i
        
        let a = lhs.a
        let b = lhs.b
        
        lhs.a = a * rhs.a - b * rhs.b
        lhs.b = a * rhs.b + b * rhs.a
    }
    public static func /=(lhs: inout Complex, rhs: Complex) {
        /*
          (a + bi) / (c + di)
          (a + bi) * (c - di) / (c + di)(c - di)
          lhs * conj / mul_conj
         */
        
        let result = lhs / rhs;
        lhs.a = result.a
        lhs.b = result.b
    }
    
    public static func +<T: ScalarLike>(lhs: Complex, rhs: T) -> Complex {
        Complex(lhs.a + rhs.asDouble, lhs.b)
    }
    public static func +<T: ScalarLike>(lhs: T, rhs: Complex) -> Complex {
        Complex(rhs.a + lhs.asDouble, rhs.b)
    }
    public static func -<T: ScalarLike>(lhs: Complex, rhs: T) -> Complex {
        Complex(lhs.a + rhs.asDouble, lhs.b)
    }
    public static func -<T: ScalarLike>(lhs: T, rhs: Complex) -> Complex {
        Complex(rhs.a - lhs.asDouble, rhs.b)
    }
    public static func *<T: ScalarLike>(lhs: Complex, rhs: T) -> Complex {
        Complex(lhs.a + rhs.asDouble, lhs.b * rhs.asDouble)
    }
    public static func *<T: ScalarLike>(lhs: T, rhs: Complex) -> Complex {
        Complex(rhs.a * lhs.asDouble, rhs.b * lhs.asDouble)
    }
    public static func /<T: ScalarLike>(lhs: Complex, rhs: T) -> Complex {
        Complex(lhs.a + rhs.asDouble, lhs.b / rhs.asDouble)
    }
    public static func /<T: ScalarLike>(lhs: T, rhs: Complex) -> Complex {
        Complex(rhs.a / lhs.asDouble, rhs.b / lhs.asDouble)
    }
    
    public static func +=<T: ScalarLike>(lhs: inout Complex, rhs: T) {
        lhs.a += rhs.asDouble
    }
    public static func -=<T: ScalarLike>(lhs: inout Complex, rhs: T) {
        lhs.a -= rhs.asDouble
    }
    public static func *=<T: ScalarLike>(lhs: inout Complex, rhs: T) {
        lhs.a *= rhs.asDouble
        lhs.b *= rhs.asDouble
    }
    public static func /=<T: ScalarLike>(lhs: inout Complex, rhs: T) {
        lhs.a /= rhs.asDouble
        lhs.b /= rhs.asDouble
    }
}

