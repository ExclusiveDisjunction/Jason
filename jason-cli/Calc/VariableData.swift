//
//  VariableData.swift
//  jason-cli
//
//  Created by Hollan on 1/17/25.
//

import Foundation

public enum VariableType : String, Codable, Equatable {
    case scalar = "SCA"
    case complex = "CMP"
    case vector = "VEC"
    case matrix = "MAT"
    
    public var displayString: String {
        switch self {
        case .scalar:
            return "Scalar"
        case .complex:
            return "Complex"
        case .vector:
            return "Vector"
        case .matrix:
            return "Matrix"
        }
    }
}

public protocol VariableData : Equatable, Codable {
    init()
    
    static var type: VariableType { get }
    var displayString: String { get }
    
    func clone() -> any VariableData;
}
