//
//  DimensionKind.swift
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

public final class DimensionError<T> : Error where T: Sendable, T: DimensionKind {
    init(dimA: T, dimB: T) {
        self.a = dimA
        self.b = dimB
    }
    
    let a: T
    let b: T
    
    var localizedDescription: String {
        "Dimension mismatch between \(a.stringDescription) and \(b.stringDescription)"
    }
}

public final class OperationError : Error {
    init(operation: String, a: String, b: String, reason: String) {
        self.operation = operation
        self.on = (a, b)
        self.reason = reason
    }
    
    public let operation: String;
    public let on: (String, String);
    public let reason: String;
    
    public var localizedDescription: String {
        "Cannot apply \(operation) on \(on.0) & \(on.1) because of '\(reason)'"
    }
}
