import Foundation

enum VariableKind : Codable {
    case scalar
    case vector
    case matrix
    case complex
    
    var displayName: String {
        switch self {
        case .scalar:
            return "Scalar"
        case .vector:
            return "Vector"
        case .matrix:
            return "Matrix"
        case .complex:
            return "Complex"
        }
    }
}

protocol VariableData : Codable, Equatable {
    init();
    
    static var kind : VariableKind { get }
    var typeDisplayString: String { get }
    
    func clone() -> any VariableData;
}

final class Scalar : VariableData {
    init() {
        self.data = 0;
    }
    init(data: Double) {
        self.data = data
    }
    
    var data: Double;
    
    static var kind: VariableKind {
        .scalar
    }
    var typeDisplayString: String {
        "(Scalar)"
    }
    
    func clone() -> any VariableData {
        return Scalar(data: self.data)
    }
    
    static func == (lhs: Scalar, rhs: Scalar) -> Bool {
        return lhs.data == rhs.data;
    }
}

class VariableEntry {
    init() {
        
    }
    
    var data: (any VariableData)?;
    
}
