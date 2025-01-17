import Foundation

struct Version : Equatable, Hashable, Comparable, Codable {
    let major: Int
    let minor: Int
    let patch: Int
    
    init(major: Int, minor: Int, patch: Int) {
        self.major = major
        self.minor = minor
        self.patch = patch
    }
    
    var asString: String {
        "\(major).\(minor).\(patch)"
    }
    
    static func ord(lhs: Version, rhs: Version) -> ComparisonResult {
        if lhs == rhs {
            return .orderedSame
        }
        
        if lhs.major == rhs.major {
            if lhs.minor == rhs.minor {
                if lhs.patch == rhs.patch {
                    return .orderedSame
                }
                else if lhs.patch < rhs.patch {
                    return .orderedDescending
                }
                else {
                    return .orderedAscending
                }
            }
            else if lhs.minor < rhs.minor {
                return .orderedDescending
            }
            else {
                return .orderedAscending
            }
        }
        else if lhs.major < rhs.major {
            return .orderedDescending
        }
        else {
            return .orderedAscending
        }
    }
    
    static func ==(lhs: Version, rhs: Version) -> Bool {
        lhs.major == rhs.major && lhs.minor == rhs.minor && lhs.patch == rhs.patch
    }
    static func < (lhs: Version, rhs: Version) -> Bool {
        return Version.ord(lhs: lhs, rhs: rhs) == .orderedDescending
    }
}
