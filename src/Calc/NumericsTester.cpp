//
//  NumericsTester.cpp
//  jason-cli
//
//  Created by Hollan on 12/18/24.
//

#include "Scalar.h"
#include "MathVector.h"
#include "Matrix.h"
#include "Complex.h"

#include "../Core/Errors.h"

bool NumericsTester()
{
    
    try
    {
        Scalar a(1.1), b(3.2), c(0.00);
        
        std::stringstream ss;
        a.str_serialize(ss);
        Scalar a_conv;
        ss.seekg(0, std::ios::beg);
        a_conv.str_deserialize(ss);
        
        std::cout << "Scalars Conversion: Expect true: " << (a == a_conv) << '\n';
        
        std::cout << "Operations:\n" <<
        (a + b == 1.1 + 3.2) << '\n' <<
        (a - b == 1.1 - 3.2) << '\n' <<
        (a * b == 1.1 * 3.2) << '\n' <<
        (a / b == 1.1 / 3.2) << '\n' << std::endl;
        
        Complex ca(1.1, 3.3), cb(2.4, 1.6);
        
        ss = {};
        ca.str_serialize(ss);
        Complex ca_conv;
        ss.seekg(0, std::ios::beg);
        ca_conv.str_deserialize(ss);
        
        std::cout << "Complex Conversion: Expect true: " << (ca == ca_conv) << '\n';
        
        std::cout << "Complex operations: \n" <<
        (ca + cb == Complex(1.1 + 2.4, 3.3 + 1.6)) << '\n' <<
        (ca - cb == Complex(1.1 - 2.4, 3.3 - 1.6)) << '\n' <<
        (ca * cb == Complex(-2.64, 9.68)) << ' ' << (ca * cb) << "==" << Complex(-2.64, 9.68) << '\n' <<
        (ca / cb == Complex(99/104, 77/104)) << ' ' << (ca / cb) << "==" << Complex(99.0/104.0, 77.0/104.0) << std::endl;
        
        
        MathVector va = MathVector::FromList(1.1, 2.3), vb = MathVector::FromList(1, 2), vc = MathVector::FromList(3, 2, 1);
        
        ss = {};
        va.str_serialize(ss);
        MathVector va_conv;
        ss.seekg(0, std::ios::beg);
        va_conv.str_deserialize(ss);
        std::cout << "Vector Conversion: Expect true: " << (va == va_conv) << '\n';
        
        std::cout << "Vector Operations: \n" <<
        (va + vb == MathVector::FromList(1.1 + 1, 2.3 + 2)) << '\n' <<
        (va - vb == MathVector::FromList(1.1 - 1, 2.3 - 2)) << '\n' <<
        (va + vc == MathVector::FromList(1.1 + 3, 2.3 + 2, 1)) << '\n';
        
        double dot = MathVector::DotProduct(va, vb);
        std::cout << (dot == (1.1 * 1 + 2.3 * 2)) << " dot = " << dot << " (should be = " << (1.1 * 1 + 2.3 * 2) << ") \n";
        
        MathVector vd = MathVector::FromList(3, 4, 5);
        MathVector cross = MathVector::CrossProduct(vc, vd);
        std::cout << display_print(cross) << " == " << display_print(MathVector::FromList(6, -12, 6)) << " ? " << (cross == MathVector::FromList(6, -12, 6)) << std::endl;
    }
    catch (const ErrorBase& e)
    {
        std::cerr << "Cought: " << e << std::endl;
        return false;
    }
    
    return true;
}
