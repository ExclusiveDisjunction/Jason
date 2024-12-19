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
        
        std::vector<Unit> a_bin = a.ToBinary();
        Scalar a_conv = Scalar::FromBinary(a_bin);
        
        std::cout << "Scalars Conversion: Expect true: " << (a == a_conv) << '\n';
        
        std::cout << "Operations:\n" <<
        (a + b == 1.1 + 3.2) << '\n' <<
        (a - b == 1.1 - 3.2) << '\n' <<
        (a * b == 1.1 * 3.2) << '\n' <<
        (a / b == 1.1 / 3.2) << '\n' << std::endl;
        
        Complex ca(1.1, 3.3), cb(2.4, 1.6);
        std::vector<Unit> ca_bin = ca.ToBinary();
        Complex ca_conv = Complex::FromBinary(ca_bin);
        
        std::cout << "Complex Conversion: Expect true: " << (ca == ca_conv) << '\n';
        
        std::cout << "Complex operations: \n" <<
        (ca + cb == Complex(1.1 + 2.4, 3.3 + 1.6)) << '\n' <<
        (ca - cb == Complex(1.1 - 2.4, 3.3 - 1.6)) << '\n' <<
        (ca * cb == Complex(-2.64, 9.68)) << ' ' << (ca * cb) << "==" << Complex(-2.64, 9.68) << '\n' <<
        (ca / cb == Complex(99/104, 77/104)) << ' ' << (ca / cb) << "==" << Complex(99/104, 77/104) << std::endl;
    }
    catch (const ErrorBase& e)
    {
        std::cerr << "Cought: " << e << std::endl;
        return false;
    }
    
    return true;
}
