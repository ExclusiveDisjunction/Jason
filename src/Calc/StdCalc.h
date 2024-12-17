#pragma once

#include <cmath>

#ifdef _WINDOWS_

    //ON windows, we must have the dllexport/imports to use proper DLLs.
    #ifdef MATH_EXPORTS
        #define MATH_LIB _declspec(dllexport)
    #else //MATH_EXPORTS
        #define MATH_LIB _declspec(dllexport)
    #endif //MATH_EXPORTS

#else //_WINDOWS

    //Since we are not on windows, we do not have to have anything special for the macros.
    #define MATH_LIB

#endif //_WINDOWS_

MATH_LIB double MapLerp(double R1h, double R1l, double R2h, double R2l, double R1p); //R1h is range 1 high, R1l is rane 1 low, same for R2, and R1l is the position in range 1.
MATH_LIB int GreatestCommonDenominator(int A, int B);