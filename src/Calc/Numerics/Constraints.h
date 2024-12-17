//
// Created by exdisj on 6/21/24.
//

#ifndef JASON_CONSTRAINTS_H
#define JASON_CONSTRAINTS_H

#include "../StdCalc.h"

class MATH_LIB Scalar;

template<typename T, typename Base, typename NumType>
concept IsBaseOrNumeric = requires
{
    std::is_base_of<T, Base>::value || std::is_same<T, NumType>::value || std::is_nothrow_convertible<T, NumType>::value;
};

template<typename T>
concept IsScalarOrDouble = IsBaseOrNumeric<T, Scalar, double>;

#endif //JASON_CONSTRAINTS_H
