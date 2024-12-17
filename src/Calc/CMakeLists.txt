add_library(Calc SHARED StdCalc.cpp
    Numerics.h
    StdCalc.h
    VariableType.h
    VariableType.cpp
    Numerics/Scalar.cpp
    Numerics/Matrix.cpp
    Numerics/MathVector.cpp
        Numerics/Complex.cpp
        Numerics/Complex.h
)

target_link_libraries(Calc Core)