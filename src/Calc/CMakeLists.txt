add_library(Calc SHARED
        Numerics.h
        VariableType.h
        VariableType.cpp
        Scalar.cpp
        Matrix.cpp
        MathVector.cpp
        Complex.cpp
        Complex.h
)

target_link_libraries(Calc Core)