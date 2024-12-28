#include "BinaryUnit.h"

/*
template<typename T>
void BinaryUnit::Allocate(const T& obj)
{
    if (Data)
        Deallocate();

    this->blockSize = sizeof(T);
    this->Data = new char[blockSize];
    memcpy(this->Data, &obj, blockSize);
}

template<typename T>
T BinaryUnit::Convert() const
{
    T result;
    this->ConvertTo<T>(result);

    return result;
}
template<typename T>
void BinaryUnit::ConvertTo(T& result) const
{
    if (sizeof(T) != blockSize)
        throw std::logic_error("The type to convert must be the same size as the allocated block size");

    memcpy(&result, this->Data, blockSize);
}
*/
