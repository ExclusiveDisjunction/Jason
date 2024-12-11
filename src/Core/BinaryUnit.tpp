#include "BinaryUnit.h"

template<typename T>
void Unit::Allocate(const T& obj)
{
    if (Data)
        Deallocate();

    this->blockSize = sizeof(T);
    this->Data = new char[blockSize];
    memcpy(this->Data, &obj, blockSize);
}
template<typename T>
void Unit::AllocateList(const std::vector<T>& obj)
{
    if (Data)
        Deallocate();

    this->blockSize = sizeof(T) * obj.size();
    this->Data = new char[blockSize];

    for (unsigned i = 0, j=0; i < blockSize; i+= sizeof(T), j++)
        memcpy(this->Data + i, &obj[j], sizeof(T));
}

template<typename T> requires is_different_v<T, Unit>
Unit& Unit::operator=(const T& obj) noexcept
{
    this->Allocate(obj);
    return *this;
}
template<typename T> requires is_different_v<T, Unit>
Unit& Unit::operator=(const std::vector<T>& items)
{
    this->Allocate(items);
    return *this;
}

template<typename T>
T Unit::Convert() const
{
    T result;
    Convert(result);

    return result;
}
template<typename T>
void Unit::Convert(T& result) const
{
    if (sizeof(T) != blockSize)
        throw std::logic_error("The type to convert must be the same size as the allocated block size");

    memcpy(&result, this->Data, blockSize);
}
template<typename T>
std::vector<T> Unit::ConvertMany() const
{
    unsigned count = blockSize / sizeof(T);
    if (count == 0)
        throw std::exception();

    std::vector<T> result;
    result.resize(count);
    for (unsigned i = 0, j = 0; i < blockSize; i+= sizeof(T), j++)
        memcpy(&result[j], this->Data + i, sizeof(T));

    return result;
}