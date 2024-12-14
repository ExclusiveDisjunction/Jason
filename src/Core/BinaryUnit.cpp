//
// Created by exdisj on 10/21/24.
//

#include "BinaryUnit.h"

Unit::Unit() : Data(nullptr), blockSize(0) {}
Unit::Unit(const Unit& obj) noexcept
{
    Allocate(obj.Data, obj.blockSize, true);
}
Unit::Unit(Unit&& obj) noexcept : Data(std::exchange(obj.Data, nullptr)), blockSize(std::exchange(obj.blockSize, 0)) 
{
    Allocate(obj.Data, obj.blockSize, false);
    obj.Data = nullptr;
    obj.blockSize = 0;
}
Unit::~Unit()
{
    Deallocate();
}

Unit Unit::FromSize(unsigned char size) noexcept
{
    Unit result;
    if (size != 0)
    {
        result.Data = new char[size];
        memset(result.Data, 0, size);
    }

    return result;
}
Unit Unit::FromCharPtr(char* data, unsigned char size, bool copy) noexcept
{
    Unit result;
    result.Allocate(data, size, copy);

    return result;
}

void Unit::Deallocate()
{
    delete[] Data;
    Data = nullptr;
    blockSize = 0;
}
void Unit::Allocate(char* data, unsigned char size, bool copy)
{
    if (!size || !data)
        return;

    this->blockSize = size;
    if (copy)
    {
        this->Data = new char[size];
        memcpy(this->Data, data, size);
    }
    else
        this->Data = data;
}

Unit& Unit::operator=(const Unit& obj) noexcept
{
    if (this == &obj)
        return *this;

    if (Data)
        Deallocate();

    Allocate(obj.Data, obj.blockSize, true);
    return *this;
}
Unit& Unit::operator=(Unit&& obj) noexcept
{
    if (Data)
        Deallocate();

    Allocate(obj.Data, obj.blockSize, false);
    obj.Data = nullptr;
    obj.blockSize = 0;
    return *this;
}

const char* Unit::Expose() const noexcept
{
    return this->Data;
}
unsigned char Unit::GetSize() const noexcept
{
    return this->blockSize;
}