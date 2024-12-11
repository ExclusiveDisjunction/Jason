//
// Created by exdisj on 10/21/24.
//

#include "BinaryUnit.h"

Unit::Unit() : Data(nullptr), blockSize(0) {}
Unit::Unit(unsigned char Size) : blockSize(Size), Data(nullptr)
{
    if (Size != 0)
    {
        this->Data = new char[blockSize];
        memset(this->Data, 0, sizeof(char) * blockSize);
    }
}
Unit::Unit(char* Data, unsigned char Size, bool Copy) : Data(nullptr), blockSize(Size)
{
    if (Copy)
    {
        this->Data = new char[blockSize];
        memcpy(this->Data, Data, sizeof(char) * blockSize);
    }
    else 
        this->Data = Data;
}
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
[[nodiscard]] unsigned char Unit::GetSize() const noexcept
{
    return this->blockSize;
}