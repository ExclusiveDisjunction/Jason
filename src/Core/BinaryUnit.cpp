//
// Created by exdisj on 10/21/24.
//

#include "BinaryUnit.h"

/*
 BinaryUnit::BinaryUnit() : Data(nullptr), blockSize(0) {}
 BinaryUnit::BinaryUnit(const BinaryUnit& obj) noexcept
 {
 Allocate(obj.Data, obj.blockSize, true);
 }
 BinaryUnit::BinaryUnit(BinaryUnit&& obj) noexcept : Data(std::exchange(obj.Data, nullptr)), blockSize(std::exchange(obj.blockSize, 0))
 {
 Allocate(obj.Data, obj.blockSize, false);
 obj.Data = nullptr;
 obj.blockSize = 0;
 }
 BinaryUnit::~BinaryUnit()
 {
 Deallocate();
 }
 
 BinaryUnit BinaryUnit::FromSize(unsigned char size) noexcept
 {
 BinaryUnit result;
 if (size != 0)
 {
 result.Data = new char[size];
 memset(result.Data, 0, size);
 }
 
 return result;
 }
 BinaryUnit BinaryUnit::FromCharPtr(char* data, unsigned char size, bool copy) noexcept
 {
 BinaryUnit result;
 result.Allocate(data, size, copy);
 
 return result;
 }
 
 void BinaryUnit::Deallocate()
 {
 delete[] Data;
 Data = nullptr;
 blockSize = 0;
 }
 void BinaryUnit::Allocate(char* data, unsigned char size, bool copy)
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
 
 BinaryUnit& BinaryUnit::operator=(const BinaryUnit& obj) noexcept
 {
 if (this == &obj)
 return *this;
 
 if (Data)
 Deallocate();
 
 Allocate(obj.Data, obj.blockSize, true);
 return *this;
 }
 BinaryUnit& BinaryUnit::operator=(BinaryUnit&& obj) noexcept
 {
 if (Data)
 Deallocate();
 
 Allocate(obj.Data, obj.blockSize, false);
 obj.Data = nullptr;
 obj.blockSize = 0;
 return *this;
 }
 
 const char* BinaryUnit::Expose() const noexcept
 {
 return this->Data;
 }
 unsigned char BinaryUnit::GetSize() const noexcept
 {
 return this->blockSize;
 }
 */
