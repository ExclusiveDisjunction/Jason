//
//  Serialize.h
//  jason-cli
//
//  Created by Hollan on 12/18/24.
//

#ifndef JASON_SERIALIZE_H
#define JASON_SERIALIZE_H

#include "BinaryUnit.h"

class StringSerializable
{
public:
    virtual void str_serialize(std::ostream& out) const noexcept = 0;
    virtual void str_deserialize(std::istream& in) = 0;
};

//string seralalize temporary
template<typename T>
struct sst
{
private:
    T& obj;
    
public:
    sst(T& obj) : obj(obj) {}
    
    friend std::ostream& operator<<(std::ostream&, const sst<const StringSerializable>&) noexcept;
    friend std::istream& operator>>(std::istream&, sst<StringSerializable>& obj);
};

sst<const StringSerializable> StrSerialize(const StringSerializable& obj) noexcept;
sst<StringSerializable> StrDeserialize(StringSerializable& obj) noexcept;

/*
class BinarySerializable
{
public:
    std::vector<BinaryUnit> distribute_sizes(unsigned char byte_sizes, const std::vector<BinaryUnit>& data) const noexcept;
    std::vector<BinaryUnit> binary_align(unsigned char byte_sizes, const std::vector<BinaryUnit>& data) const noexcept;
    std::vector<BinaryUnit> binary_align(const std::vector<unsigned char>& byte_sizes, const std::vector<BinaryUnit>& data) const noexcept;
    
    virtual std::vector<BinaryUnit> binary_serialize(unsigned char bytes_size) const noexcept = 0;
    virtual void binary_deserialize(unsigned char bytes_size, const std::vector<BinaryUnit>& data) = 0;
};
 */

#endif
