//
//  Serialize.cpp
//  jason-cli
//
//  Created by Hollan on 12/20/24.
//

#include "Serialize.h"

std::ostream& operator<<(std::ostream& out, const sst<const StringSerializable>& obj) noexcept
{
    obj.obj.str_serialize(out);
    
    return out;
}
std::istream& operator>>(std::istream& in, sst<StringSerializable>& obj)
{
    obj.obj.str_deserialize(in);
    
    return in;
}

sst<const StringSerializable> StrSerialize(const StringSerializable& obj) noexcept
{
    return sst<const StringSerializable>(obj);
}
sst<StringSerializable> StrDeserialize(StringSerializable& obj) noexcept
{
    return sst<StringSerializable>(obj);
}

/*
std::vector<BinaryUnit> BinarySerializable::distribute_sizes(unsigned char byte_sizes, const std::vector<BinaryUnit>& data) const noexcept
{
    throw std::logic_error("not implemented");
}
std::vector<BinaryUnit> BinarySerializable::binary_align(unsigned char byte_sizes, const std::vector<BinaryUnit>& data) const noexcept
{
    throw std::logic_error("not implemented");
}
std::vector<BinaryUnit> BinarySerializable::binary_align(const std::vector<unsigned char>& byte_sizes, const std::vector<BinaryUnit>& data) const noexcept
{
    throw std::logic_error("not implemented");
}
*/
