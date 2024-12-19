//
//  Serialize.h
//  jason-cli
//
//  Created by Hollan on 12/18/24.
//

#ifndef JASON_SERIALIZE_H
#define JASON_SERIALIZE_H

#include "BinaryUnit.h"

class StringSerialize
{
public:
    virtual void str_serialize(std::ostream& out) const noexcept = 0;
};
class StringDeserialize
{
public:
    virtual void str_deserialize(std::istream& in) = 0;
};

class BinarySerialize
{
public:
    virtual std::vector<Unit> binary_serialize(unsigned char bytes_size) const noexcept = 0;
};
class BinaryDeserialize
{
public:
    virtual void binary_deserialize(unsigned char bytes_size, const std::vector<Unit>& data) = 0;
};

#endif
