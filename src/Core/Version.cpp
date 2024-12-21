#include "Version.h"

#include "Errors.h"

void Version::dbg_fmt(std::ostream& out) const noexcept 
{
    out << this->Major << '.' << this->Minor << '.' << this->Release;
}

void Version::str_serialize(std::ostream &out) const noexcept
{
    out << this->Major << ' ' << this->Minor << ' ' << this->Release;
}
void Version::str_deserialize(std::istream &in)
{
    in >> this->Major >> this->Minor >> this->Release;
}
std::vector<BinaryUnit> Version::binary_serialize(unsigned char bytes_size) const noexcept
{
    return BinarySerializable::distribute_sizes(
            bytes_size,
            std::vector<BinaryUnit>(
                {
                    BinaryUnit::FromVar(this->Major),
                    BinaryUnit::FromVar(this->Minor),
                    BinaryUnit::FromVar(this->Release)
                }
            )
    );
}
void Version::binary_deserialize(unsigned char bytes_size, const std::vector<BinaryUnit> &data)
{
    auto transformed = BinarySerializable::binary_align(sizeof(unsigned), data);
    if (transformed.size() < 3)
        throw ConversionError("binary deserialize", "the input data does not contain at least three elements");
    
    transformed[0].ConvertTo(this->Major);
    transformed[1].ConvertTo(this->Minor);
    transformed[2].ConvertTo(this->Release);
}

std::strong_ordering Version::operator<=>(const Version& obj) const noexcept
{
    if (this->Major == obj.Major && this->Minor == obj.Minor && this->Release == obj.Release)
        return std::strong_ordering::equal;

    if (auto cmp = this->Major <=> obj.Major; cmp != 0) return cmp;
    if (auto cmp = this->Minor <=> obj.Major; cmp != 0) return cmp;
    return this->Release <=> obj.Release;
}
