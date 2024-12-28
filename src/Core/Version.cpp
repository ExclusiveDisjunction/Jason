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

std::strong_ordering Version::operator<=>(const Version& obj) const noexcept
{
    if (this->Major == obj.Major && this->Minor == obj.Minor && this->Release == obj.Release)
        return std::strong_ordering::equal;

    if (auto cmp = this->Major <=> obj.Major; cmp != 0) return cmp;
    if (auto cmp = this->Minor <=> obj.Major; cmp != 0) return cmp;
    return this->Release <=> obj.Release;
}
