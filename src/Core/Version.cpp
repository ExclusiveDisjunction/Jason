#include "Version.h"

std::ostream& operator<<(std::ostream& out, const Version& obj) noexcept
{
    out << obj.Major << '.' << obj.Minor << '.' << obj.Release;
    return out;
}
std::istream& operator>>(std::istream& in, Version& obj)
{
    char p; //Placeholder for the '.'. This way, the istream will split up the reading between the three parts. 
    in >> obj.Major >> p >> obj.Minor >> p >> obj.Release;
    return in;
}

void Version::dbg_fmt(std::ostream& out) const noexcept 
{
    out << *this;
}

std::strong_ordering Version::operator<=>(const Version& obj) const noexcept
{
    if (this->Major == obj.Major && this->Minor == obj.Minor && this->Release == obj.Release)
        return std::strong_ordering::equal;

    if (auto cmp = this->Major <=> obj.Major; cmp != 0) return cmp;
    if (auto cmp = this->Minor <=> obj.Major; cmp != 0) return cmp;
    return this->Release <=> obj.Release;
}