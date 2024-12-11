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

bool Version::operator==(const Version& obj) const noexcept
{
    return this->Major == obj.Major && this->Minor == obj.Minor && this->Release == obj.Release;
}
bool Version::operator!=(const Version& obj) const noexcept
{  
    return this->Major != obj.Major || this->Minor != obj.Minor || this->Release != obj.Release;
}