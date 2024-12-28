#ifndef JASON_VERSION
#define JASON_VERSION

#include <iostream>

#include "Printing.h"
#include "Serialize.h"

class Version : public DebugPrint, public DisplayPrint, public StringSerializable
{
public:
    Version() : Version(0, 0, 0) {}
    Version(unsigned Major, unsigned Minor, unsigned Release) : Major(Major), Minor(Minor), Release(Release) {}

    unsigned Major;
    unsigned Minor;
    unsigned Release;

    void dbg_fmt(std::ostream& out) const noexcept override;
    void dsp_fmt(std::ostream& out) const noexcept override { this->dbg_fmt(out); }
    
    void str_serialize(std::ostream& out) const noexcept override;
    void str_deserialize(std::istream& in) override;

    std::strong_ordering operator<=>(const Version& obj) const noexcept;
    bool operator==(const Version& obj) const noexcept { return this->operator<=>(obj) == 0; }
    bool operator!=(const Version& obj) const noexcept = default;
    bool operator<(const Version& obj) const noexcept = default;
    bool operator<=(const Version& obj) const noexcept = default;
    bool operator>(const Version& obj) const noexcept = default;
    bool operator>=(const Version& obj) const noexcept = default;
};

#define JASON_VERSION_1_0_0 (Version(1, 0, 0))
#define JASON_CURRENT_VERSION JASON_VERSION_1_0_0

#endif
